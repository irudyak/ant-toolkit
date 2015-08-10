package com.anttoolkit.documentum.tasks.docapp;

import com.anttoolkit.documentum.common.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.taskdefs.*;

import com.documentum.ApplicationInstall.*;
import com.documentum.fc.common.*;

import java.io.*;

@Deprecated
public class DocAppInstallTask
		extends GenericDocbaseTask
{
	private static final String LOG_FILE_SUFFIX = "_installerLog.html";

	private String docAppName = null;
	private String zipFile = null;
	private String folder = null;
	private String logFile = null;

	public void setDocAppName(String name)
	{
		docAppName = name;
	}

	public void setZipFile(String file)
	{
		zipFile = file;
	}

	public void setFolder(String folder)
	{
		this.folder = folder;
	}

	public void setLogFile(String file)
	{
		logFile = file;
	}

	public void doWork()
			throws BuildException
	{
		//calculate paths
		String fullLogFileName = getFileFullPath(logFile == null ? docAppName : logFile);
		String docAppDirectory = folder;

		Delete deleteTask = new Delete();
		if (zipFile != null)
		{
			String fullZipFileName = getFileFullPath(zipFile);
			int index = fullZipFileName.lastIndexOf(File.separatorChar);
			String fullZipFilePath = fullZipFileName.substring(0, index);
			docAppDirectory = fullZipFilePath + File.separatorChar + docAppName;

			//delete DocApp old local directory if exists
			this.initDelegateTask(deleteTask);
			deleteTask.setDir(new File(docAppDirectory));
			deleteTask.perform();

			//create new local directory
			Mkdir mkdirTask = new Mkdir();
			this.initDelegateTask(mkdirTask);
			mkdirTask.setDir(new File(docAppDirectory));
			mkdirTask.perform();

			//unzip archive to local directory
			Expand expandTask = new Expand();
			this.initDelegateTask(expandTask);
			expandTask.setSrc(new File(fullZipFileName));
			expandTask.setDest(new File(docAppDirectory));
			expandTask.setOverwrite(true);
			expandTask.perform();
		}

		LoginInfo loginInfo = DocbaseSessionManager.getCurrentSessionContext();

		//install DocApp to repository
		DfAppInstaller appInstaller = new DfAppInstaller();
		String propertyFile = null;
		String args[] = {"-d", loginInfo.getDocbase(), "-n", loginInfo.getLogin(), "-p", loginInfo.getPassword(),
			"-m", loginInfo.getDomain(), "-a", docAppDirectory, "-l", fullLogFileName,
			"-f", propertyFile};

		DfAppInstallerCommandLine commandLineAppInstaller = new DfAppInstallerCommandLine(args);
		if(commandLineAppInstaller.processArgumentList())
		{
			appInstaller.connectionInfo(loginInfo.getDocbase(), loginInfo.getDomain(), loginInfo.getLogin(), loginInfo.getPassword());
			appInstaller.logFile(commandLineAppInstaller.getLogFileName(), commandLineAppInstaller.getLogFileLocation());
			appInstaller.appFile(commandLineAppInstaller.getApplicationFileName());
			appInstaller.setPropertiesFile(commandLineAppInstaller.getPropertiesFile());

			try
			{
				appInstaller.startInstall(false);
			}
			catch (DfException e)
			{
				throw new BuildException("Documentum server exception occured", e);
			}
		}
		else
		{
			throw new BuildException(DfAppInstaller.getUsageString());
		}

		//rename log file
		if (logFile != null)
		{
			Move moveTask = new Move();
			this.initDelegateTask(moveTask);
			moveTask.setFile(new File(fullLogFileName + LOG_FILE_SUFFIX));
			moveTask.setTofile(new File(fullLogFileName));
			moveTask.perform();
		}

		//delete local directory
		if (zipFile != null)
		{
			deleteTask.perform();
		}
	}

	protected void validate()
	{
		if (docAppName == null)
		{
			throw new BuildException("docAppName is mandatory attribute");
		}

		if (zipFile == null && folder == null)
		{
			throw new BuildException("zipFile or folder attribute should be specified");
		}

		if (zipFile != null && folder != null)
		{
			throw new BuildException("zipFile and folder attributes couldn't be specified " +
					"simultaneously, only one of them should be specified");
		}
	}
}
