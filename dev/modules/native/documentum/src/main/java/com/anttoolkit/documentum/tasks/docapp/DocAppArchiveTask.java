package com.anttoolkit.documentum.tasks.docapp;

import com.anttoolkit.documentum.common.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.taskdefs.*;

import com.documentum.fc.common.*;
import com.documentum.ApplicationManager.*;

import java.io.*;

@Deprecated
public class DocAppArchiveTask
		extends GenericDocbaseTask
{
	private String docAppName = null;
	private String zipFile = null;
	private String folder = null;

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

	public void doWork()
			throws BuildException
	{
		try
		{
			//calculate paths
			String fullZipFileName = null;
			String docAppDirectory;
			String docAppParentDirectory;

			if (zipFile != null)
			{
				fullZipFileName = getFileFullPath(zipFile);
				int index = fullZipFileName.lastIndexOf(File.separatorChar);
				docAppParentDirectory = fullZipFileName.substring(0, index);
				docAppDirectory = docAppParentDirectory + File.separatorChar + docAppName;
			}
			else
			{
				docAppParentDirectory = folder;
				docAppDirectory = folder + File.separatorChar + docAppName;
			}

			//delete DocApp old local directory if exists
			Delete deleteTask = new Delete();
			this.initDelegateTask(deleteTask);
			deleteTask.setDir(new File(docAppDirectory));
			deleteTask.perform();

			//create DocApp archive into the local directory
			IDfApplication application = DfAppUtils.findFromApplicationName(this.getSession().getDfSession(), docAppName);
			application.makeExternalApplication(docAppParentDirectory);

			if (zipFile == null)
			{
				return;
			}

			//create zip archive
			Zip zipTask = new Zip();
			this.initDelegateTask(zipTask);
			zipTask.setBasedir(new File(docAppDirectory));
			zipTask.setDestFile(new File(fullZipFileName));
			zipTask.setUpdate(true);
			zipTask.perform();

			//delete DocApp local directory
			deleteTask.perform();
		}
		catch (DfException e)
		{
			throw new BuildException("Documentum server exception occured", e);
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
