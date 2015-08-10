package com.anttoolkit.hadoop.tasks.yarn.util;

import java.io.*;
import java.util.*;

import org.apache.commons.lang.*;
import org.apache.hadoop.conf.*;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.yarn.api.records.*;
import org.apache.hadoop.yarn.conf.*;
import org.apache.hadoop.yarn.logaggregation.*;
import org.apache.hadoop.yarn.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;

public class LogsHelper
{
	private Configuration conf;
	private String logsFile;
	private String logsDir;

	private PrintStream out;

	public LogsHelper(Configuration conf, String logsFile, String logsDir)
	{
		this.conf = conf;
	}

	public void dumpAContainersLogs(String appId, String containerId,
								   String nodeId, String jobOwner)
			throws IOException
	{
		dumpAContainersLogsForALogType(appId, containerId, nodeId, jobOwner, null);
	}

	public void dumpAContainersLogsForALogType(String appId, String containerId,
											  String nodeId, String jobOwner, List<String> logType)
	{
		boolean foundContainerLogs = false;
		Path remoteAppLogDir = null;

		initContainerLogOutput(containerId);

		try
		{
			String containerString = SystemHelper.doubleLineSeparator + "Container: " + containerId;

			out.println(StringUtils.repeat("=", containerString.length()));
			out.println(containerString);
			out.println(StringUtils.repeat("=", containerString.length()));

			Path remoteRootLogDir = new Path(conf.get(YarnConfiguration.NM_REMOTE_APP_LOG_DIR, YarnConfiguration.DEFAULT_NM_REMOTE_APP_LOG_DIR));

			String suffix = LogAggregationUtils.getRemoteNodeLogDirSuffix(conf);
			remoteAppLogDir = LogAggregationUtils.getRemoteAppLogDir(remoteRootLogDir, ConverterUtils.toApplicationId(appId), jobOwner, suffix);

			RemoteIterator<FileStatus> nodeFiles;
			try
			{
				Path qualifiedLogDir = FileContext.getFileContext(conf).makeQualified(remoteAppLogDir);
				nodeFiles = FileContext.getFileContext(qualifiedLogDir.toUri(), conf).listStatus(remoteAppLogDir);
			}
			catch (FileNotFoundException e)
			{
				throw new BuildException("Remote directory \"" + remoteAppLogDir.toString() + "\" doesn't exist");
			}

			while (nodeFiles.hasNext())
			{
				FileStatus thisNodeFile = nodeFiles.next();
				String fileName = thisNodeFile.getPath().getName();

				if (!fileName.contains(LogAggregationUtils.getNodeString(nodeId)) ||
						!fileName.endsWith(LogAggregationUtils.TMP_FILE_SUFFIX))
				{
					continue;
				}

				AggregatedLogFormat.LogReader reader = null;
				try
				{
					reader = new AggregatedLogFormat.LogReader(conf, thisNodeFile.getPath());
					if (logType == null)
					{
						if (dumpAContainerLogs(containerId, reader, thisNodeFile.getModificationTime()) > -1)
						{
							foundContainerLogs = true;
						}
					}
					else
					{
						if (dumpAContainerLogsForALogType(containerId, reader, thisNodeFile.getModificationTime(), logType) > -1)
						{
							foundContainerLogs = true;
						}
					}
				}
				finally
				{
					if (reader != null)
					{
						reader.close();
					}
				}
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to dump YARN logs for container: " + containerId, e);
		}
		finally
		{
			close();
		}

		if (!foundContainerLogs)
		{
			throw new BuildException("Logs for container " + containerId + " are not present in logs dir: " + remoteAppLogDir.toString());
		}
	}

	public void dumpAllContainersLogs(ApplicationId appId, String appOwner)
			throws IOException
	{
		Path remoteRootLogDir = new Path(conf.get(YarnConfiguration.NM_REMOTE_APP_LOG_DIR, YarnConfiguration.DEFAULT_NM_REMOTE_APP_LOG_DIR));
		String logDirSuffix = LogAggregationUtils.getRemoteNodeLogDirSuffix(conf);

		// TODO Change this to get a list of files from the LAS.
		Path remoteAppLogDir = LogAggregationUtils.getRemoteAppLogDir(remoteRootLogDir, appId, appOwner, logDirSuffix);
		RemoteIterator<FileStatus> nodeFiles;
		try
		{
			Path qualifiedLogDir = FileContext.getFileContext(conf).makeQualified(remoteAppLogDir);
			nodeFiles = FileContext.getFileContext(qualifiedLogDir.toUri(), conf).listStatus(remoteAppLogDir);
		}
		catch (FileNotFoundException e)
		{
			throw new BuildException("Remote directory \"" + remoteAppLogDir.toString() + "\" doesn't exist");
		}

		boolean foundAnyLogs = false;

		while (nodeFiles.hasNext())
		{
			FileStatus thisNodeFile = nodeFiles.next();
			if (thisNodeFile.getPath().getName().endsWith(LogAggregationUtils.TMP_FILE_SUFFIX))
			{
				continue;
			}

			AggregatedLogFormat.LogReader reader = new AggregatedLogFormat.LogReader(conf, thisNodeFile.getPath());

			try
			{
				DataInputStream valueStream;
				AggregatedLogFormat.LogKey key = new AggregatedLogFormat.LogKey();
				valueStream = reader.next(key);

				initContainerLogOutput(key.toString());

				while (valueStream != null)
				{
					String containerString = SystemHelper.doubleLineSeparator + "Container: " + key + " on " + thisNodeFile.getPath().getName();

					out.println(StringUtils.repeat("=", containerString.length()));
					out.println(containerString);
					out.println(StringUtils.repeat("=", containerString.length()));

					while (true)
					{
						try
						{
							AggregatedLogFormat.LogReader.readAContainerLogsForALogType(valueStream, out, thisNodeFile.getModificationTime());
							foundAnyLogs = true;
						}
						catch (EOFException eof)
						{
							break;
						}
					}

					// Next container
					key = new AggregatedLogFormat.LogKey();
					valueStream = reader.next(key);
				}
			}
			finally
			{
				reader.close();
				close();
			}
		}

		if (!foundAnyLogs)
		{
			throw new BuildException("Application remote log dir \"" + remoteAppLogDir.toString() + "\" doesn't have any log files");
		}
	}

	private int dumpAContainerLogs(String containerIdStr,
								  AggregatedLogFormat.LogReader reader,
								  long logUploadedTime)
			throws IOException
	{
		DataInputStream valueStream;
		AggregatedLogFormat.LogKey key = new AggregatedLogFormat.LogKey();
		valueStream = reader.next(key);

		while (valueStream != null && !key.toString().equals(containerIdStr))
		{
			// Next container
			key = new AggregatedLogFormat.LogKey();
			valueStream = reader.next(key);
		}

		if (valueStream == null)
		{
			return -1;
		}

		boolean foundContainerLogs = false;
		while (true)
		{
			try
			{
				AggregatedLogFormat.LogReader.readAContainerLogsForALogType(valueStream, out, logUploadedTime);
				foundContainerLogs = true;
			}
			catch (EOFException e)
			{
				break;
			}
		}

		return foundContainerLogs ? 0 : -1;
	}

	private int dumpAContainerLogsForALogType(String containerIdStr,
											 AggregatedLogFormat.LogReader reader,
											 long logUploadedTime, List<String> logType)
			throws IOException
	{
		DataInputStream valueStream;
		AggregatedLogFormat.LogKey key = new AggregatedLogFormat.LogKey();
		valueStream = reader.next(key);

		while (valueStream != null && !key.toString().equals(containerIdStr))
		{
			// Next container
			key = new AggregatedLogFormat.LogKey();
			valueStream = reader.next(key);
		}

		if (valueStream == null)
		{
			return -1;
		}

		boolean foundContainerLogs = false;
		while (true)
		{
			try
			{
				int result = readContainerLogsForALogType(valueStream, logUploadedTime, logType);
				if (result == 0)
				{
					foundContainerLogs = true;
				}
			}
			catch (EOFException e)
			{
				break;
			}
		}

		if (foundContainerLogs)
		{
			return 0;
		}

		return -1;
	}

	private int readContainerLogsForALogType(DataInputStream valueStream, long logUploadedTime, List<String> logType)
			throws IOException
	{
		byte[] buf = new byte[65535];

		String fileType = valueStream.readUTF();
		String fileLengthStr = valueStream.readUTF();
		long fileLength = Long.parseLong(fileLengthStr);

		if (!logType.contains(fileType))
		{
			long totalSkipped = 0;
			long currSkipped = 0;

			while (currSkipped != -1 && totalSkipped < fileLength)
			{
				currSkipped = valueStream.skip(fileLength - totalSkipped);
				totalSkipped += currSkipped;
			}

			return -1;
		}

		out.print("LogType:");
		out.println(fileType);

		if (logUploadedTime != -1)
		{
			out.print("Log Upload Time:");
			out.println(Times.format(logUploadedTime));
		}

		out.print("LogLength:");
		out.println(fileLengthStr);
		out.println("Log Contents:");

		long curRead = 0;
		long pendingRead = fileLength - curRead;
		int toRead = pendingRead > buf.length ? buf.length : (int) pendingRead;
		int len = valueStream.read(buf, 0, toRead);

		while (len != -1 && curRead < fileLength)
		{
			out.write(buf, 0, len);
			curRead += len;

			pendingRead = fileLength - curRead;
			toRead = pendingRead > buf.length ? buf.length : (int) pendingRead;
			len = valueStream.read(buf, 0, toRead);
		}

		out.println("End of LogType:" + fileType);
		out.println("");
		return 0;
	}

	private void initContainerLogOutput(String containerId)
	{
		close();

		String file = logsFile != null ? logsFile :
				logsDir.trim().replace("\\", "/").endsWith("/") ?
						logsDir + containerId + ".log" :
						logsDir + "/" + containerId + ".log";

		File logFile = new File(file);
		if (logFile.exists() && logFile.isDirectory())
		{
			throw new BuildException("Can't save YARN log content to file '" + file + "' cause directory with the same name already exists");
		}

		try
		{
			if (!logFile.createNewFile())
			{
				throw new BuildException("Failed to create new file '" + file + "' to write YARN log into it");
			}
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create new file '" + file + "' to write YARN log into it", e);
		}

		try
		{
			out = new PrintStream(new FileOutputStream(file, true));
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to get output stream for log file '" + file + "'", e);
		}
	}

	private void close()
	{
		if (out == null)
		{
			return;
		}

		try
		{
			out.close();
		}
		catch (Throwable e)
		{
		}

		out = null;
	}

	protected void finalize() throws Throwable
	{
		super.finalize();
		close();
	}
}
