package com.anttoolkit.general.tasks.build.csv;

import java.io.*;
import java.text.*;
import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.build.util.*;

public class CsvVersionProvider extends GenericVersionProvider
{
	//file to store updates
	@VersionProviderParam("file")
	protected String csvFile;

	//time format to use
	@VersionProviderParam("timeFormat")
	protected String timeFormat = "MM/dd/yyyy hh:mm:ss";

	private List<VersionRecord> versionRecords = new LinkedList<VersionRecord>();
	private Map<String, List<VersionHistoryRecord>> versionHistoryRecords = new HashMap<String, List<VersionHistoryRecord>>();

	private boolean initialized = false;

	@Override
	public BuildVersion getCurrentVersion(String qualifier)
	{
		init();

		for (VersionRecord record : versionRecords)
		{
			if (record.qualifier.equals(qualifier))
			{
				return record.version;
			}
		}

		return BuildVersion.INITIAL;
	}

	@Override
	public boolean canUpdateToVersion(BuildVersion version, String qualifier)
	{
		init();

		List<VersionHistoryRecord> historyRecords = versionHistoryRecords.get(qualifier);
		if (historyRecords == null)
		{
			return true;
		}

		for (VersionHistoryRecord record : historyRecords)
		{
			if (record.newVersion.equals(version))
			{
				return false;
			}
		}

		return true;
	}

	@Override
	public void updateToVersion(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier)
	{
		init();

		boolean outOfTurnUpdate = newVersion.compareTo(currentVersion) < 0;

		if (!outOfTurnUpdate)
		{
			updateVersionRecord(currentVersion, newVersion, comment, qualifier);
		}

		createHistoryRecord(currentVersion, newVersion, comment, qualifier);

		persist();
	}

	@Override
	public void validate(GenericTask task)
	{
		if (csvFile == null || csvFile.trim().isEmpty())
		{
			throw new BuildException("No CSV file specified for CSV version provider!!!");
		}

		csvFile = task.getFileFullPath(csvFile);
	}

	private void updateVersionRecord(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier)
	{
		DateFormat formatter = new SimpleDateFormat(timeFormat);
		VersionRecord newRecord = new VersionRecord(newVersion, qualifier, formatter.format(new Date()),
				comment, SystemHelper.osUser, SystemHelper.hostName, SystemHelper.hostIp);

		for (int i = 0; i < versionRecords.size(); i++)
		{
			VersionRecord record = versionRecords.get(i);

			if (record.qualifier.equals(qualifier) && record.version.equals(currentVersion))
			{
				versionRecords.set(i, newRecord);
				return;
			}
		}

		versionRecords.add(newRecord);
	}

	private void createHistoryRecord(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier)
	{
		DateFormat formatter = new SimpleDateFormat(timeFormat);
		VersionHistoryRecord newRecord = new VersionHistoryRecord(currentVersion, newVersion, qualifier,
				formatter.format(new Date()), comment, SystemHelper.osUser, SystemHelper.hostName, SystemHelper.hostIp);

		if (!versionHistoryRecords.containsKey(qualifier))
		{
			versionHistoryRecords.put(qualifier, new LinkedList<VersionHistoryRecord>());
		}

		versionHistoryRecords.get(qualifier).add(newRecord);
	}

	private void init()
	{
		if (initialized)
		{
			return;
		}

		File file = new File(csvFile);
		if (file.exists() && file.isDirectory())
		{
			throw new BuildException("CSV version provider can't use '" + csvFile + "' file, cause directory with the same name already exists");
		}

		if (!file.exists())
		{
			try
			{
				if (!file.createNewFile())
				{
					throw new BuildException("Failed to create new file '" + csvFile + "' for CSV version provider");
				}
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to create new file '" + csvFile + "' for CSV version provider");
			}
		}

		InputStream in = null;
		BufferedReader reader = null;

		try
		{
			in = new FileInputStream(file);
			reader = new BufferedReader(new InputStreamReader(in, "UTF-8"));

			String line;

			while ((line = reader.readLine()) != null)
			{
				if (line.trim().startsWith("#") || line.trim().isEmpty())
				{
					continue;
				}

				if (VersionRecord.isVersionRecord(line))
				{
					versionRecords.add(new VersionRecord(line));
				}
				else if (VersionHistoryRecord.isVersionHistoryRecord(line))
				{
					VersionHistoryRecord historyRecord = new VersionHistoryRecord(line);

					if (!versionHistoryRecords.containsKey(historyRecord.qualifier))
					{
						versionHistoryRecords.put(historyRecord.qualifier, new LinkedList<VersionHistoryRecord>());
					}

					versionHistoryRecords.get(historyRecord.qualifier).add(historyRecord);
				}
				else
				{
					throw new BuildException("Incorrect format of CSV file '" + csvFile + "' provided to CSV version provider!!!");
				}
    		}

			Collections.sort(versionRecords);
		}
		catch (FileNotFoundException e)
		{
			throw new BuildException("Failed to open file " + csvFile, e);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed read file " + csvFile, e);
		}
		finally
		{
			if (reader != null)
			{
				try
				{
					reader.close();
				}
				catch (Throwable ex) {}
			}

			if (in != null)
			{
				try
				{
					in.close();
				}
				catch (Throwable ex) {}
			}
		}

		initialized = true;
	}

	private void persist()
	{
		Collections.sort(versionRecords);

		File file = new File(csvFile);

		try
		{
			if (!file.exists() && !file.createNewFile())
			{
				throw new BuildException("Failed to create new file '" + csvFile + "' for CSV version provider");
			}
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to create new file '" + csvFile + "' for CSV version provider");
		}

		OutputStream out = null;
		BufferedWriter writer = null;

		try
		{
			out = new FileOutputStream(file, false);
			writer = new BufferedWriter(new OutputStreamWriter(out, "UTF-8"));

			persistVersionRecords(writer);
			persistVersionHistoryRecords(writer);

			writer.flush();
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to write builds info into file: " + csvFile, e);
		}
		finally
		{
			if (writer != null)
			{
				try
				{
					writer.close();
				}
				catch (IOException e) {}
			}

			if (out != null)
			{
				try
				{
					out.close();
				}
				catch (IOException e) {}
			}
		}
	}

	private void persistVersionRecords(BufferedWriter writer)
			throws IOException
	{
		writer.write("### VERSION RECORDS ###");

		for (VersionRecord record : versionRecords)
		{
			writer.newLine();
			writer.write(record.toString());
		}
	}

	private void persistVersionHistoryRecords(BufferedWriter writer)
			throws IOException
	{
		writer.newLine();
		writer.write("### HISTORY RECORDS ###");

		int i = 0;

		for (VersionRecord record : versionRecords)
		{
			if (i != 0)
			{
				writer.newLine();
				writer.write("################################################");
			}

			List<VersionHistoryRecord> historyRecords = versionHistoryRecords.get(record.qualifier);
			for (VersionHistoryRecord histRecord : historyRecords)
			{
				writer.newLine();
				writer.write(histRecord.toString());
			}

			i++;
		}
	}
}
