package com.anttoolkit.hadoop.tasks.mapred;

import java.io.*;

import org.apache.hadoop.mapreduce.jobhistory.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public class PrintHistoryTask
		extends GenericHadoopTask
{
	private String jobOutputDir;
	private boolean printAll = false;

	public void setJobOutputDir(String dir)
	{
		jobOutputDir = dir;
	}

	public void setAll(boolean all)
	{
		printAll = all;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			HistoryViewer historyViewer = new HistoryViewer(jobOutputDir, getConfiguration(), printAll);
			historyViewer.print();
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to print job history for job directory: " + jobOutputDir, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (jobOutputDir == null || jobOutputDir.trim().isEmpty())
		{
			throw new BuildException("Job output directory should be specified");
		}
	}
}
