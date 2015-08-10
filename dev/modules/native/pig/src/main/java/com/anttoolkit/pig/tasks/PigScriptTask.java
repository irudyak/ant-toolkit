package com.anttoolkit.pig.tasks;

import java.io.*;
import java.util.*;

import org.apache.pig.backend.executionengine.*;
import org.apache.pig.tools.pigstats.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class PigScriptTask
		extends GenericPigScriptTask
{
	private String hadoopJobsArray;
	private String jobName;

	public void addText(String script)
	{
		this.setScript(script);
	}

	public void setJobName(String name)
	{
		jobName = name;
	}

	public void setHadoopJobsArray(String array)
	{
		this.hadoopJobsArray = array;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		if (getScript() != null)
		{
			log("Executing Pig script: " + SystemHelper.lineSeparator + getScript());
		}
		else
		{
			log("Executing Pig script file: " + getScriptFile());
		}

		try
		{
			if (jobName != null && !jobName.trim().isEmpty())
			{
				getPigServer().setJobName(jobName);
			}

			List<ExecJob> list = getPigServer().executeBatch();

			if (list == null || hadoopJobsArray == null || hadoopJobsArray.trim().isEmpty())
			{
				return;
			}

			for (ExecJob execJob : list)
			{
				PigStats.JobGraph graph = execJob.getStatistics().getJobGraph();
				for (JobStats stat : graph.getJobList())
				{
					if (!ArrayManager.contains(hadoopJobsArray, stat.getJobId()))
					{
						ArrayManager.add(hadoopJobsArray, stat.getJobId());
					}
				}
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Exception occured during Pig script execution", e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		super.hadoopValidate();

		if ((getScript() == null || getScript().trim().isEmpty()) &&
			(getScriptFile() == null || getScriptFile().trim().isEmpty()))
		{
			throw new BuildException("Either script or script file should be specified");
		}

		if (hadoopJobsArray != null &&
			!hadoopJobsArray.trim().isEmpty() &&
			!ArrayManager.exists(hadoopJobsArray))
		{
			throw new BuildException("Specified array " + hadoopJobsArray + " doesn't exist");
		}
	}
}
