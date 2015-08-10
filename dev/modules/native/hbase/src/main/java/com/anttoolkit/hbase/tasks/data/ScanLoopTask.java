package com.anttoolkit.hbase.tasks.data;

import java.util.*;

import org.apache.hadoop.hbase.client.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.general.refs.*;
import com.anttoolkit.hbase.tasks.data.util.scan.*;

public class ScanLoopTask
		extends GenericHBaseTask
		implements TaskContainer
{
	private String table;
	private ScanAction action;
	private String reference;
	private List<Task> tasks = new LinkedList<Task>();

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setReference(String reference)
	{
		this.reference = reference;
	}

	public void setScan(String ref)
	{
		Object obj = this.getReference(ref);
		if (!(obj instanceof ScanAction))
		{
			throw new IllegalArgumentException("Incorrect SCAN configuration specified");
		}

		this.action = (ScanAction)obj;
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		ResultScanner scanner = action.getScanner(getTable(table), this);

		try
		{
			for (Result result : scanner)
			{
				if (reference != null)
				{
					this.setReference(reference, result);
				}

				action.processResult(result, table, this);

				for (Task task : tasks)
				{
					task.perform();
				}
			}
		}
		finally
		{
			if (scanner != null)
			{
				try
				{
					scanner.close();
				}
				catch (Throwable e) {}
			}

			if (reference != null)
			{
				this.removeReference(reference);
			}
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (action == null)
		{
			throw new BuildException("No scan object was specified, to loop through SCAN operation results");
		}

		if (table == null)
		{
			throw new BuildException("No table specified for SCAN operation: " + toString());
		}

		action.validate(this);
	}
}
