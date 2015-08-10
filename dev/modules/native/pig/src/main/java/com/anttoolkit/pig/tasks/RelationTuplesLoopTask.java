package com.anttoolkit.pig.tasks;

import java.io.*;
import java.util.*;

import com.anttoolkit.pig.tasks.util.ContextManager;
import org.apache.pig.data.*;
import org.apache.tools.ant.*;

public class RelationTuplesLoopTask
		extends GenericPigScriptTask
		implements TaskContainer
{
	private String relation;
	private String tupleSizeProp;
	private String tupleMemSizeProp;
	private String tupleValuesProp;

	private List<Task> tasks = new LinkedList<Task>();

	public void setTupleSizeProperty(String prop)
	{
		tupleSizeProp = prop;
	}

	public void setTupleMemSizeProperty(String prop)
	{
		tupleMemSizeProp = prop;
	}

	public void setTupleValuesProperty(String prop)
	{
		tupleValuesProp = prop;
	}

	public void setRelation(String relation)
	{
		this.relation = relation;
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Iterator<Tuple> iter;

		try
		{
			iter = this.getPigServer().openIterator(relation);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get tuples iterator", e);
		}

		while (iter != null && iter.hasNext())
		{
			Tuple tuple = iter.next();

			if (tupleSizeProp != null)
			{
				setPropertyThreadSafe(tupleSizeProp, Integer.toString(tuple.size()));
			}

			if (tupleMemSizeProp != null)
			{
				setPropertyThreadSafe(tupleMemSizeProp, Long.toString(tuple.getMemorySize()));
			}

			if (tupleValuesProp != null)
			{
				try
				{
					setPropertyThreadSafe(tupleValuesProp, tuple.toDelimitedString(","));
				}
				catch (IOException e)
				{
					throw new BuildException("Failed to convert all tuple values to delimited string", e);
				}
			}

			ContextManager.setCurrentTupleContext(tuple);

			try
			{
				for (Task task : tasks)
				{
					task.perform();
				}
			}
			finally
			{
				ContextManager.resetCurrentTupleContext();
			}
		}
	}

	@Override
	protected void hadoopValidate()
	{

		if (relation == null)
		{
			throw new BuildException("Relation should be specified");
		}
	}
}
