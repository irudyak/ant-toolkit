package com.anttoolkit.pig.tasks;

import java.util.*;

import org.apache.pig.backend.executionengine.*;
import org.apache.pig.data.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.pig.tasks.util.*;

public class BagTuplesLoopTask
		extends GenericHadoopTask
		implements TaskContainer
{
	private int fieldNumber = -1;
	private List<Task> tasks = new LinkedList<Task>();

	public void setFieldNumber(int fieldNumber)
	{
		this.fieldNumber = fieldNumber;
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Iterator<Tuple> iter = getBagForProcessing().iterator();

		while (iter.hasNext())
		{
			ContextManager.setCurrentTupleContext(iter.next());

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
		if (fieldNumber == -1 && ContextManager.getCurrentBag() == null)
		{
			throw new BuildException("Either tuple field number should be specified or " +
					getTaskName() + " task should be inside task container which provides " +
					"Bag to execution context");
		}
	}

	private DataBag getBagForProcessing()
	{
		if (fieldNumber == -1)
		{
			DataBag bag = ContextManager.getCurrentBag();
			if (bag == null)
			{
				throw new BuildException(this.getTaskName() + " should be put inside task container which provides Tuple or Bag " +
						"to execution context");
			}

			return bag;
		}

		Tuple tuple = ContextManager.getCurrentTuple();
		if (tuple == null)
		{
			throw new BuildException(this.getTaskName() + " should be put inside task container which provides tuple," +
					" to be able to get Bag from tuple field " + fieldNumber);
		}

		try
		{
			if (tuple.getType(fieldNumber) != DataType.BAG ||
				!(tuple.get(fieldNumber) instanceof DataBag))
			{
				throw new BuildException("Tuple field " + fieldNumber + " doesn't represent a Bag");
			}

			return (DataBag)tuple.get(fieldNumber);

		}
		catch (ExecException e)
		{
			throw new BuildException("Failed to get Bag from tuple field " + fieldNumber, e);
		}
	}
}
