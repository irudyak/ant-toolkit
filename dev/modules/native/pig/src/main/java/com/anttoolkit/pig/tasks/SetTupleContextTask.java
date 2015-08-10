package com.anttoolkit.pig.tasks;

import java.util.*;

import org.apache.pig.backend.executionengine.*;
import org.apache.pig.data.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.pig.tasks.util.*;

public class SetTupleContextTask
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
		Tuple tuple = ContextManager.getCurrentTuple();
		if (tuple == null)
		{
			throw new BuildException(this.getTaskName() + " should be put inside task container which provides tuple to execution context");
		}

		try
		{
			if (tuple.getType(fieldNumber) != DataType.TUPLE||
				!(tuple.get(fieldNumber) instanceof Tuple))
			{
				throw new BuildException("Tuple field " + fieldNumber + " doesn't represent a Tuple");
			}

			ContextManager.setCurrentTupleContext((Tuple) tuple.get(fieldNumber));

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
		catch (ExecException e)
		{
			throw new BuildException("Failed to check type of tuple field " + fieldNumber, e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (fieldNumber == -1)
		{
			throw new BuildException("Field number should be specified");
		}
	}
}
