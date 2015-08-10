package com.anttoolkit.pig.tasks;

import org.apache.pig.data.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.pig.tasks.util.*;

public class GetTupleFieldsCountTask
		extends GenericHadoopTask
{
	private String property;

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Tuple tuple = ContextManager.getCurrentTuple();
		if (tuple == null)
		{
			throw new BuildException(this.getTaskName() + " should be put inside task container which provides tuple to execution context");
		}

		setPropertyThreadSafe(property, Integer.toString(tuple.size()));
	}

	@Override
	protected void hadoopValidate()
	{
		if (property == null)
		{
			throw new BuildException("Property to store tuple fields count should be specified");
		}
	}
}
