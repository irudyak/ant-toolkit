package com.anttoolkit.pig.tasks;

import org.apache.pig.backend.executionengine.*;
import org.apache.pig.data.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.pig.tasks.util.*;

public class GetTupleFieldTypeTask
		extends GenericHadoopTask
{
	private String property;
	private int fieldNumber = -1;

	public void setFieldNumber(int fieldNumber)
	{
		this.fieldNumber = fieldNumber;
	}

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
			throw new BuildException(this.getTaskName() + " task should be inside appropriate tuples loop task");
		}

		try
		{
			setPropertyThreadSafe(property, dataTypeToString(tuple.getType(fieldNumber)));
		}
		catch (ExecException e)
		{
			throw new BuildException("Failed to get field " + fieldNumber + " type from tuple", e);
		}
	}

	protected void hadoopValidate()
	{
		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property to assign field type should be specified");
		}

		if (fieldNumber == -1)
		{
			throw new BuildException("Field number should be specified");
		}
	}

	private String dataTypeToString(byte type)
	{
		switch (type)
		{
			case DataType.UNKNOWN:
				return "UNKNOWN";
			case DataType.NULL:
				return "NULL";
			case DataType.BOOLEAN:
				return "BOOLEAN";
			case DataType.BYTE:
				return "BYTE";
			case DataType.INTEGER:
				return "INTEGER";
			case DataType.LONG:
				return "LONG";
			case DataType.FLOAT:
				return "FLOAT";
			case DataType.DOUBLE:
				return "DOUBLE";
			case DataType.DATETIME:
				return "DATETIME";
			case DataType.BYTEARRAY:
				return "BYTEARRAY";
			case DataType.CHARARRAY:
				return "CHARARRAY";
			case DataType.BIGINTEGER:
				return "BIGINTEGER";
			case DataType.BIGDECIMAL:
				return "BIGDECIMAL";
			case DataType.BIGCHARARRAY:
				return "BIGCHARARRAY";
			case DataType.MAP:
				return "MAP";
			case DataType.TUPLE:
				return "TUPLE";
			case DataType.BAG:
				return "BAG";
			case DataType.GENERIC_WRITABLECOMPARABLE:
				return "GENERIC_WRITABLECOMPARABLE";
			case DataType.INTERNALMAP:
				return "INTERNALMAP";
			case DataType.ERROR:
				return "ERROR";
		}

		throw new BuildException("Unknown tuple field data type: " + type);
	}
}
