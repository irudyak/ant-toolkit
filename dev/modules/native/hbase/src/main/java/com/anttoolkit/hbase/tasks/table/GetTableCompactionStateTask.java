package com.anttoolkit.hbase.tasks.table;

import org.apache.hadoop.hbase.protobuf.generated.AdminProtos.GetRegionInfoResponse.CompactionState;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class GetTableCompactionStateTask extends GenericHBaseTask
{
	private String table;
	private String property;

	public void setTable(String table)
	{
		this.table = table;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		try
		{
			if (!getHBaseAdmin().tableExists(table))
			{
				throw new BuildException("Table " + table + " doesn't exist");
			}

			CompactionState state = getHBaseAdmin().getCompactionState(table);

			setPropertyThreadSafe(property, state.toString());
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to get " + table + " table compaction state", e);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (table == null || table.trim().isEmpty())
		{
			throw new BuildException("Table name should be specified");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property should be specified");
		}
	}
}
