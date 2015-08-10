package com.anttoolkit.hbase.tasks.data;

import com.anttoolkit.hbase.tasks.data.util.BatchManager;
import org.apache.tools.ant.*;

import com.anttoolkit.hbase.tasks.*;

public class DestroyBatchTask extends GenericHBaseTask
{
	private String batchId;

	public void setBatchId(String batchId)
	{
		this.batchId = batchId;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		BatchManager.destroy(batchId);
	}

	@Override
	protected void hadoopValidate()
	{
		if (batchId == null || batchId.trim().isEmpty())
		{
			throw new BuildException("Batch ID should be specified");
		}
	}
}
