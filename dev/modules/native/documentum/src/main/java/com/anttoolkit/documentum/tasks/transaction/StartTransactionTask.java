package com.anttoolkit.documentum.tasks.transaction;

import com.anttoolkit.documentum.common.*;

import org.apache.tools.ant.*;

public class StartTransactionTask
		extends GenericDocbaseTask
{
	public void doWork()
			throws BuildException
	{
		if (this.getSession().isTransactionActive())
		{
			throw new BuildException("Transaction is already started");
		}

		this.getSession().startTransaction();
	}
}
