package com.anttoolkit.documentum.tasks.transaction;

import com.anttoolkit.documentum.common.*;

import org.apache.tools.ant.*;

public class CommitTransactionTask
		extends GenericDocbaseTask
{
	public void doWork()
			throws BuildException
	{
		if (!this.getSession().isDfSessionOpened())
		{
			return;
		}

		if (!this.getSession().isTransactionActive())
		{
			throw new BuildException("There are no transactions to commit");
		}

		this.getSession().commitTransaction();
	}
}
