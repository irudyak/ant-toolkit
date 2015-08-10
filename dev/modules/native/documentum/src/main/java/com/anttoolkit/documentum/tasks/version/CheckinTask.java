package com.anttoolkit.documentum.tasks.version;

import com.anttoolkit.documentum.tasks.object.DocbaseObjectTask;
import org.apache.tools.ant.*;

public class CheckinTask extends DocbaseObjectTask
{
	public void setType(String type)
	{
		throw new BuildException("type attribute is not allowed");
	}

	public void doWork() throws BuildException
	{
		setCheckinOperationFlag();
		super.doWork();
	}
}
