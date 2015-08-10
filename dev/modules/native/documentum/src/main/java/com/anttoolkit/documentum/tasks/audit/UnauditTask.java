package com.anttoolkit.documentum.tasks.audit;

import org.apache.tools.ant.*;

public class UnauditTask
		extends GenericAuditTask
{
	public void doWork()
			throws BuildException
	{
		String policyId = this.getPolicyId();
		String[] types = this.getTypeNames();
		String[] events = this.getEventNames();

		for (String type : types)
		{
			for (String event : events)
			{
				this.unregisterEventForType(type.trim(), event.trim(), this.getApplication(), policyId, this.getState());
			}
		}
	}
}
