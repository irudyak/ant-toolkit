package com.anttoolkit.aws.ec2.tasks.dhcp;

import com.amazonaws.services.ec2.model.*;

import com.anttoolkit.aws.ec2.tasks.*;

public abstract class DhcpOptionsInfoTask extends GenericEc2Task
{
	private String optionsProperty;

	public void setOptionsProperty(String property)
	{
		optionsProperty = property;
	}

	protected void setPropertiesFromDhcpOptions(DhcpOptions opts)
	{
		if (optionsProperty != null)
		{
			this.setPropertyThreadSafe(optionsProperty, opts.toString());
		}
	}
}
