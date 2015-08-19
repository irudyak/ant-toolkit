package com.anttoolkit.aws.ec2.tasks.dhcp;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import java.util.*;

public class DescribeDhcpOptionsTask extends DhcpOptionsInfoTask
{
	private DescribeDhcpOptionsRequest request;

	public void setOptionsId(String id)
	{
		request = new DescribeDhcpOptionsRequest();
		request.setDhcpOptionsIds(Arrays.asList(id));
	}

	@Override
	public void doWork() throws BuildException
	{
		DescribeDhcpOptionsResult result = getEc2Client().describeDhcpOptions(request);
		if (result == null || result.getDhcpOptions() == null || result.getDhcpOptions().isEmpty())
		{
			throw new BuildException("DHCP options set '" + request.getDhcpOptionsIds().iterator().next() + "' doesn't exist");
		}

		setPropertiesFromDhcpOptions(result.getDhcpOptions().iterator().next());
	}
}
