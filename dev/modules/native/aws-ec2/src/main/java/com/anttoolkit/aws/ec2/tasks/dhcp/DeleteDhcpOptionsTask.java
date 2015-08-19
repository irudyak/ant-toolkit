package com.anttoolkit.aws.ec2.tasks.dhcp;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.tasks.*;

public class DeleteDhcpOptionsTask extends GenericEc2Task
{
	private DeleteDhcpOptionsRequest request;

	public void setOptionsId(String id)
	{
		request = new DeleteDhcpOptionsRequest(id);
	}

	@Override
	public void doWork() throws BuildException
	{
		getEc2Client().deleteDhcpOptions(request);
	}

	@Override
	protected void validate()
	{
		if (request == null)
		{
			throw new BuildException("Options id should be specified");
		}
	}
}
