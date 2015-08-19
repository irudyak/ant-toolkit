package com.anttoolkit.aws.ec2.tasks.dhcp;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.tasks.*;

public class AssociateDhcpOptionsTask extends GenericEc2Task
{
	private String optionsId;
	private String vpcId;

	public void setOptionsId(String id)
	{
		this.optionsId = id;
	}

	public void setVpcId(String id)
	{
		this.vpcId = id;
	}

	@Override
	public void doWork() throws BuildException
	{
		AssociateDhcpOptionsRequest request = new AssociateDhcpOptionsRequest();
		request.setDhcpOptionsId(optionsId);
		request.setVpcId(vpcId);

		getEc2Client().associateDhcpOptions(request);
	}

	@Override
	protected void validate()
	{
		if (optionsId == null || optionsId.trim().isEmpty())
		{
			throw new BuildException("Options id should be specified");
		}

		if (vpcId == null || vpcId.trim().isEmpty())
		{
			throw new BuildException("VPC id should be specified");
		}
	}
}
