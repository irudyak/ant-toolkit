package com.anttoolkit.aws.ec2.tasks.vpc;

import com.amazonaws.services.ec2.model.*;

import com.anttoolkit.aws.ec2.tasks.*;

import org.apache.tools.ant.*;

public class ModifyVpcTask extends GenericEc2Task
{
	private String vpcId;
	private String name;
	private Boolean dnsSupport;
	private Boolean dnsHostnames;
	private String dhcpOptionsSetId;

	public void setName(String name)
	{
		this.name = name;
	}

	public void setVpcId(String id)
	{
		vpcId = id;
	}

	public void setDnsSupport(boolean enable)
	{
		dnsSupport = enable;
	}

	public void setDnsHostnames(boolean enable)
	{
		dnsHostnames = enable;
	}

	public void setDhcpOptionsSetId(String id)
	{
		dhcpOptionsSetId = id;
	}

	@Override
	public void doWork() throws BuildException
	{
		if (name != null)
		{
			createTags(vpcId, new Tag("name", name));
		}

		if (dnsSupport != null || dnsHostnames != null)
		{
			ModifyVpcAttributeRequest modifyAttrRequest = new ModifyVpcAttributeRequest();
			modifyAttrRequest.setVpcId(vpcId);

			if (dnsSupport != null)
			{
				modifyAttrRequest.setEnableDnsSupport(dnsSupport);
			}

			if (dnsHostnames != null)
			{
				modifyAttrRequest.setEnableDnsHostnames(dnsHostnames);
			}

			getEc2Client().modifyVpcAttribute(modifyAttrRequest);
		}

		if (dhcpOptionsSetId != null)
		{
			AssociateDhcpOptionsRequest associateRequest = new AssociateDhcpOptionsRequest();
			associateRequest.setVpcId(vpcId);
			associateRequest.setDhcpOptionsId(dhcpOptionsSetId);

			getEc2Client().associateDhcpOptions(associateRequest);
		}
	}

	@Override
	protected void validate()
	{
		if (vpcId == null || vpcId.trim().isEmpty())
		{
			throw new BuildException("VPC id should be specified");
		}
	}
}
