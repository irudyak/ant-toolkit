package com.anttoolkit.aws.ec2.tasks.vpc;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.tasks.*;

public class CreateVpcTask extends GenericEc2Task
{
	private String name;
	private String vpcIdProperty;
	private Boolean dnsSupport;
	private Boolean dnsHostnames;
	private String dhcpOptionsSetId;

	private CreateVpcRequest request = new CreateVpcRequest();

	public void setName(String name)
	{
		this.name = name;
	}

	public void setCidrBlock(String cidrBlock)
	{
		request.setCidrBlock(cidrBlock);
	}

	public void setTenancy(String tenancy)
	{
		request.setInstanceTenancy(tenancy.toLowerCase().trim());
	}

	public void setVpcIdProperty(String vpcIdProperty)
	{
		this.vpcIdProperty = vpcIdProperty;
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
		CreateVpcResult result = getEc2Client().createVpc(request);

		if (vpcIdProperty != null)
		{
			this.setPropertyThreadSafe(vpcIdProperty, result.getVpc().getVpcId());
		}

		if (name != null)
		{
			createTags(result.getVpc().getVpcId(), new Tag("name", name));
		}

		if (dnsSupport != null || dnsHostnames != null)
		{
			ModifyVpcAttributeRequest modifyAttrRequest = new ModifyVpcAttributeRequest();
			modifyAttrRequest.setVpcId(result.getVpc().getVpcId());

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
			associateRequest.setVpcId(result.getVpc().getVpcId());
			associateRequest.setDhcpOptionsId(dhcpOptionsSetId);

			getEc2Client().associateDhcpOptions(associateRequest);
		}
	}
}
