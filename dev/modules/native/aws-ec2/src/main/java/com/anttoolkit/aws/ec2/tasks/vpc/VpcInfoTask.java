package com.anttoolkit.aws.ec2.tasks.vpc;

import com.amazonaws.services.ec2.model.*;

import com.anttoolkit.aws.ec2.tasks.*;

import com.anttoolkit.general.common.*;

public abstract class VpcInfoTask extends GenericEc2Task
{
	private String vpcIdProperty;
	private String stateProperty;
	private String cidrBlockProperty;
	private String dhcpOptionsIdProperty;
	private String dnsSupportProperty;
	private String dnsHostnamesProperty;
	private String tagsProperty;
	private String instanceTenancyProperty;
	private String isDefaultProperty;

	public void setVpcIdProperty(String property)
	{
		this.vpcIdProperty = property;
	}

	public void setStateProperty(String property)
	{
		this.stateProperty = property;
	}

	public void setCidrBlockProperty(String property)
	{
		this.cidrBlockProperty = property;
	}

	public void setDhcpOptionsIdProperty(String property)
	{
		this.dhcpOptionsIdProperty = property;
	}

	public void setDnsSupportProperty(String property)
	{
		this.dnsSupportProperty = property;
	}

	public void setDnsHostnamesProperty(String property)
	{
		this.dnsHostnamesProperty = property;
	}

	public void setTagsProperty(String property)
	{
		this.tagsProperty = property;
	}

	public void setInstanceTenancyProperty(String property)
	{
		this.instanceTenancyProperty = property;
	}

	public void setIsDefaultProperty(String property)
	{
		this.isDefaultProperty = property;
	}

	protected void setPropertiesFromVpc(Vpc vpc)
	{
		if (vpcIdProperty != null)
		{
			this.setPropertyThreadSafe(vpcIdProperty, vpc.getVpcId());
		}

		if (stateProperty != null)
		{
			this.setPropertyThreadSafe(stateProperty, vpc.getState());
		}

		if (cidrBlockProperty != null)
		{
			this.setPropertyThreadSafe(cidrBlockProperty, vpc.getCidrBlock());
		}

		if (dhcpOptionsIdProperty != null)
		{
			this.setPropertyThreadSafe(dhcpOptionsIdProperty, vpc.getDhcpOptionsId());
		}

		if (tagsProperty != null)
		{
			this.setPropertyThreadSafe(tagsProperty, CollectionsHelper.toString(vpc.getTags()));
		}

		if (instanceTenancyProperty != null)
		{
			this.setPropertyThreadSafe(instanceTenancyProperty, vpc.getInstanceTenancy());
		}

		if (isDefaultProperty != null)
		{
			this.setPropertyThreadSafe(isDefaultProperty, Boolean.toString(vpc.isDefault()));
		}

		if (dnsSupportProperty != null)
		{
			DescribeVpcAttributeRequest request = new DescribeVpcAttributeRequest();
			request.setVpcId(vpc.getVpcId());
			request.setAttribute("enableDnsSupport");

			DescribeVpcAttributeResult result = getEc2Client().describeVpcAttribute(request);

			this.setPropertyThreadSafe(dnsSupportProperty, Boolean.toString(result.getEnableDnsSupport()));
		}

		if (dnsHostnamesProperty != null)
		{
			DescribeVpcAttributeRequest request = new DescribeVpcAttributeRequest();
			request.setVpcId(vpc.getVpcId());
			request.setAttribute("enableDnsHostnames");

			DescribeVpcAttributeResult result = getEc2Client().describeVpcAttribute(request);

			this.setPropertyThreadSafe(dnsHostnamesProperty, Boolean.toString(result.getEnableDnsHostnames()));
		}
	}
}
