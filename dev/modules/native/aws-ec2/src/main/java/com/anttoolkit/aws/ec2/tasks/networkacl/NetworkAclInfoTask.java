package com.anttoolkit.aws.ec2.tasks.networkacl;

import com.amazonaws.services.ec2.model.*;

import com.anttoolkit.aws.ec2.tasks.*;

public abstract class NetworkAclInfoTask extends GenericEc2Task
{
	private String aclIdProperty;
	private String vpcIdProperty;
	private String isDefaultProperty;
	private String detailsProperty;

	public void setAclIdProperty(String property)
	{
		aclIdProperty = property;
	}

	public void setVpcIdProperty(String property)
	{
		vpcIdProperty = property;
	}

	public void setIsDefaultProperty(String property)
	{
		isDefaultProperty = property;
	}

	public void setDetailsProperty(String property)
	{
		detailsProperty = property;
	}

	protected void setPropertiesFromNetworkAcl(NetworkAcl acl)
	{
		if (aclIdProperty != null)
		{
			this.setPropertyThreadSafe(aclIdProperty, acl.getNetworkAclId());
		}

		if (vpcIdProperty != null)
		{
			this.setPropertyThreadSafe(vpcIdProperty, acl.getVpcId());
		}

		if (isDefaultProperty != null)
		{
			this.setPropertyThreadSafe(isDefaultProperty, Boolean.TRUE.equals(acl.getIsDefault()) ? "true" : "false");
		}

		if (detailsProperty != null)
		{
			this.setPropertyThreadSafe(detailsProperty, acl.toString());
		}
	}
}
