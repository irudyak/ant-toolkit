package com.anttoolkit.aws.ec2.tasks.securitygroup;

import com.amazonaws.services.ec2.model.*;

import com.anttoolkit.aws.ec2.tasks.*;
import com.anttoolkit.general.common.*;

public abstract class SecurityGroupInfoTask extends GenericEc2Task
{
	private String ownerIdProperty;
	private String groupNameProperty;
	private String groupIdProperty;
	private String descriptionProperty;
	private String inboundPermProperty;
	private String outboundPermProperty;
	private String vpcIdProperty;
	private String tagsProperty;

	public void setOwnerIdProperty(String property)
	{
		ownerIdProperty = property;
	}

	public void setGroupNameProperty(String property)
	{
		groupNameProperty = property;
	}

	public void setGroupIdProperty(String property)
	{
		groupIdProperty = property;
	}

	public void setDescriptionProperty(String property)
	{
		descriptionProperty = property;
	}

	public void setInboundPermProperty(String property)
	{
		inboundPermProperty = property;
	}

	public void setOutboundPermProperty(String property)
	{
		outboundPermProperty = property;
	}

	public void setVpcIdProperty(String property)
	{
		vpcIdProperty = property;
	}

	public void setTagsProperty(String property)
	{
		tagsProperty = property;
	}

	protected void setPropertiesFromGroup(SecurityGroup group)
	{
		if (ownerIdProperty != null)
		{
			this.setPropertyThreadSafe(ownerIdProperty, group.getOwnerId());
		}

		if (groupNameProperty != null)
		{
			this.setPropertyThreadSafe(groupNameProperty, group.getGroupName());
		}

		if (groupIdProperty != null)
		{
			this.setPropertyThreadSafe(groupIdProperty, group.getGroupId());
		}

		if (descriptionProperty != null)
		{
			this.setPropertyThreadSafe(descriptionProperty, group.getDescription());
		}

		if (inboundPermProperty != null)
		{
			this.setPropertyThreadSafe(inboundPermProperty, CollectionsHelper.toString(group.getIpPermissions()));
		}

		if (outboundPermProperty != null)
		{
			this.setPropertyThreadSafe(outboundPermProperty, CollectionsHelper.toString(group.getIpPermissionsEgress()));
		}

		if (vpcIdProperty != null)
		{
			this.setPropertyThreadSafe(vpcIdProperty, group.getVpcId());
		}

		if (tagsProperty != null)
		{
			this.setPropertyThreadSafe(tagsProperty, CollectionsHelper.toString(group.getTags()));
		}
	}
}
