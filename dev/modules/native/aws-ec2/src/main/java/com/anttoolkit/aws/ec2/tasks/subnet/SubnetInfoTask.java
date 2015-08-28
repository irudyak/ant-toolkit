package com.anttoolkit.aws.ec2.tasks.subnet;

import com.amazonaws.services.ec2.model.*;

import com.anttoolkit.general.common.CollectionsHelper;

import com.anttoolkit.aws.ec2.tasks.*;

public abstract class SubnetInfoTask extends GenericEc2Task
{
	private String subnetIdProperty;
	private String stateProperty;
	private String vpcIdProperty;
	private String cidrBlockProperty;
	private String ipAddressCountProperty;
	private String availabilityZoneProperty;
	private String defaultForAzProperty;
	private String mapPublicIpProperty;
	private String tagsProperty;

	public void setSubnetIdProperty(String property)
	{
		subnetIdProperty = property;
	}

	public void setStateProperty(String property)
	{
		stateProperty = property;
	}

	public void setVpcIdProperty(String property)
	{
		vpcIdProperty = property;
	}

	public void setCidrBlockProperty(String property)
	{
		cidrBlockProperty = property;
	}

	public void setIpAddressCountProperty(String property)
	{
		ipAddressCountProperty = property;
	}

	public void setAvailabilityZoneProperty(String property)
	{
		availabilityZoneProperty = property;
	}

	public void setDefaultForAzProperty(String property)
	{
		defaultForAzProperty = property;
	}

	public void setMapPublicIpProperty(String property)
	{
		mapPublicIpProperty = property;
	}

	public void setTagsProperty(String property)
	{
		tagsProperty = property;
	}

	protected void setPropertiesFromSubnet(Subnet subnet)
	{
		if (subnetIdProperty != null)
		{
			this.setPropertyThreadSafe(subnetIdProperty, subnet.getSubnetId());
		}

		if (stateProperty != null)
		{
			this.setPropertyThreadSafe(stateProperty, subnet.getState());
		}

		if (vpcIdProperty != null)
		{
			this.setPropertyThreadSafe(vpcIdProperty, subnet.getVpcId());
		}

		if (cidrBlockProperty != null)
		{
			this.setPropertyThreadSafe(cidrBlockProperty, subnet.getCidrBlock());
		}

		if (ipAddressCountProperty != null)
		{
			this.setPropertyThreadSafe(ipAddressCountProperty, subnet.getAvailableIpAddressCount() != null ? subnet.getAvailableIpAddressCount().toString() : "0");
		}

		if (availabilityZoneProperty != null)
		{
			this.setPropertyThreadSafe(availabilityZoneProperty, subnet.getAvailabilityZone());
		}

		if (defaultForAzProperty != null)
		{
			this.setPropertyThreadSafe(defaultForAzProperty, subnet.getDefaultForAz() != null ? subnet.getDefaultForAz().toString() : "false");
		}

		if (mapPublicIpProperty != null)
		{
			this.setPropertyThreadSafe(mapPublicIpProperty, subnet.getMapPublicIpOnLaunch() != null ? subnet.getMapPublicIpOnLaunch().toString() : "false");
		}

		if (tagsProperty != null)
		{
			this.setPropertyThreadSafe(tagsProperty, CollectionsHelper.toString(subnet.getTags()));
		}
	}
}
