package com.anttoolkit.aws.ec2.tasks.instance.util;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

public class InstanceNetworkInterfaceSpecificationWrapper
{
	private InstanceNetworkInterfaceSpecification spec = new InstanceNetworkInterfaceSpecification();

	private List<String> groups;
	private List<PrivateIpAddressSpecification> privateIpAddresses;

	public void setNetworkInterfaceId(String id)
	{
		spec.setNetworkInterfaceId(id);
	}

	public void setDeviceIndex(int index)
	{
		spec.setDeviceIndex(index);
	}

	public void setSubnetId(String id)
	{
		spec.setSubnetId(id);
	}

	public void setDescription(String description)
	{
		spec.setDescription(description);
	}

	public void setPrivateIpAddress(String address)
	{
		spec.setPrivateIpAddress(address);
	}

	public void addConfiguredGroup(String group)
	{
		if (groups == null)
		{
			groups = new LinkedList<String>();
		}

		if (!groups.contains(group.trim()))
		{
			groups.add(group.trim());
		}
	}

	public void setDeleteOnTermination(boolean delete)
	{
		spec.setDeleteOnTermination(delete);
	}

	public void addConfiguredPrivateIpAddress(PrivateIpAddressSpecificationWrapper wrapper)
	{
		if (privateIpAddresses == null)
		{
			privateIpAddresses = new LinkedList<PrivateIpAddressSpecification>();
		}

		privateIpAddresses.add(wrapper.getSpecification());
	}

	public void setSecondaryPrivateIpAddressCount(int count)
	{
		spec.setSecondaryPrivateIpAddressCount(count);
	}

	public void setAssociatePublicIpAddress(boolean associate)
	{
		spec.setAssociatePublicIpAddress(associate);
	}

	public InstanceNetworkInterfaceSpecification getSpecification()
	{
		if (groups != null)
		{
			spec.setGroups(groups);
		}

		if (privateIpAddresses != null)
		{
			spec.setPrivateIpAddresses(privateIpAddresses);
		}

		return spec;
	}
}
