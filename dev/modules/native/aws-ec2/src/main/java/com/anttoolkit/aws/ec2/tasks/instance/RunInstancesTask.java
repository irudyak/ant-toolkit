package com.anttoolkit.aws.ec2.tasks.instance;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.general.tasks.collections.array.util.*;
import com.anttoolkit.aws.ec2.common.*;

public class RunInstancesTask extends InstanceTask
{
	private String instanceName;

	private String instanceIdProperty;
	private String publicIpProperty;
	private String privateIpProperty;
	private String publicDnsProperty;
	private String privateDnsProperty;
	private String instanceIdsArray;
	private String publicIpsArray;
	private String privateIpsArray;
	private String publicDnsArray;
	private String privateDnsArray;

	private RunInstancesRequest request = new RunInstancesRequest();

	private List<String> securityGroups;
	private List<String> securityGroupIds;
	private List<BlockDeviceMapping> blockDeviceMappings;
	private List<InstanceNetworkInterfaceSpecification> networkInterfaces;

	public void setName(String name)
	{
		instanceName = name;
	}

	public void setInstanceIdProperty(String property)
	{
		this.instanceIdProperty = property;
	}

	public void setPublicIpProperty(String property)
	{
		this.publicIpProperty = property;
	}

	public void setPrivateIpProperty(String property)
	{
		this.privateIpProperty = property;
	}

	public void setPublicDnsProperty(String property)
	{
		this.publicDnsProperty = property;
	}

	public void setPrivateDnsProperty(String property)
	{
		this.privateDnsProperty = property;
	}

	public void setInstanceIdsArray(String array)
	{
		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Array '" + array + "' to store instance ids doesn't exist.");
		}

		this.instanceIdsArray = array;
	}

	public void setPublicIpsArray(String array)
	{
		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Array '" + array + "' to store instance public IPs doesn't exist.");
		}

		this.publicIpsArray = array;
	}

	public void setPrivateIpsArray(String array)
	{
		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Array '" + array + "' to store instance private IPs doesn't exist.");
		}

		this.privateIpsArray = array;
	}

	public void setPublicDnsArray(String array)
	{
		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Array '" + array + "' to store instance public DNS doesn't exist.");
		}

		this.publicDnsArray = array;
	}

	public void setPrivateDnsArray(String array)
	{
		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Array '" + array + "' to store instance private DNS doesn't exist.");
		}

		this.privateDnsArray = array;
	}

	public void setCount(String count)
	{
		if (count == null || count.trim().isEmpty() || count.split(":").length > 2)
		{
			throw new BuildException("Incorrect instance count specified " + count);
		}

		String[] parts = count.split(":");

		try
		{
			request.setMinCount(Integer.parseInt(parts[0]));

			if (parts.length == 2)
			{
				request.setMaxCount(Integer.parseInt(parts[1]));
			}
		}
		catch (NumberFormatException e)
		{
			throw new BuildException("Incorrect instance count specified " + count);
		}
	}

	public void setImageId(String imageId)
	{
		request.setImageId(imageId);
	}

	public void setKeyName(String keyName)
	{
		request.setKeyName(keyName);
	}

	public void addConfiguredSecurityGroup(ValueHolder group)
	{
		if (securityGroups == null)
		{
			securityGroups = new LinkedList<String>();
		}

		if (!securityGroups.contains(group.getValue()))
		{
			securityGroups.add(group.getValue());
		}
	}

	public void addConfiguredSecurityGroupId(ValueHolder groupId)
	{
		if (securityGroupIds == null)
		{
			securityGroupIds = new LinkedList<String>();
		}

		if (!securityGroupIds.contains(groupId.getValue()))
		{
			securityGroupIds.add(groupId.getValue());
		}
	}

	public void setUserData(String data)
	{
		request.setUserData(data);
	}

	public void setInstanceType(String type)
	{
		request.setInstanceType(type);
	}

	public void setPlacement(String placement)
	{
		request.setPlacement(PlacementParser.parsePlacement(placement));
	}

	public void setKernelId(String id)
	{
		request.setKernelId(id);
	}

	public void setRamdiskId(String id)
	{
		request.setRamdiskId(id);
	}

	public void addConfiguredBlockDevice(BlockDeviceMappingWrapper wrapper)
	{
		if (blockDeviceMappings == null)
		{
			blockDeviceMappings = new LinkedList<BlockDeviceMapping>();
		}

		blockDeviceMappings.add(wrapper.getMapping());
	}

	public void setMonitoring(boolean monitoring)
	{
		request.setMonitoring(monitoring);
	}

	public void setSubnetId(String subnetId)
	{
		request.setSubnetId(subnetId);
	}

	public void setDisableApiTermination(boolean disable)
	{
		request.setDisableApiTermination(disable);
	}

	public void setShutdownBehavior(String behaviour)
	{
		request.setInstanceInitiatedShutdownBehavior(behaviour);
	}

	public void setPrivateIpAddress(String address)
	{
		request.setPrivateIpAddress(address);
	}

	public void setClientToken(String token)
	{
		request.setClientToken(token);
	}

	public void setAdditionalInfo(String info)
	{
		request.setAdditionalInfo(info);
	}

	public void addConfiguredNetworkInterface(InstanceNetworkInterfaceSpecificationWrapper wrapper)
	{
		if (networkInterfaces == null)
		{
			networkInterfaces = new LinkedList<InstanceNetworkInterfaceSpecification>();
		}

		networkInterfaces.add(wrapper.getSpecification());
	}

	public void setIamInstanceProfile(String profile)
	{
		request.setIamInstanceProfile(IamInstanceProfileParser.parseProfile(profile));
	}

	public void setEbsOptimized(boolean optimized)
	{
		request.setEbsOptimized(optimized);
	}

	@Override
	public void doWork() throws BuildException
	{
		if (request.getMinCount() == null || request.getMinCount() <= 0)
		{
			request.setMinCount(1);
		}

		if (request.getMaxCount() == null || request.getMaxCount() <= 0)
		{
			request.setMaxCount(1);
		}

		if (securityGroups != null)
		{
			request.setSecurityGroups(securityGroups);
		}

		if (securityGroupIds != null)
		{
			request.setSecurityGroupIds(securityGroupIds);
		}

		if (blockDeviceMappings != null)
		{
			request.setBlockDeviceMappings(blockDeviceMappings);
		}

		if (networkInterfaces != null)
		{
			request.setNetworkInterfaces(networkInterfaces);
		}

		processResult(getEc2Client().runInstances(request));
	}

	@Override
	protected void validate()
	{
		if (request.getImageId() == null)
		{
			throw new BuildException("Instance image id should be specified");
		}

		if (request.getInstanceType() == null)
		{
			throw new BuildException("Instance type should be specified");
		}

		if (request.getKeyName() == null)
		{
			throw new BuildException("Instance key should be specified");
		}
	}

	private void processResult(RunInstancesResult result)
	{
		Collection<String> instanceIds = this.getInstanceIds(result.getReservation());

		log("Instances created: " + CollectionsHelper.toString(instanceIds));

		if (instanceName != null)
		{
			try
			{
				createTags(instanceIds, new Tag("name", instanceName));
			}
			catch (Throwable e)
			{
			}
		}

		if (isAsync())
		{
			if (instanceIdProperty != null)
			{
				setPropertyThreadSafe(instanceIdProperty, instanceIds.iterator().next());
			}

			if (instanceIdsArray != null)
			{
				for (String id : instanceIds)
				{
					ArrayManager.add(instanceIdsArray, id);
				}
			}

			return;
		}

		waitInstancesStatusToChange(instanceIds, RUNNING_STATUS, true);

		Map<String, Instance> instancesInfo = getInstances(instanceIds);
		Instance firstInst = instancesInfo.values().iterator().next();

		if (instanceIdProperty != null)
		{
			setPropertyThreadSafe(instanceIdProperty, firstInst.getInstanceId());
		}

		if (publicIpProperty != null)
		{
			setPropertyThreadSafe(publicIpProperty, firstInst.getPublicIpAddress());
		}

		if (privateIpProperty != null)
		{
			setPropertyThreadSafe(privateIpProperty, firstInst.getPrivateIpAddress());
		}

		if (publicDnsProperty != null)
		{
			setPropertyThreadSafe(publicDnsProperty, firstInst.getPublicDnsName());
		}

		if (privateDnsProperty != null)
		{
			setPropertyThreadSafe(privateDnsProperty, firstInst.getPrivateDnsName());
		}

		for (Instance inst : instancesInfo.values())
		{
			if (instanceIdsArray != null)
			{
				ArrayManager.add(instanceIdsArray, inst.getInstanceId());
			}

			if (publicIpsArray != null)
			{
				ArrayManager.add(publicIpsArray, inst.getPublicIpAddress());
			}

			if (privateIpsArray != null)
			{
				ArrayManager.add(privateIpsArray, inst.getPrivateIpAddress());
			}

			if (publicDnsArray != null)
			{
				ArrayManager.add(publicDnsArray, inst.getPublicDnsName());
			}

			if (privateDnsArray != null)
			{
				ArrayManager.add(privateDnsArray, inst.getPrivateDnsName());
			}
		}
	}
}
