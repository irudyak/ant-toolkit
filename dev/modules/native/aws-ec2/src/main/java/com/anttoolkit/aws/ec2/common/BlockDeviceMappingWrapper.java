package com.anttoolkit.aws.ec2.common;

import com.amazonaws.services.ec2.model.*;

public class BlockDeviceMappingWrapper
{
	private BlockDeviceMapping mapping = new BlockDeviceMapping();

	public void setVirtualName(String virtualName)
	{
		mapping.setVirtualName(virtualName);
	}

	public void setDeviceName(String deviceName)
	{
		mapping.setDeviceName(deviceName);
	}

	public void addConfiguredEbs(EbsBlockDevice ebs)
	{
		mapping.setEbs(ebs);
	}

	public void setNoDevice(String noDevice)
	{
		mapping.setNoDevice(noDevice);
	}

	public BlockDeviceMapping getMapping()
	{
		return mapping;
	}
}
