package com.anttoolkit.aws.ec2.common;

import com.amazonaws.services.ec2.model.*;

public class PrivateIpAddressSpecificationWrapper
{
	private PrivateIpAddressSpecification spec = new PrivateIpAddressSpecification();

	public void setPrivateIpAddress(String address)
	{
		spec.setPrivateIpAddress(address);
	}

	public void setPrimary(boolean primary)
	{
		spec.setPrimary(primary);
	}

	public PrivateIpAddressSpecification getSpecification()
	{
		return spec;
	}
}
