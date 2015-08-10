package com.anttoolkit.aws.ec2.common;

public class InstanceNotFoundException extends RuntimeException
{
	public final String instanceId;

	public InstanceNotFoundException(String instanceId, String message)
	{
		super(message);

		this.instanceId = instanceId;
	}
}
