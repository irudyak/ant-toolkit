package com.anttoolkit.aws.ec2.tasks.instance;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;

public class ModifyInstanceTask extends InstanceTask
{
	private String instanceId;

	private Map<String, String> attrs = new HashMap<String, String>();

	public void setInstanceId(String id)
	{
		instanceId = id;
	}

	public void addConfiguredAttribute(NameValueHolder holder)
	{
		attrs.put(holder.getName(), holder.getValue());
	}

	@Override
	public void doWork() throws BuildException
	{
		for (String attribute : attrs.keySet())
		{
			ModifyInstanceAttributeRequest request = new ModifyInstanceAttributeRequest(instanceId, attribute);
			request.setValue(attrs.get(attribute));

			try
			{
				getEc2Client().modifyInstanceAttribute(request);
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to set attribute '" + attribute + "=" + attrs.get(attribute) + "' for instance " + instanceId, e);
			}
		}
	}

	@Override
	protected void validate()
	{
		if (instanceId == null || instanceId.trim().isEmpty())
		{
			throw new BuildException("Instance id should be specified");
		}

		if (attrs.isEmpty())
		{
			throw new BuildException("Instance attributes should be specified");
		}

	}
}
