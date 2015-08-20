package com.anttoolkit.aws.ec2.tasks.dhcp;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.aws.ec2.tasks.*;

public class CreateDhcpOptionsTask extends GenericEc2Task
{
	private String name;
	private String optionsIdProperty;

	private List<DhcpConfiguration> options = new LinkedList<DhcpConfiguration>();

	public void setName(String name)
	{
		this.name = name;
	}

	public void setOptionsIdProperty(String property)
	{
		this.optionsIdProperty = property;
	}

	public void addConfiguredOption(KeyValueHolder holder)
	{
		DhcpConfiguration conf = new DhcpConfiguration();
		conf.setKey(holder.getKey());
		conf.setValues(CollectionsHelper.asList(holder.getValue()));
		options.add(conf);
	}

	@Override
	public void doWork() throws BuildException
	{
		CreateDhcpOptionsRequest request = new CreateDhcpOptionsRequest();
		request.setDhcpConfigurations(options);

		CreateDhcpOptionsResult result = getEc2Client().createDhcpOptions(request);
		DhcpOptions opts = result.getDhcpOptions();

		if (optionsIdProperty != null)
		{
			this.setPropertyThreadSafe(optionsIdProperty, opts.getDhcpOptionsId());
		}

		if (name != null)
		{
			setResourceName(opts.getDhcpOptionsId(), name);
		}
	}

	@Override
	protected void validate()
	{
		if (options.isEmpty())
		{
			throw new BuildException("No options specified");
		}
	}
}
