package com.anttoolkit.aws.ec2.tasks.tag;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.aws.ec2.tasks.*;

public class CreateTagsTask extends GenericEc2Task
{
	private String resources;
	private List<Tag> tags = new LinkedList<Tag>();

	public void setResources(String resources)
	{
		if (resources == null || resources.trim().isEmpty())
		{
			throw new IllegalArgumentException("Can't specify empty resources list");
		}

		this.resources = resources;
	}

	public void addConfiguredTag(KeyValueHolder holder)
	{
		tags.add(new Tag(holder.getKey(), holder.getValue()));
	}

	@Override
	public void doWork() throws BuildException
	{
		createTags(resources, tags);
	}

	@Override
	protected void validate()
	{
		if (resources == null || resources.trim().isEmpty())
		{
			throw new BuildException("Resource should be specified");
		}

		if (tags.isEmpty())
		{
			throw new BuildException("Tags should be specified");
		}
	}
}
