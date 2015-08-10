package com.anttoolkit.aws.ec2.tasks.tag;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.tasks.*;

public class GetTagValueTask extends GenericEc2Task
{
	private String resourceId;
	private String resourceType;
	private String key;
	private String property;

	public void setResourceId(String id)
	{
		this.resourceId = id;
	}

	public void setResourceType(String type)
	{
		this.resourceType = type;
	}

	public void setKey(String key)
	{
		this.key = key;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		List<Filter> filters = new LinkedList<Filter>();

		Filter resourceIdFilter = new Filter();
		resourceIdFilter.setName("resource-id");
		resourceIdFilter.setValues(Arrays.asList(resourceId.trim()));

		Filter keyFilter = new Filter();
		keyFilter.setName("key");
		keyFilter.setValues(Arrays.asList(key.trim()));

		filters.add(resourceIdFilter);
		filters.add(keyFilter);

		if (resourceType != null)
		{
			Filter resourceTypeFilter = new Filter();
			resourceTypeFilter.setName("resource-type");
			resourceTypeFilter.setValues(Arrays.asList(resourceType.trim()));
			filters.add(resourceTypeFilter);
		}

		DescribeTagsResult result = getEc2Client().describeTags(new DescribeTagsRequest(filters));
		String value = result == null || result.getTags() == null || result.getTags().isEmpty() ?
				"" : result.getTags().iterator().next().getValue();

		this.setPropertyThreadSafe(property, value);
	}

	@Override
	protected void validate()
	{
		if (resourceId == null || resourceId.trim().isEmpty())
		{
			throw new BuildException("Resource id should be specified");
		}

		if (key == null || key.trim().isEmpty())
		{
			throw new BuildException("Key should be specified");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property should be specified");
		}
	}
}
