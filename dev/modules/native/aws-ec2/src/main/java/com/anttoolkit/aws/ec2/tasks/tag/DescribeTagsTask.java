package com.anttoolkit.aws.ec2.tasks.tag;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.collections.array.util.*;
import com.anttoolkit.aws.ec2.tasks.*;
import com.anttoolkit.aws.ec2.common.*;

public class DescribeTagsTask extends GenericEc2Task
{
	private List<Filter> filters;
	private String resourceIdArray;
	private String resourceTypeArray;
	private String keysArray;
	private String valuesArray;

	public void setFilters(String filters)
	{
		this.filters = FilterParser.parse(filters);
	}

	public void setResourceIdArray(String array)
	{
		if (ArrayManager.exists(array))
		{
			throw new BuildException("Specified array doesn't exist");
		}

		this.resourceIdArray = array;
	}

	public void setResourceTypeArray(String array)
	{
		if (ArrayManager.exists(array))
		{
			throw new BuildException("Specified array doesn't exist");
		}

		this.resourceTypeArray = array;
	}

	public void setKeysArray(String array)
	{
		if (ArrayManager.exists(array))
		{
			throw new BuildException("Specified array doesn't exist");
		}

		this.keysArray = array;
	}

	public void setValuesArray(String array)
	{
		if (ArrayManager.exists(array))
		{
			throw new BuildException("Specified array doesn't exist");
		}

		this.valuesArray = array;
	}

	@Override
	public void doWork() throws BuildException
	{
		DescribeTagsRequest request = filters != null ?
				new DescribeTagsRequest(filters) :
				new DescribeTagsRequest();

		DescribeTagsResult result = getEc2Client().describeTags(request);
		if (result == null || result.getTags() == null || result.getTags().isEmpty())
		{
			return;
		}

		for (TagDescription desc : result.getTags())
		{
			if (resourceIdArray != null)
			{
				ArrayManager.add(resourceIdArray, desc.getResourceId());
			}

			if (resourceTypeArray != null)
			{
				ArrayManager.add(resourceTypeArray, desc.getResourceType());
			}

			if (keysArray != null)
			{
				ArrayManager.add(keysArray, desc.getKey());
			}

			if (valuesArray != null)
			{
				ArrayManager.add(valuesArray, desc.getValue());
			}
		}
	}
}
