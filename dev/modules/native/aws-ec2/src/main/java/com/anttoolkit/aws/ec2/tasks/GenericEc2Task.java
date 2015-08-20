package com.anttoolkit.aws.ec2.tasks;

import java.util.*;

import com.amazonaws.*;
import com.amazonaws.regions.*;
import com.amazonaws.services.ec2.*;
import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.tasks.*;
import com.anttoolkit.general.common.*;

public abstract class GenericEc2Task extends AwsClientTask
{
	protected final AmazonEC2Client getEc2Client()
	{
		try
		{
			return (AmazonEC2Client) getClient();
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to instantiate AWS EC2 client", e);
		}
	}

	protected final String serviceName()
	{
		return ServiceAbbreviations.EC2;
	}

	protected final Class<? extends AmazonWebServiceClient> awsClientClass()
	{
		return AmazonEC2Client.class;
	}

	protected final void createTags(Collection<String> resources, Tag... tags)
	{
		CreateTagsRequest request = new CreateTagsRequest();
		request.setResources(resources);
		request.setTags(Arrays.asList(tags));
		getEc2Client().createTags(request);
	}

	protected final void createTags(Collection<String> resources, Collection<Tag> tags)
	{
		CreateTagsRequest request = new CreateTagsRequest();
		request.setResources(resources);
		request.setTags(tags);
		getEc2Client().createTags(request);
	}

	protected final void createTags(String resources, Tag... tags)
	{
		CreateTagsRequest request = new CreateTagsRequest();
		request.setResources(CollectionsHelper.asList(resources, ",", true, true));
		request.setTags(Arrays.asList(tags));
		getEc2Client().createTags(request);
	}

	protected final void createTags(String resources, Collection<Tag> tags)
	{
		CreateTagsRequest request = new CreateTagsRequest();
		request.setResources(CollectionsHelper.asList(resources, ",", true, true));
		request.setTags(tags);
		getEc2Client().createTags(request);
	}

	protected final void setResourceName(String resources, String name)
	{
		createTags(resources, new Tag("name", name));
	}

	protected final void setResourceName(Collection<String> resources, String name)
	{
		createTags(resources, new Tag("name", name));
	}

}
