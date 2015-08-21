package com.anttoolkit.aws.ec2.tasks.networkacl.util;

import java.util.*;

import com.amazonaws.services.ec2.*;
import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

public class InboundRules
{
	private int number = 100;
	private List<CreateNetworkAclEntryRequest> requests = new LinkedList<CreateNetworkAclEntryRequest>();

	public void addConfiguredAclEntry(AclEntry entry)
	{
		requests.add(entry.getRequest(number, false));
		number++;
	}

	public void createRules(AmazonEC2Client client, String aclId)
	{
		for (CreateNetworkAclEntryRequest request : requests)
		{
			request.setNetworkAclId(aclId);

			try
			{
				client.createNetworkAclEntry(request);
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to create ACL entry: " + request.toString(), e);
			}
		}
	}
}
