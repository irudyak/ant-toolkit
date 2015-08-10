package com.anttoolkit.aws.ec2.tasks.securitygroup.util;

import java.util.*;

import com.amazonaws.services.ec2.*;
import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

public class Authorize
{
	private List<InboundRequest> inbounds = new LinkedList<InboundRequest>();
	private List<OutboundRequest> outbounds = new LinkedList<OutboundRequest>();

	public void addConfiguredInbound(InboundRequest request)
	{
		inbounds.add(request);
	}

	public void addConfiguredOutbound(OutboundRequest request)
	{
		outbounds.add(request);
	}

	public void authorize(String groupName, String groupId, AmazonEC2Client client)
	{
		for (InboundRequest request : inbounds)
		{
			AuthorizeSecurityGroupIngressRequest authRequest = request.getAuthorizeRequest(groupName, groupId);

			try
			{
				client.authorizeSecurityGroupIngress(authRequest);
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to authorize inbound rule: " + authRequest.toString(), e);
			}
		}

		for (OutboundRequest request : outbounds)
		{
			AuthorizeSecurityGroupEgressRequest authRequest = request.getAuthorizeRequest(groupId);

			try
			{
				client.authorizeSecurityGroupEgress(authRequest);
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to authorize outbound rule: " + authRequest.toString(), e);
			}
		}
	}
}
