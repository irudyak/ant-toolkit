package com.anttoolkit.aws.ec2.tasks.securitygroup.util;

import java.util.*;

import com.amazonaws.services.ec2.*;
import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

public class Revoke
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

	public void revoke(String groupName, String groupId, AmazonEC2Client client)
	{
		for (InboundRequest request : inbounds)
		{
			RevokeSecurityGroupIngressRequest revokeRequest = request.getRevokeRequest(groupName, groupId);

			try
			{
				client.revokeSecurityGroupIngress(revokeRequest);
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to revoke inbound rule: " + revokeRequest.toString(), e);
			}
		}

		for (OutboundRequest request : outbounds)
		{
			RevokeSecurityGroupEgressRequest revokeRequest = request.getRevokeRequest(groupId);

			try
			{
				client.revokeSecurityGroupEgress(revokeRequest);
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to revoke outbound rule: " + revokeRequest.toString(), e);
			}
		}
	}
}
