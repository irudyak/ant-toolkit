package com.anttoolkit.aws.ec2.tasks.securitygroup.util;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

public class InboundRequest
{
	private AuthorizeSecurityGroupIngressRequest authorizeRequest = new AuthorizeSecurityGroupIngressRequest();
	private RevokeSecurityGroupIngressRequest revokeRequest = new RevokeSecurityGroupIngressRequest();
	private List<IpPermission> ipPermissions;

	public void setSourceGroupName(String sourceGroupName)
	{
		authorizeRequest.setSourceSecurityGroupName(sourceGroupName);
		revokeRequest.setSourceSecurityGroupName(sourceGroupName);
	}

	public void setSourceGroupOwnerId(String sourceGroupOwnerId)
	{
		authorizeRequest.setSourceSecurityGroupOwnerId(sourceGroupOwnerId);
		revokeRequest.setSourceSecurityGroupOwnerId(sourceGroupOwnerId);
	}

	public void setIpProtocol(String ipProtocol)
	{
		authorizeRequest.setIpProtocol(ipProtocol);
		revokeRequest.setIpProtocol(ipProtocol);
	}

	public void setFromPort(int fromPort)
	{
		authorizeRequest.setFromPort(fromPort);
		revokeRequest.setFromPort(fromPort);
	}

	public void setToPort(int toPort)
	{
		authorizeRequest.setToPort(toPort);
		revokeRequest.setToPort(toPort);
	}

	public void setCidrIp(String cidrIp)
	{
		authorizeRequest.setCidrIp(cidrIp);
		revokeRequest.setCidrIp(cidrIp);
	}

	public void addConfiguredIpPermission(IpPermissionWrapper permission)
	{
		if (ipPermissions == null)
		{
			ipPermissions = new LinkedList<IpPermission>();
		}

		ipPermissions.add(permission.getPermission());
	}

	public AuthorizeSecurityGroupIngressRequest getAuthorizeRequest(String groupName, String groupId)
	{
		if (ipPermissions != null)
		{
			authorizeRequest.setIpPermissions(ipPermissions);
		}

		authorizeRequest.setGroupName(groupName);
		authorizeRequest.setGroupId(groupId);

		return authorizeRequest;
	}

	public RevokeSecurityGroupIngressRequest getRevokeRequest(String groupName, String groupId)
	{
		if (!ipPermissions.isEmpty())
		{
			revokeRequest.setIpPermissions(ipPermissions);
		}

		revokeRequest.setGroupName(groupName);
		revokeRequest.setGroupId(groupId);

		return revokeRequest;
	}

}
