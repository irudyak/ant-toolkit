package com.anttoolkit.aws.ec2.tasks.securitygroup.util;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

public class OutboundRequest
{
	private AuthorizeSecurityGroupEgressRequest authorizeRequest = new AuthorizeSecurityGroupEgressRequest();
	private RevokeSecurityGroupEgressRequest revokeRequest = new RevokeSecurityGroupEgressRequest();
	private List<IpPermission> ipPermissions;

	public void setSourceGroupName(String sourceSecurityGroupName)
	{
		authorizeRequest.setSourceSecurityGroupName(sourceSecurityGroupName);
		revokeRequest.setSourceSecurityGroupName(sourceSecurityGroupName);
	}

	public void setSourceGroupOwnerId(String sourceSecurityGroupOwnerId)
	{
		authorizeRequest.setSourceSecurityGroupOwnerId(sourceSecurityGroupOwnerId);
		revokeRequest.setSourceSecurityGroupOwnerId(sourceSecurityGroupOwnerId);
	}

	public void setIpProtocol(String ipProtocol)
	{
		authorizeRequest.setIpProtocol(ipProtocol);
		revokeRequest.setIpProtocol(ipProtocol);
	}

	public void setFromPort(Integer fromPort)
	{
		authorizeRequest.setFromPort(fromPort);
		revokeRequest.setFromPort(fromPort);
	}

	public void setToPort(Integer toPort)
	{
		authorizeRequest.setToPort(toPort);
		revokeRequest.setToPort(toPort);
	}

	public void setCidrIp(String cidrIp)
	{
		authorizeRequest.setCidrIp(cidrIp);
		revokeRequest.setCidrIp(cidrIp);
	}

	public void addConfiguredIpPermission(IpPermissionWrapper wrapper)
	{
		if (ipPermissions == null)
		{
			ipPermissions = new LinkedList<IpPermission>();
		}

		ipPermissions.add(wrapper.getPermission());
	}

	public AuthorizeSecurityGroupEgressRequest getAuthorizeRequest(String groupId)
	{
		if (ipPermissions != null)
		{
			authorizeRequest.setIpPermissions(ipPermissions);
		}

		authorizeRequest.setGroupId(groupId);

		return authorizeRequest;
	}

	public RevokeSecurityGroupEgressRequest getRevokeRequest(String groupId)
	{
		if (ipPermissions != null)
		{
			revokeRequest.setIpPermissions(ipPermissions);
		}

		revokeRequest.setGroupId(groupId);

		return revokeRequest;
	}
}
