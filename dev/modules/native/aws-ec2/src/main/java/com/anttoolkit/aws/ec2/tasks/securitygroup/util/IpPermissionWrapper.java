package com.anttoolkit.aws.ec2.tasks.securitygroup.util;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import com.anttoolkit.general.common.*;

public class IpPermissionWrapper
{
	private IpPermission permission = new IpPermission();

	private List<UserIdGroupPair> userIdGroupPairs;
	private List<String> ipRanges;
	private List<PrefixListId> prefixListIds;

	public void setIpProtocol(String ipProtocol)
	{
		permission.setIpProtocol(ipProtocol);
	}

	public void setFromPort(Integer fromPort)
	{
		permission.setFromPort(fromPort);
	}

	public void setToPort(Integer toPort)
	{
		permission.setToPort(toPort);
	}

	public void addConfiguredUserIdGroupPair(UserIdGroupPairWrapper pair)
	{
		if (userIdGroupPairs == null)
		{
			userIdGroupPairs = new LinkedList<UserIdGroupPair>();
		}

		userIdGroupPairs.add(pair.getPair());
	}

	public void addConfiguredIpRange(ValueHolder holder)
	{
		if (ipRanges == null)
		{
			ipRanges = new LinkedList<String>();
		}

		ipRanges.add(holder.getValue());
	}

	public void addConfiguredPrefixListId(ValueHolder holder)
	{
		if (prefixListIds == null)
		{
			prefixListIds = new LinkedList<PrefixListId>();
		}

		prefixListIds.add((new PrefixListId()).withPrefixListId(holder.getValue()));
	}

	public IpPermission getPermission()
	{
		if (userIdGroupPairs != null)
		{
			permission.setUserIdGroupPairs(userIdGroupPairs);
		}

		if (ipRanges != null)
		{
			permission.setIpRanges(ipRanges);
		}

		if (prefixListIds != null)
		{
			permission.setPrefixListIds(prefixListIds);
		}

		return permission;
	}
}
