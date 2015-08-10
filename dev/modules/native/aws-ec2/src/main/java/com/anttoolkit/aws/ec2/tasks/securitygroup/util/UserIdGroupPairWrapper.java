package com.anttoolkit.aws.ec2.tasks.securitygroup.util;

import com.amazonaws.services.ec2.model.*;

public class UserIdGroupPairWrapper
{
	private UserIdGroupPair pair = new UserIdGroupPair();

	public void setUserId(String userId)
	{
		pair.setUserId(userId);
	}

	public void setGroupName(String groupName)
	{
		pair.setGroupName(groupName);
	}

	public void setGroupId(String groupId)
	{
		pair.setGroupId(groupId);
	}

	public UserIdGroupPair getPair()
	{
		return pair;
	}
}
