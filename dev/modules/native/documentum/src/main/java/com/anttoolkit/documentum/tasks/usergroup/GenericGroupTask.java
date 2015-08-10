package com.anttoolkit.documentum.tasks.usergroup;

import com.anttoolkit.documentum.common.*;

import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

public abstract class GenericGroupTask
		extends GenericDocbaseTask

{
	private String rootGroup = null;
	private String user = null;
	private String group = null;

	public void setRootGroup(String group)
	{
		rootGroup = group.toLowerCase();
	}

	public String getRootGroupName()
	{
		return rootGroup;
	}

	public void setUser(String user)
	{
		this.user = user.toLowerCase();
	}

	public void setGroup(String group)
	{
		this.group = group.toLowerCase();
	}

	protected void validate()
	{
		if (rootGroup == null)
		{
			throw new BuildException("rootGroup doesn't specified");
		}

		if (user == null && group == null)
		{
			throw new BuildException("user ot group should be specified");
		}
	}

	protected String[] getUsers()
	{
		if (user == null)
		{
			return new String[]{};
		}

		String[] users = user.split(",", -1);

		int count = users.length;
		for (int i = 0; i < count; i++)
		{
			users[i] = users[i].trim();
		}

		return users;
	}

	protected String[] getGroups()
	{
		if (group == null)
		{
			return new String[]{};
		}

		String[] groups = group.split(",", -1);

		int count = groups.length;
		for (int i = 0; i < count; i++)
		{
			groups[i] = groups[i].trim();
		}

		return groups;
	}

	protected IDfGroup getRootGroup()
			throws BuildException
	{
		try
		{
			return this.getSession().getDfSession().getGroup(rootGroup);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get root group=" + rootGroup + " from docbase", e);
		}
	}

	protected void saveRootGroup(IDfGroup group)
			throws BuildException
	{
		try
		{
			group.save();
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to save root group: " + rootGroup, e);
		}
	}
}
