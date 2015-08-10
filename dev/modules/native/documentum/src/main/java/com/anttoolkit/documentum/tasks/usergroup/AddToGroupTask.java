package com.anttoolkit.documentum.tasks.usergroup;

import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

public class AddToGroupTask
		extends GenericGroupTask
{
	public void doWork()
			throws BuildException
	{
		IDfGroup group =  this.getRootGroup();

		String[] users = this.getUsers();
		for (String user : users)
		{
			try
			{
				if (!group.isUserInGroup(user))
				{
					group.addUser(user);
				}
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to add user=" + user + " to group=" +
						this.getRootGroupName(), e);
			}
		}

		String[] groups = this.getGroups();
		for (String _group : groups)
		{
			try
			{
				if (!group.isGroupInGroup(_group))
				{
					group.addGroup(_group);
				}
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to add group=" + _group + " to group=" +
						this.getRootGroupName(), e);
			}
		}

		this.saveRootGroup(group);
	}
}
