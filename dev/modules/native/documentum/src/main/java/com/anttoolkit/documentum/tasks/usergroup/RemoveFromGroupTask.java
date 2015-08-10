package com.anttoolkit.documentum.tasks.usergroup;

import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

public class RemoveFromGroupTask
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
				if (group.isUserInGroup(user))
				{
					group.removeUser(user);
				}
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to remove user=" + user + " from group=" +
						this.getRootGroupName(), e);
			}
		}

		String[] groups = this.getGroups();
		for (String _group : groups)
		{
			try
			{
				if (group.isGroupInGroup(_group))
				{
					group.removeGroup(_group);
				}
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to remove group=" + _group + " from group=" +
						this.getRootGroupName(), e);
			}
		}

		this.saveRootGroup(group);
	}
}
