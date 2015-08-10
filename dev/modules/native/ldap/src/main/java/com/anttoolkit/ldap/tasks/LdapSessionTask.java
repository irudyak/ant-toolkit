package com.anttoolkit.ldap.tasks;

import java.util.*;

import org.apache.tools.ant.*;

public class LdapSessionTask
		extends GenericLdapTask
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();

	@Override
	public void doWork() throws BuildException
	{
		for (Task task : tasks)
		{
			task.perform();
		}
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void validate()
	{
		if (!isLdapConfigSpecified())
		{
			throw new BuildException("LDAP config should be specified");
		}
	}
}
