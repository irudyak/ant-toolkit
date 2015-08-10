package com.anttoolkit.aws.ec2.tasks.instance;

import java.util.*;

import org.apache.tools.ant.*;

public class WaitInstancesStateTask extends InstanceTask
{
	private String state;
	private List<String> ids = new LinkedList<String>();

	public void setState(String state)
	{
		if (state == null || state.isEmpty())
		{
			throw new IllegalArgumentException("Instance state can't be empty");
		}

		this.state = state.trim().toLowerCase();

		if (!SUPPORTED_STATES.containsKey(this.state))
		{
			throw new IllegalArgumentException("Incorrect instance state specified: " + state);
		}
	}

	public void setInstanceIds(String ids)
	{
		if (ids == null || ids.isEmpty())
		{
			throw new IllegalArgumentException("Instance ids can't be empty");
		}

		for (String id : ids.split(","))
		{
			id = id.trim();
			if (!id.isEmpty() && !this.ids.contains(id))
			{
				this.ids.add(id);
			}
		}
	}

	@Override
	public void doWork() throws BuildException
	{
		waitInstancesStatusToChange(ids, SUPPORTED_STATES.get(state), true);
	}

	@Override
	protected void validate()
	{
		if (ids.isEmpty())
		{
			throw new BuildException("Instance ids should be specified");
		}

		if (state == null || state.isEmpty())
		{
			throw new BuildException("Instance state should be specified");
		}
	}
}
