package com.anttoolkit.maestro.tasks.instance;

import java.util.*;

import com.anttoolkit.maestro.common.*;
import com.anttoolkit.maestro.tasks.*;
import com.maestro.xml.XMLAttribute;
import com.maestro.xml.XMLElement;
import org.apache.tools.ant.BuildException;

public abstract class InstanceTask extends CliCommandTask
{
	public static final long WAIT_TIME = 1200000;	//20 mins
	public static final long SLEEP_TIMEOUT = 60000;	//1min

	@Param(sorted = true)
	private Set<String> instances = new HashSet<String>();

	private boolean async = false;
	private long wait = WAIT_TIME;

	public void setInstance(String instances)
	{
		this.populateParamValues(instances, this.instances);
	}

	public void setAsync(boolean async)
	{
		this.async = async;
	}

	public boolean isAsync()
	{
		return async;
	}

	public void setWait(long wait)
	{
		if (wait <= 0)
		{
			throw new BuildException("Incorrect wait time specified: " + wait);
		}

		this.wait = wait;
	}

	protected void setInstance(Set<String> instances)
	{
		this.instances = instances;
	}

	protected Set<String> getInstances()
	{
		return instances;
	}

	protected Map<String, Map<String, String>> getAllInstancesInfo()
			throws InstanceNotFoundException
	{
		Map<String, String> params = regionAndProjectParams();

		XMLElement element = this.executeCommand("POST", "describe-instances", params);

		Map<String, Map<String, String>> result = parseInstancesInfo(element);

		for (String id : instances)
		{
			if (!result.containsKey(id))
			{
				throw new InstanceNotFoundException("There is no instance with id: " + id);
			}
		}

		return result;
	}

	protected Map<String, Map<String, String>> parseInstancesInfo(XMLElement element)
	{
		Map<String, Map<String, String>> result = new HashMap<String, Map<String, String>>();

		List<XMLElement> items = element.Items();
		for (XMLElement item : items)
		{
			XMLAttribute idAttr = item.getAttribute("instanceID");
			String id = idAttr == null || idAttr.getValue() == null ? null : idAttr.getValue();
			if (id == null)
			{
				continue;
			}

			Map<String, String> info = new HashMap<String, String>();
			List<XMLAttribute> attrs = item.getAttrs();
			for (XMLAttribute attr : attrs)
			{
				if (attr.getName().equals("instanceID"))
				{
					continue;
				}

				info.put(attr.getName(), attr.getValue());
			}

			result.put(id, info);
		}

		return result;
	}

	protected void waitInstancesStateToChange(String state)
	{
		long start = System.currentTimeMillis();

		Set<String> readyInstances = new HashSet<String>();

		while (System.currentTimeMillis() - start < wait)
		{
			try
			{
				Map<String, Map<String, String>> info = getAllInstancesInfo();
				for (String id : instances)
				{
					Map<String, String> params = info.get(id);
					if (params != null && state.equals(params.get("state")))
					{
						readyInstances.add(id);
					}
				}

				if (readyInstances.size() == instances.size())
				{
					return;
				}

				Thread.sleep(SLEEP_TIMEOUT);
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Waiting for instances '" + getStringFromCollection(instances) +
						"' to switch to '" + state + "' state was interrupted", e);
			}
		}

		if (readyInstances.size() < instances.size())
		{
			throw new BuildException("Wait time exceeded, but not all instances '" +
					getStringFromCollection(instances) + "' switched to '" + state + "' state");
		}
	}

	protected void waitInstancesToStart()
	{
		long start = System.currentTimeMillis();

		Set<String> readyInstances = new HashSet<String>();

		while (System.currentTimeMillis() - start < wait)
		{
			try
			{
				Map<String, Map<String, String>> info = getAllInstancesInfo();
				for (String id : instances)
				{
					Map<String, String> params = info.get(id);
					if (params != null &&
						"running".equals(params.get("state")) &&
						params.get("privateIP") != null &&
						!params.get("privateIP").trim().isEmpty())
					{
						readyInstances.add(id);
					}
				}

				if (readyInstances.size() == instances.size())
				{
					return;
				}

				Thread.sleep(SLEEP_TIMEOUT);
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Waiting for instances '" + getStringFromCollection(instances) +
						"' to switch to 'running' state was interrupted", e);
			}
		}

		if (readyInstances.size() < instances.size())
		{
			throw new BuildException("Wait time exceeded, but not all instances '" +
					getStringFromCollection(instances) + "' switched to 'running' state or obtained IP addresses");
		}
	}

	protected void waitInstancesToTerminate()
	{
		long start = System.currentTimeMillis();

		Set<String> terminatedInstances = new HashSet<String>();

		while (System.currentTimeMillis() - start < wait)
		{
			try
			{
				Map<String, Map<String, String>> info = getAllInstancesInfo();
				for (String id : instances)
				{
					Map<String, String> params = info.get(id);
					if (params != null)
					{
						terminatedInstances.add(id);
					}
				}

				if (terminatedInstances.size() == instances.size())
				{
					return;
				}

				Thread.sleep(SLEEP_TIMEOUT);
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Waiting for instances '" + getStringFromCollection(instances) +
						"' to terminate was interrupted", e);
			}
		}

		if (terminatedInstances.size() < instances.size())
		{
			throw new BuildException("Wait time exceeded, but not all instances '" +
					getStringFromCollection(instances) + "' were terminated");
		}
	}
}
