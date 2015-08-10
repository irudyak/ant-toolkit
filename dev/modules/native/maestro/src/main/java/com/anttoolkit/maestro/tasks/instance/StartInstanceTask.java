package com.anttoolkit.maestro.tasks.instance;

import java.util.*;

import com.maestro.xml.*;
import org.apache.tools.ant.*;

import com.anttoolkit.maestro.common.*;
import com.anttoolkit.general.tasks.collections.map.util.*;

@Action("start-instances")
public class StartInstanceTask extends InstanceTask
{
	@Param
	private String expiration;

	@Param("ignore-state")
	private boolean ignoreState;

	private String ipsMap;

	public void setExpiration(String expiration)
	{
		this.expiration = expiration;
	}

	public void setIgnoreState(boolean ignore)
	{
		ignoreState = ignore;
	}

	public void setIpsMap(String ipsMap)
	{
		this.ipsMap = ipsMap;
	}

	@Override
	public void doWork() throws BuildException
	{
		XMLElement element = executeCommand();

		this.printResult(element);

		if (isAsync())
		{
			return;
		}

		waitInstancesToStart();

		if (ipsMap == null)
		{
			return;
		}

		Map<String, Map<String, String>> info = getAllInstancesInfo();

		Set<String> ids = getInstances();
		for (String id : ids)
		{
			String ip = info.get(id).get("privateIP");
			MapManager.put(ipsMap, id, ip);
		}
	}

	@Override
	protected void validate()
	{
		if (ipsMap != null && !MapManager.exists(ipsMap))
		{
			throw new BuildException("Specified map '" + ipsMap + "' doesn't exist");
		}
	}
}
