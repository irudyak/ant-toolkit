package com.anttoolkit.maestro.tasks.instance;

import java.util.*;

import org.apache.tools.ant.*;

import com.maestro.xml.*;

import com.anttoolkit.maestro.common.*;
import com.anttoolkit.general.tasks.collections.map.util.*;

@SupressParams({"instances"})
@Action("run-instances")
public class RunInstanceTask extends InstanceTask
{
	@Param("image-id")
	private String image;

	@Param
	private String shape;

	@Param
	private int count = 1;

	@Param("key-name")
	private String keyName;

	@Param
	private String expiration;

	@Param("file-name")
	private List<String> scripts = new LinkedList<String>();

	private String ipsMap;

	public void setImage(String image)
	{
		this.image = image;
	}

	public void setShape(String shape)
	{
		this.shape = shape;
	}

	public void setCount(int count)
	{
		if (count <= 0 || count > 5)
		{
			throw new BuildException("Incorrect instances count specified: " + count);
		}

		this.count = count;
	}

	public void setKeyName(String keyName)
	{
		this.keyName = keyName;
	}

	public void setScript(String script)
	{
		if (script == null || script.trim().isEmpty())
		{
			scripts.clear();
			return;
		}

		String[] files = script.split(",", -1);
		Collections.addAll(scripts, files);
	}

	public void setIpsMap(String ipsMap)
	{
		this.ipsMap = ipsMap;
	}

	@Override
	public void doWork() throws BuildException
	{
		XMLElement element = executeCommand();

		Map<String, Map<String, String>> info = parseInstancesInfo(element);
		setInstance(info.keySet());

		this.printResult(element);

		if (isAsync())
		{
			if (ipsMap != null)
			{
				for (String id : info.keySet())
				{
					MapManager.put(ipsMap, id, "");
				}
			}

			return;
		}

		waitInstancesStateToChange("running");

		if (ipsMap == null)
		{
			return;
		}

		info = getAllInstancesInfo();

		for (String id : info.keySet())
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
