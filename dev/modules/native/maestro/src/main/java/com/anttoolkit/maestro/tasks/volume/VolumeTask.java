package com.anttoolkit.maestro.tasks.volume;

import java.util.*;

import org.apache.tools.ant.*;

import com.maestro.xml.*;

import com.anttoolkit.maestro.tasks.*;
import com.anttoolkit.maestro.common.*;

public abstract class VolumeTask extends CliCommandTask
{
	public static final long WAIT_TIME = 1200000;	//20 mins
	public static final long SLEEP_TIMEOUT = 60000;	//1min

	private boolean async = false;
	private long wait = WAIT_TIME;

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

	protected void waitVolumeStateToChange(String volumeId, String state)
			throws VolumeNotFoundException
	{
		long start = System.currentTimeMillis();

		Set<String> readyInstances = new HashSet<String>();

		while (System.currentTimeMillis() - start < wait)
		{
			try
			{
				Map<String, String> info = getVolumeInfo(volumeId);
				if (info != null && state.equals(info.get("state")))
				{
					return;
				}

				Thread.sleep(SLEEP_TIMEOUT);
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Waiting for volume '" + volumeId +
						"' to switch to '" + state + "' state was interrupted", e);
			}
		}

		Map<String, String> info = getVolumeInfo(volumeId);
		if (info != null && !state.equals(info.get("state")))
		{
			throw new BuildException("Wait time exceeded, but volume '" +
					volumeId + "' doesn't switched to '" + state + "' state");
		}
	}

	protected Map<String, String> getVolumeInfo(String volumeId)
			throws VolumeNotFoundException
	{
		Map<String, String> params = regionAndProjectParams();
		params.put("volumes", volumeId);

		XMLElement element = this.executeCommand("POST", "describe-volumes", params);

		Map<String, Map<String, String>> info = parseVolumesInfo(element);
		if (info == null || !info.containsKey(volumeId))
		{
			throw new VolumeNotFoundException("There is no volume having id " + volumeId);
		}

		return info.get(volumeId);
	}



	protected Map<String, Map<String, String>> parseVolumesInfo(XMLElement element)
	{
		Map<String, Map<String, String>> result = new HashMap<String, Map<String, String>>();

		List<XMLElement> items = element.Items();
		for (XMLElement item : items)
		{
			XMLAttribute idAttr = item.getAttribute("id");
			String id = idAttr == null || idAttr.getValue() == null ? null : idAttr.getValue();
			if (id == null)
			{
				continue;
			}

			Map<String, String> params = new HashMap<String, String>();

			List<XMLAttribute> attrs = item.getAttrs();
			for (XMLAttribute attr : attrs)
			{
				if (attr.getName().equals("id"))
				{
					continue;
				}

				params.put(attr.getName(), attr.getValue());
			}

			result.put(id, params);
		}

		return result;
	}
}
