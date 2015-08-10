package com.anttoolkit.maestro.tasks.properties;

import java.util.*;

import com.anttoolkit.maestro.tasks.properties.util.PropertiesHolder;
import org.apache.tools.ant.*;

import com.maestro.xml.*;

import com.anttoolkit.maestro.common.*;

@Action("describe-properties")
public class GetPropertiesTask extends PropertyTask
{
	@Param(value = "property-name-list", sorted = true)
	private Set<String> properties = new HashSet<String>();

	private Map<String, PropertiesHolder> instancesMap = new HashMap<String, PropertiesHolder>();
	private Map<String, PropertiesHolder> volumesMap = new HashMap<String, PropertiesHolder>();

	public void setProperties(String properties)
	{
		this.populateParamValues(properties, this.properties);
	}

	public void addConfiguredInstance(PropertiesHolder instance)
	{
		instance.validate("instance");
		instancesMap.put(instance.getId(), instance);
		addInstance(instance.getId());
	}

	public void addConfiguredVolume(PropertiesHolder volume)
	{
		volume.validate("volume");
		volumesMap.put(volume.getId(), volume);
		addVolume(volume.getId());
	}

	@Override
	public void doWork() throws BuildException
	{
		XMLElement element = executeCommand();

		List<XMLElement> items = element.Items();
		for (XMLElement item : items)
		{
			XMLAttribute idAttr = item.getAttribute("resource");
			XMLAttribute typeAttr = item.getAttribute("resourceType");
			XMLAttribute nameAttr = item.getAttribute("name");
			XMLAttribute valueAttr = item.getAttribute("value");

			String id = idAttr == null ? null : idAttr.getValue();
			String type = typeAttr == null ? null : typeAttr.getValue();
			String name = nameAttr == null ? null : nameAttr.getValue();
			String value = valueAttr == null ? null : valueAttr.getValue();

			if (id == null || type == null || name == null || value == null)
			{
				continue;
			}

			if ("instance".equals(type))
			{
				instancesMap.get(id).addProperty(name, value);
			}
			else if ("volume".equals(type))
			{
				volumesMap.get(id).addProperty(name, value);
			}
		}
	}
}
