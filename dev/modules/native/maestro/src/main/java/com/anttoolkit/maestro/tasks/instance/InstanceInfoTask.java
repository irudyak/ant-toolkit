package com.anttoolkit.maestro.tasks.instance;

import java.util.*;

import org.apache.tools.ant.*;

import com.maestro.xml.*;

import com.anttoolkit.maestro.tasks.*;
import com.anttoolkit.maestro.common.*;

@Action("describe-instances")
public class InstanceInfoTask extends CliCommandTask
{
	@Param
	private String instance;

	private String xmlItemReference;

	public void setInstance(String instance)
	{
		this.instance = instance;
	}

	public void setXmlItemReference(String ref)
	{
		this.xmlItemReference = ref;
	}

	@Override
	public void doWork() throws BuildException
	{
		XMLElement element = executeCommand();

		List<XMLElement> items = element.Items();
		for (XMLElement item : items)
		{

			XMLAttribute idAttr = item.getAttribute("instanceID");

			if (idAttr != null &&
				instance.equals(idAttr.getValue()) &&
				xmlItemReference != null)
			{
				this.setReference(xmlItemReference, item);
				return;
			}
		}

		throw new InstanceNotFoundException("There is no instance with id: " + instance);
	}

	@Override
	protected void validate()
	{
		if (instance == null)
		{
			throw new BuildException("Instance should be specified");
		}

		if (xmlItemReference == null)
		{
			throw new BuildException("Xml item reference should be specified");
		}
	}
}
