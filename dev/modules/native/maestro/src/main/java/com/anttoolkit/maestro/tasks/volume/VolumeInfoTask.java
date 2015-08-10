package com.anttoolkit.maestro.tasks.volume;

import java.util.*;

import org.apache.tools.ant.*;

import com.maestro.xml.*;

import com.anttoolkit.maestro.common.*;

@Action("describe-volumes")
public class VolumeInfoTask extends VolumeTask
{
	@Param("volumes")
	String volume;

	private String xmlItemReference;

	public void setVolume(String volume)
	{
		this.volume = volume;
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

			XMLAttribute idAttr = item.getAttribute("id");

			if (idAttr != null &&
				volume.equals(idAttr.getValue()) &&
				xmlItemReference != null)
			{
				this.setReference(xmlItemReference, item);
				return;
			}
		}

		throw new VolumeNotFoundException("There is no volume with id: " + volume);
	}

	@Override
	protected  void validate()
	{
		if (volume == null)
		{
			throw new BuildException("Volume should be specified");
		}

		if (xmlItemReference == null)
		{
			throw new BuildException("Xml item reference should be specified");
		}
	}
}
