package com.anttoolkit.maestro.tasks.utils;

import org.apache.tools.ant.*;

import com.maestro.xml.*;

import com.anttoolkit.general.tasks.*;

public class GetXmlItemAttributeTask extends GenericTask
{
	private String reference;
	private String attribute;
	private String property;

	public void setReference(String reference)
	{
		this.reference = reference;
	}

	public void setAttribute(String attribute)
	{
		this.attribute = attribute;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		Object obj = getReference(reference);
		if (!(obj instanceof XMLElement))
		{
			throw new BuildException("Reference '" + reference + "' doesn't contain XMLElement");
		}

		XMLAttribute attr = ((XMLElement)obj).getAttribute(attribute);

		this.setPropertyThreadSafe(property, attr == null ? "" : attr.getValue());
	}

	@Override
	protected void validate()
	{
		if (reference == null || reference.trim().isEmpty())
		{
			throw new BuildException("Xml item reference should be specified");
		}

		if (attribute == null || attribute.trim().isEmpty())
		{
			throw new BuildException("Xml item attribute should be specified");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property to store xml item attribute value should be specified");
		}
	}
}
