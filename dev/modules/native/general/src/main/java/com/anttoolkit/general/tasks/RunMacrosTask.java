package com.anttoolkit.general.tasks;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import org.apache.tools.ant.taskdefs.MacroInstance;

public class RunMacrosTask extends GenericTask
{
	private String macros;
	private List<NameValueHolder> attributes = new LinkedList<NameValueHolder>();

	public void setMacros(String macros)
	{
		this.macros = macros;
	}

	public void addConfiguredAttribute(NameValueHolder attr)
	{
		if (attr.getName() == null || attr.getName().trim().isEmpty())
		{
			throw new BuildException("Attribute name can't be empty");
		}

		attributes.add(attr);
	}

	@Override
	public void doWork() throws BuildException
	{
		MacroInstance instance = getMacroInstance();
		if (instance == null)
		{
			throw new BuildException("There is no macros with '" + macros + "' name in the project");
		}

		for (NameValueHolder attr : attributes)
		{
			instance.setDynamicAttribute(attr.getName().toLowerCase(), attr.getValue());
		}

		instance.perform();
	}

	@Override
	protected void validate()
	{
		if (macros == null || macros.trim().isEmpty())
		{
			throw new BuildException("Macros name should be specified");
		}
	}

	private MacroInstance getMacroInstance()
	{
		ComponentHelper helper = ComponentHelper.getComponentHelper(this.getProject());

		AntTypeDefinition typeDef = helper.getAntTypeTable().get(macros);
		if (typeDef == null)
		{
			return null;
		}

		Object obj = typeDef.create(this.getProject());

		return obj instanceof MacroInstance ? (MacroInstance)obj : null;
	}
}
