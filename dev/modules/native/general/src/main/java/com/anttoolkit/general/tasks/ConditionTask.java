package com.anttoolkit.general.tasks;

import org.apache.tools.ant.*;
import org.apache.tools.ant.taskdefs.condition.*;

public class ConditionTask
		extends org.apache.tools.ant.taskdefs.condition.ConditionBase
{
	private String property = null;
	private Object value = "true";
	private Object alternative = null;

	public ConditionTask()
	{
		super("generalCondition");
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void setValue(Object value)
	{
		this.value = value;
	}

	public void setValue(String v)
	{
		setValue((Object) v);
	}

	public void setElse(Object alt)
	{
		alternative = alt;
	}

	public void setElse(String e)
	{
		setElse((Object) e);
	}

	public void execute() throws BuildException
	{
		if (countConditions() > 1)
		{
			throw new BuildException("You must not nest more than one condition into <"
					+ getTaskName() + ">");
		}

		if (countConditions() < 1)
		{
			throw new BuildException("You must nest a condition into <" + getTaskName() + ">");
		}

		if (property == null)
		{
			throw new BuildException("The property attribute is required.");
		}

		Condition c = (Condition) getConditions().nextElement();

		if (c.eval())
		{
			log("Condition true; setting " + property + " to " + value, Project.MSG_DEBUG);
			GenericTask.setPropertyThreadSafe(this.getProject(), property, value.toString());
		}
		else if (alternative != null)
		{
			log("Condition false; setting " + property + " to " + alternative, Project.MSG_DEBUG);
			GenericTask.setPropertyThreadSafe(this.getProject(), property, alternative.toString());
		}
		else
		{
			log("Condition false; not setting " + property, Project.MSG_DEBUG);
		}
	}
}
