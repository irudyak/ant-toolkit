package com.anttoolkit.maestro.tasks.utils;

import java.util.*;

import com.maestro.xml.*;
import org.apache.tools.ant.*;

import com.anttoolkit.maestro.tasks.*;

public class XmlItemLoopTask extends CliCommandTask implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();

	private String xmlItemReference;

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
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
			if (xmlItemReference != null)
			{
				this.setReference(xmlItemReference, item);
			}

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}

	@Override
	protected void validate()
	{
		if (xmlItemReference == null)
		{
			throw new BuildException("Xml item reference should be specified");
		}
	}
}
