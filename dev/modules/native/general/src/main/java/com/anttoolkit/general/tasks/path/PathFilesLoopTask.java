package com.anttoolkit.general.tasks.path;

import java.util.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.types.*;
import org.apache.tools.ant.types.resources.*;

import com.anttoolkit.general.tasks.*;

public class PathFilesLoopTask
		extends GenericTask
		implements TaskContainer
{
	private List<Task> tasks = new LinkedList<Task>();
	private Path path;
	private String property;

	public void setPathRef(String ref)
	{
		if (ref == null)
		{
			throw new BuildException("Path reference can't be null");
		}

		Object obj = this.getReference(ref);
		if (!(obj instanceof Path))
		{
			throw new BuildException("Path reference '" + ref +"' doesn't contain Path object");
		}

		path = (Path)obj;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		for (Resource res : path)
		{
			if (res instanceof FileResource)
			{
				this.setPropertyThreadSafe(property, this.getFileFullPath(res.toString()));
			}

			for (Task task : tasks)
			{
				task.perform();
			}
		}
	}

	@Override
	public void addTask(Task task)
	{
		tasks.add(task);
	}

	@Override
	protected void validate()
	{
		if (path == null)
		{
			throw new BuildException("Path reference should be specified");
		}

		if (property == null || property.trim().isEmpty())
		{
			throw new BuildException("Property should be specified");
		}
	}
}
