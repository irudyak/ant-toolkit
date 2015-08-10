package com.anttoolkit.general.tasks.classload;

import java.util.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.types.*;

import com.anttoolkit.general.tasks.*;

public class SetThreadClassLoaderTask
	extends GenericTask
	implements TaskContainer
{
	private boolean permanent = false;
	private List<Task> tasks = new LinkedList<Task>();
	private ResourceClassLoader resourceClassLoader;
	private Path classpath;

	public void setPermanent(boolean permanent)
	{
		this.permanent = permanent;
	}

	public void setClasspathRef(String ref) throws BuildException
	{
		Object obj = this.getReference(ref);
		if (obj == null || !(obj instanceof Path))
		{
			throw new BuildException("Reference '" + ref + "' doesn't contain valid Path object");
		}

		classpath = (Path)obj;
 	}

	@Override
	public void doWork() throws BuildException
	{
		for (Task task : tasks)
		{
			task.perform();
		}
	}

	@Override
	protected void preProcessing()
	{
		resourceClassLoader = new ResourceClassLoader(classpath, null);
		resourceClassLoader.substituteThreadClassLoader();
	}

	@Override
	protected void postProcessing()
	{
		if (resourceClassLoader != null && !permanent)
		{
			resourceClassLoader.restoreThreadClassLoader();
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
		if (classpath == null || classpath.size() == 0)
		{
			throw new BuildException("Classpath should be specified");
		}
	}
}
