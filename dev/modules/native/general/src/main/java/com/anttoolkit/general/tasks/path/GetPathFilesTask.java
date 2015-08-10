package com.anttoolkit.general.tasks.path;

import com.anttoolkit.general.tasks.collections.array.util.ArrayManager;
import org.apache.tools.ant.*;
import org.apache.tools.ant.types.*;
import org.apache.tools.ant.types.resources.*;

import com.anttoolkit.general.tasks.*;

public class GetPathFilesTask extends GenericTask
{
	private String array;
	private Path path;

	public void setArray(String array)
	{
		this.array = array;
	}

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

	@Override
	public void doWork() throws BuildException
	{
		for (Resource res : path)
		{
			if (res instanceof FileResource &&
				!ArrayManager.contains(array, this.getFileFullPath(res.toString())))
			{
				ArrayManager.add(array, res.toString());
			}
		}
	}

	@Override
	protected void validate()
	{
		if (path == null)
		{
			throw new BuildException("Path reference should be specified");
		}

		if (array == null || array.trim().isEmpty())
		{
			throw new BuildException("Array name should be specified");
		}

		if (!ArrayManager.exists(array))
		{
			throw new BuildException("Specified array '" + array + "' doesn't exist");
		}
	}
}
