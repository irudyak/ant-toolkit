package com.anttoolkit.documentum.tasks;

import com.anttoolkit.documentum.common.*;

import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import java.util.*;

public class FlushCachesTask
		extends GenericDocbaseTask
{
	public void doWork()
			throws BuildException
	{
		getSession().flushCaches();
	}
}
