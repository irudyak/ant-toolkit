package com.anttoolkit.zookeeper.tasks;

import java.nio.charset.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.common.*;
import com.anttoolkit.zookeeper.common.*;

public abstract class GenericZookeeperTask extends GenericTask
{
	public static final String HOST_INFO_STR = SystemHelper.hostName + "/" + SystemHelper.hostIp;
	public static final byte[] HOST_INFO = HOST_INFO_STR.getBytes(Charset.forName("UTF-8"));

	protected ZookeeperSession getZookeeperSession()
	{
		ZookeeperSession session = ZookeeperSessionManager.getZookeeperSession();
		if (session == null)
		{
			throw new BuildException("There is no ZooKeeper session was previously established");
		}

		return session;
	}
}
