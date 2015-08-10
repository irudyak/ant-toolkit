package com.anttoolkit.zookeeper.types;

import java.nio.charset.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class AuthInfo
{
	private String scheme;
	private byte[] info;
	private String infoRef;

	public void setScheme(String scheme)
	{
		this.scheme = scheme;
	}

	public void setInfo(String info)
	{
		this.info = info.getBytes(Charset.forName("UTF-8"));
	}

	public void setInfoRef(String ref)
	{
		this.infoRef = ref;
	}

	public String getScheme()
	{
		if (scheme == null || scheme.trim().isEmpty())
		{
			throw new BuildException("Authentication scheme doesn't specified");
		}

		return scheme;
	}

	public byte[] getInfo()
	{
		if (info == null)
		{
			throw new BuildException("There is no authentication info specified");
		}

		return info;
	}

	public void init(GenericTask task)
	{
		if (info != null)
		{
			return;
		}

		if (infoRef == null)
		{
			throw new BuildException("There is no authentication info specified");
		}

		if (!task.checkReferenceExists(infoRef))
		{
			throw new BuildException("Specified reference '" + infoRef + "' for authentication info doesn't exist");
		}

		Object obj = task.getReference(infoRef);
		if (!(obj instanceof byte[]))
		{
			throw new BuildException("Specified reference '" + infoRef + "' doesn't contain object of type byte[]");
		}

		info = (byte[])obj;
	}
}
