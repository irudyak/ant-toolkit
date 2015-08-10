package com.anttoolkit.zookeeper.tasks;

import java.io.*;

import org.apache.tools.ant.*;
import org.apache.zookeeper.data.*;

public class GetNodeDataTask extends GenericZookeeperTask
{
	private String node;
	private String dataProperty;
	private String dataRef;
	private String czxidProperty;
	private String mzxidProperty;
	private String ctimeProperty;
	private String mtimeProperty;
	private String versionProperty;
	private String cversionProperty;
	private String aversionProperty;
	private String ephemeralOwnerProperty;
	private String dataLengthProperty;
	private String numChildrenProperty;
	private String pzxidProperty;

	public void setNode(String node)
	{
		this.node = node;
	}

	public void setDataProperty(String dataProperty)
	{
		this.dataProperty = dataProperty;
	}

	public void setDataReference(String dataRef)
	{
		this.dataRef = dataRef;
	}

	public void setCzxidProperty(String property)
	{
		czxidProperty = property;
	}

	public void setMzxidProperty(String property)
	{
		mzxidProperty = property;
	}

	public void setCtimeProperty(String property)
	{
		ctimeProperty = property;
	}

	public void setMtimeProperty(String property)
	{
		mtimeProperty = property;
	}

	public void setVersionProperty(String property)
	{
		versionProperty = property;
	}

	public void setCversionProperty(String property)
	{
		cversionProperty = property;
	}

	public void setAversionProperty(String property)
	{
		aversionProperty = property;
	}

	public void setEphemeralOwnerProperty(String property)
	{
		ephemeralOwnerProperty = property;
	}

	public void setDataLengthProperty(String property)
	{
		dataLengthProperty = property;
	}

	public void setNumChildrenProperty(String property)
	{
		numChildrenProperty = property;
	}

	public void setPzxidProperty(String property)
	{
		pzxidProperty = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		Stat stat = new Stat();
		byte[] data = getZookeeperSession().getData(node, stat);

		if (dataProperty != null)
		{
			try
			{
				this.setPropertyThreadSafe(dataProperty, data == null ? null : new String(data, "UTF-8"));
			}
			catch (UnsupportedEncodingException e)
			{
				throw new BuildException("Failed to convert ZooKeeper node '" + node + "' data to UFT-8 string");
			}
		}

		if (dataRef != null)
		{
			this.setReference(dataRef, data);
		}

		if (czxidProperty != null)
		{
			this.setPropertyThreadSafe(czxidProperty, Long.toString(stat.getCzxid()));
		}

		if (mzxidProperty != null)
		{
			this.setPropertyThreadSafe(mzxidProperty, Long.toString(stat.getMzxid()));
		}

		if (ctimeProperty != null)
		{
			this.setPropertyThreadSafe(ctimeProperty, Long.toString(stat.getCtime()));
		}

		if (mtimeProperty != null)
		{
			this.setPropertyThreadSafe(mtimeProperty, Long.toString(stat.getMtime()));
		}

		if (versionProperty != null)
		{
			this.setPropertyThreadSafe(versionProperty, Integer.toString(stat.getVersion()));
		}

		if (cversionProperty != null)
		{
			this.setPropertyThreadSafe(cversionProperty, Integer.toString(stat.getCversion()));
		}

		if (aversionProperty != null)
		{
			this.setPropertyThreadSafe(aversionProperty, Integer.toString(stat.getAversion()));
		}

		if (ephemeralOwnerProperty != null)
		{
			this.setPropertyThreadSafe(ephemeralOwnerProperty, Long.toString(stat.getEphemeralOwner()));
		}

		if (dataLengthProperty != null)
		{
			this.setPropertyThreadSafe(dataLengthProperty, Integer.toString(stat.getDataLength()));
		}

		if (numChildrenProperty != null)
		{
			this.setPropertyThreadSafe(numChildrenProperty, Integer.toString(stat.getNumChildren()));
		}

		if (pzxidProperty != null)
		{
			this.setPropertyThreadSafe(pzxidProperty, Long.toString(stat.getPzxid()));
		}
	}

	@Override
	protected void validate()
	{
		if (node == null || node.trim().isEmpty())
		{
			throw new BuildException("Node name should be specified");
		}
	}
}
