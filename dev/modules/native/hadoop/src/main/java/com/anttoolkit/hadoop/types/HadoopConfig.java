package com.anttoolkit.hadoop.types;

import java.util.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.types.*;

public class HadoopConfig
		extends Path
{
	public static final String HADOOP_DEFAULT_FS_PROP = "fs.defaultFS";
	public static final String HADOOP_JOB_UGI_PROP = "hadoop.job.ugi";

	private String hadoopUser;
	private String principal;
	private String keytab;
	private List<Property> properties = new LinkedList<Property>();

	public HadoopConfig(Project project)
	{
    	super(project);
 	}

	public HadoopConfig(Project project, String path)
	{
		super(project, path);
	}

	public void setPrincipal(String principal)
	{
		this.principal = principal;
	}

	public String getPrincipal()
	{
		return principal;
	}

	public void setKeytab(String keytab)
	{
		this.keytab = keytab;
	}

	public String getKeytab()
	{
		return keytab;
	}

	public void setHadoopUser(String user)
	{
		this.hadoopUser = user == null || user.trim().isEmpty() ? null : user.trim();
	}

	public String getHadoopUser()
	{
		return hadoopUser;
	}

	public void addConfiguredProperty(Property prop)
	{
		if (!properties.contains(prop))
		{
			properties.add(prop);
		}
	}

	public List<Property> getProperties()
	{
		return Collections.unmodifiableList(properties);
	}

	public String getConfiguredPropertyValue(String name)
	{
		for (Property prop : properties)
		{
			if (prop.getName().equals(name))
			{
				return prop.getValue();
			}
		}

		return null;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (!(obj instanceof HadoopConfig))
		{
			return false;
		}

		HadoopConfig conf = (HadoopConfig)obj;

		return compareUsers(conf) &&
				compareProperties(conf) &&
				compareResources(conf);
	}

	private boolean compareUsers(HadoopConfig conf)
	{
		if (getHadoopUser() != null && conf.getHadoopUser() != null)
		{
			return getHadoopUser().equals(conf.getHadoopUser());
		}

		return getHadoopUser() == null && conf.getHadoopUser() == null;
	}

	private boolean compareProperties(HadoopConfig conf)
	{
		if (conf.getProperties().size() != getProperties().size())
		{
			return false;
		}

		for (Property prop : getProperties())
		{
			if (!conf.getProperties().contains(prop))
			{
				return false;
			}
		}

		return true;
	}

	private boolean compareResources(HadoopConfig conf)
	{
		if (conf.size() != this.size())
		{
			return false;
		}

		for (Resource res : this)
		{
			boolean contains = false;

			for (Resource _res : conf)
			{
				if (_res.equals(res))
				{
					contains = true;
					break;
				}
			}

			if (!contains)
			{
				return false;
			}
		}

		return true;
	}
}
