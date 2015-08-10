package com.anttoolkit.sql.tasks;

import java.sql.*;
import java.util.*;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;
import org.apache.tools.ant.taskdefs.*;
import org.apache.tools.ant.types.*;

import com.anttoolkit.sql.common.*;
import com.anttoolkit.sql.tasks.build.util.*;

public class SqlTaskContainer
		extends GenericTask
		implements TaskContainer
{
	private static ThreadLocal<Map<String, Driver>> drivers = new ThreadLocal<Map<String, Driver>>()
	{
		protected Map<String, Driver> initialValue()
		{
			return new HashMap<String, Driver>();
		}
	};

	private String user;
	private String password;
	private String url;
	private String driver;
	private Path classpath;
	private List<Property> connectionProperties = new LinkedList<Property>();
	private List<Task> tasks = new LinkedList<Task>();

	public void setUser(String user)
	{
    	this.user = user;
 	}

	public void setPassword(String password)
	{
		this.password = password;
	}

	public void setUrl(String url)
	{
		this.url = url;
	}

	public void setDriver(String driver)
	{
		this.driver = driver.trim();
	}

	public void setClasspath(Path classpath)
	{
		this.classpath = classpath;
	}

	public Path createClasspath()
	{
		if (classpath == null)
		{
			classpath = new Path(getProject());
		}

	 	return classpath.createPath();
	}

	public void setClasspathRef(Reference r)
	{
		createClasspath().setRefid(r);
	}

	public void addConnectionProperty(Property var)
	{
		connectionProperties.add(var);
	}

	public void doWork() throws BuildException
	{
		SqlSessionManager.setCurrentSessionContext(getLoginInfo());

		try
		{
			for (Task task : tasks)
			{
				task.perform();
			}
		}
		catch (Throwable e)
		{
			SqlSessionManager.getSession().rollbackTransaction();

			if (e instanceof BuildException)
			{
				throw (BuildException)e;
			}

			throw new BuildException("Exception occured", e);
		}
		finally
		{
			SqlSessionManager.releaseCurrentSessionContext();
		}
	}

	public void addTask(Task task)
	{
		tasks.add(task);
	}

	protected void validate()
	{
		if (user == null)
		{
			throw new BuildException("User name is not specified");
		}

		if (password == null)
		{
			throw new BuildException("Password name is not specified");
		}

		if (url == null)
		{
			throw new BuildException("Connection URL is not specified");
		}

		if (driver == null)
		{
			throw new BuildException("Driver class is not specified");
		}
	}

	private Driver getDriver()
	{
		if (driver == null)
		{
      		throw new BuildException("Driver attribute is not specified");
  		}

		Driver driverInstance = drivers.get().get(driver);
		if (driverInstance != null)
		{
			return driverInstance;
		}

  		try
		{
// TODO: refactoring needed
/*
      		Class clazz;
      		if (classpath != null)
			{
            	AntClassLoader loader = getProject().createClassLoader(classpath);
				clazz = loader.loadClass(driver);
      		}
			else
			{
          		clazz = Class.forName(driver);
			}
*/

			Class clazz = Class.forName(driver);

			return (Driver)clazz.newInstance();
  		}
		catch (ClassNotFoundException e)
		{
      		throw new BuildException("Class Not Found: JDBC driver " + driver + " could not be loaded", e, getLocation());
		}
		catch (IllegalAccessException e)
		{
			throw new BuildException("Illegal Access: JDBC driver " + driver + " could not be loaded", e, getLocation());
		}
		catch (InstantiationException e)
		{
			throw new BuildException("Instantiation Exception: JDBC driver " + driver + " could not be loaded", e, getLocation());
		}
	}

	private Properties getConnectionProperties()
	{
		Properties props = new Properties();
		props.put("user", user);
		props.put("password", password);
		props.put("autocommit", "false");

		for (Property p : connectionProperties)
		{
			String name = p.getName();
			String value = p.getValue();
			if (name != null && value != null)
			{
				props.put(name, value);
			}
		}

		return props;
	}

	protected LoginInfo getLoginInfo()
	{
		return new LoginInfo(url, user, password, getDriver(), getConnectionProperties());
	}

}
