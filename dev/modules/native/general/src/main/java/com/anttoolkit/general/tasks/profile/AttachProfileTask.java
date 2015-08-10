package com.anttoolkit.general.tasks.profile;

import java.io.*;
import java.net.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.property.ResolvePropertyMap;
import org.apache.tools.ant.types.*;

import com.anttoolkit.general.tasks.*;

public class AttachProfileTask extends GenericTask
{
	private static final String PROFILE_RESOLVED_INDICATOR = "anttoolkit.profile.resolved";

	private static final String FILE_RESOURCE_PREFIX = "file:/";
	private static final String JAR_RESOURCE_PREFIX = "jar:file:/";

	private static final String PROFILE_PROPERTY = "profile";
	private static final String PROFILE_DIR_PROPERTY = "profile.dir";
	private static final String PROFILE_JAR_PROPERTY = "profile.jar";

	private List<ProfileResource> resources = new LinkedList<ProfileResource>();

	private String profile;
	private String profileDir;
	private String profileJar;

	public void addConfiguredResource(ProfileResource resource)
	{
		resources.add(resource);
	}

	@Override
	public void doWork() throws BuildException
	{
		String indicator = getProject().getProperty(PROFILE_RESOLVED_INDICATOR);
		if ("true".equals(indicator))
		{
			return;
		}

		Path classpath = new Path(getProject());
		classpath = classpath.concatSystemClasspath("only");

		AntClassLoader loader = null;
		try
		{
			loader = AntClassLoader.newAntClassLoader(getProject().getCoreLoader(), getProject(), classpath, false);

			for (ProfileResource resource : resources)
			{
				attachResource(loader, resource);
			}
		}
		finally
		{
			if (loader != null) {
				loader.cleanup();
			}
		}

		if (profile == null)
		{
			throw new BuildException("Failed to detect profile");
		}

		log("");
		log("-------------------------------------------------------------------------------------------");
		log("--- Profile: " + profile);

		this.setPropertyThreadSafe(PROFILE_PROPERTY, profile);

		if (profileJar != null)
		{
			log("--- Profile jar: " + profileJar);
			this.setPropertyThreadSafe(PROFILE_JAR_PROPERTY, profileJar);
		}

		if (profileDir != null)
		{
			log("--- Profile dir: " + profileDir);
			this.setPropertyThreadSafe(PROFILE_DIR_PROPERTY, profileDir);
		}

		log("-------------------------------------------------------------------------------------------");
		log("");

		getProject().setProperty(PROFILE_RESOLVED_INDICATOR, "true");
	}

	private void attachResource(AntClassLoader loader, ProfileResource resource)
	{
		String resourceName = resource.name();

		if (resourceName.startsWith("/"))
		{
			resourceName = resourceName.substring(1);
		}

		URL url;

		try
		{
			url = loader.getResource(resourceName);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to find profile resource: " + resource.name(), e);
		}

		if (url == null)
		{
			if (!resource.required())
			{
				log("Couldn't find profile resource: " + resource.name(), Project.MSG_WARN);
				return;
			}

			throw new BuildException("Failed to find profile resource: " + resource.name());
		}

		String loc = url.toExternalForm();

		boolean isJarResource = loc.startsWith(JAR_RESOURCE_PREFIX);
		boolean isFileResource = loc.startsWith(FILE_RESOURCE_PREFIX);

		if (isFileResource)
		{
			profileJar = null;
			profileDir = loc.replace(FILE_RESOURCE_PREFIX, "");
			profileDir = profileDir.replace("/" + resource.name(), "");

			int index = profileDir.lastIndexOf("/");
			if (index <= 0)
			{
				throw new BuildException("Failed to identify profile resource directory '" + loc + "' for resource '" + resource.name() + "'");
			}

			profile = profileDir.substring(index + 1).trim();
			if (profile.isEmpty())
			{
				throw new BuildException("Failed to identify profile name by resource '" + resource.name() + "', location: " + loc);
			}
		}
		else if (isJarResource)
		{
			profileDir = null;
			profileJar = loc.replace(JAR_RESOURCE_PREFIX, "");

			int index = profileJar.lastIndexOf("!");
			if (index <= 0)
			{
				throw new BuildException("Failed to identify profile resource jar '" + loc + "' for resource '" + resource.name() + "'");
			}

			profileJar = profileJar.substring(0, index);

			index = profileJar.lastIndexOf("/");
			if (index <= 0)
			{
				throw new BuildException("Failed to identify profile name by resource '" + resource.name() + "', location: " + loc);
			}

			profileDir = profileJar.substring(0, index);

			profile = profileJar.substring(index + 1, profileJar.length() -4).trim();
			if (profile.isEmpty())
			{
				throw new BuildException("Failed to identify profile name by resource '" + resource.name() + "', location: " + loc);
			}

			profileJar = profileJar.replace(profileDir + "/", "");
		}
		else
		{
			throw new BuildException("Failed to identify profile resource type '" + loc + "' for resource '" + resource.name() + "'");
		}

		initProjectProperties(resource);
	}

	private void initProjectProperties(ProfileResource resource)
	{
		Properties props = new Properties();
		InputStream in = null;

		try
		{
			ClassLoader cL = this.getClass().getClassLoader();

			in = cL != null ?
					cL.getResourceAsStream(resource.name()) :
					ClassLoader.getSystemResourceAsStream(resource.name());


			if (in == null) {
				throw new BuildException("Unable to find profile resource: " + resource.name());
			}

			if (resource.name().toLowerCase().endsWith(".xml"))
			{
				props.loadFromXML(in);
			}
			else
			{
				props.load(in);
			}

			setProperties(props);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to load profile resource: " + resource.name(), e);
		}
		finally
		{
			if (in != null)
			{
				try
				{
					in.close();
				}
				catch (IOException e) { }
			}
		}
	}

	private void setProperties(Properties props)
	{
		Map map = new HashMap(props);

		resolveAllProperties(map);

		for (Object obj : map.keySet())
		{
			if (obj instanceof String)
			{
				String propertyName = (String) obj;
				this.setPropertyThreadSafe(propertyName, (String) map.get(obj));
			}
		}
	}

	private void resolveAllProperties(Map props)
			throws BuildException
	{
		PropertyHelper helper = PropertyHelper.getPropertyHelper(getProject());
		new ResolvePropertyMap(getProject(), helper, helper.getExpanders()).resolveAllProperties(props, null, false);
	}

}
