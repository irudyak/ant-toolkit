package com.anttoolkit.general.tasks.build.util;

import org.apache.tools.ant.BuildException;

public class BuildVersion implements Comparable
{
	public final int major;
	public final int minor;
	public final int patch;

	public static final BuildVersion INITIAL = new BuildVersion(0, 0, 0);

	public static BuildVersion parse(String version)
	{
		if (version == null || version.trim().isEmpty())
		{
			throw new IllegalArgumentException("Build version couldn't be specified as empty string");
		}

		int major = -1;
		int minor = 0;
		int patch = 0;

		String[] chunks = version.split("\\.", -1);
		if (chunks.length == 0 || chunks.length > 3)
		{
			throw new BuildException("Incorrect build version number specified: " + version);
		}

		try
		{
			major = Integer.parseInt(chunks[0]);
		}
		catch (NumberFormatException e)
		{
			throw new BuildException("Incorrect build version number specified: " + version);
		}

		if (chunks.length > 1)
		{
			try
			{
				minor = Integer.parseInt(chunks[1]);
			}
			catch (NumberFormatException e)
			{
				throw new BuildException("Incorrect build version number specified: " + version);
			}
		}

		if (chunks.length > 2)
		{
			try
			{
				patch = Integer.parseInt(chunks[2]);
			}
			catch (NumberFormatException e)
			{
				throw new BuildException("Incorrect build version number specified: " + version);
			}
		}

		if (major == 0 && minor == 0 && patch == 0)
		{
			return INITIAL;
		}

		if (major <= 0)
		{
			throw new BuildException("Incorrect build version number specified: " + version);
		}

		if (minor < 0)
		{
			throw new BuildException("Incorrect build version number specified: " + version);
		}

		if (patch < 0)
		{
			throw new BuildException("Incorrect build version number specified: " + version);
		}

		return new BuildVersion(major, minor, patch);
	}

	public BuildVersion(int major, int minor, int patch)
	{
		this.major = major;
		this.minor = minor;
		this.patch = patch;

		if (major < 0)
		{
			throw new BuildException("Incorrect build version number specified: " + major + "." + minor + "." + patch);
		}

		if (minor < 0)
		{
			throw new BuildException("Incorrect build version number specified: " + major + "." + minor + "." + patch);
		}

		if (patch < 0)
		{
			throw new BuildException("Incorrect build version number specified: " + major + "." + minor + "." + patch);
		}
	}

	@Override
	public boolean equals(Object obj)
	{
		return obj != null &&
				obj instanceof BuildVersion &&
				major == ((BuildVersion)obj).major &&
				minor == ((BuildVersion)obj).minor &&
				patch == ((BuildVersion)obj).patch;
	}

	@Override
	public int hashCode()
	{
		return major * 100 + minor * 10 + patch;
	}

	@Override
	public String toString()
	{
		return major + "." + minor + "." + patch;
	}

	@Override
	public int compareTo(Object obj)
	{
		if (obj == null)
		{
			throw new IllegalArgumentException("Can't compare build version object to null object");
		}

		if (!(obj instanceof BuildVersion))
		{
			throw new IllegalArgumentException("Can't compare build version object to not build version object");
		}

		if (major < ((BuildVersion)obj).major)
		{
			return -1;
		}

		if (major > ((BuildVersion)obj).major)
		{
			return 1;
		}

		if (minor < ((BuildVersion)obj).minor)
		{
			return -1;
		}

		if (minor > ((BuildVersion)obj).minor)
		{
			return 1;
		}

		if (patch < ((BuildVersion)obj).patch)
		{
			return -1;
		}

		if (patch > ((BuildVersion)obj).patch)
		{
			return 1;
		}

		return 0;
	}
}
