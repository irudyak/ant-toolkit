package com.anttoolkit.general.common;

import java.io.*;
import java.util.*;
import java.util.jar.*;
import java.util.regex.*;

import org.apache.tools.ant.*;

public class JarHelper
{
	public static String getMainClass(String jarFile)
	{
		JarFile _jarFile = null;

		try
		{
			_jarFile = new JarFile(jarFile);
			Manifest manifest = _jarFile.getManifest();

			String mainClassName = null;

			if (manifest != null)
			{
				mainClassName = manifest.getMainAttributes().getValue("Main-Class");
			}

			return mainClassName == null || mainClassName.trim().isEmpty() ? null : mainClassName;
		}
		catch(IOException e)
		{
			throw new BuildException("Error opening jar: " + jarFile, e);
		}
		finally
		{
			if (_jarFile != null)
			{
				try
				{
					_jarFile.close();
				}
				catch (IOException e) {}
			}
		}
	}

	public static void unJar(String jarFile, String toDir)
	{
		unJar(jarFile, toDir, ".*");
	}

	public static void unJar(String jarFile, String toDir, String unpackRegex)
	{
		JarFile jar;

		try
		{
			jar = new JarFile(jarFile);
		}
		catch (IOException e)
		{
			throw new BuildException("Error opening jar: " + jarFile, e);
		}

		Pattern filter = Pattern.compile(unpackRegex);
		try
		{
			Enumeration<JarEntry> entries = jar.entries();

			while (entries.hasMoreElements())
			{
				final JarEntry entry = entries.nextElement();

				if (entry.isDirectory() || !filter.matcher(entry.getName()).matches())
				{
					continue;
				}

				InputStream in = jar.getInputStream(entry);

				try
				{
					File file = new File(toDir, entry.getName());

					File dir = file.getParentFile();

					if (!dir.mkdirs() && !dir.isDirectory())
					{
						throw new IOException("Mkdirs failed to create " + dir.toString());
					}

					OutputStream out = new FileOutputStream(file);

					try
					{
						StreamHelper.copyBytes(in, out, 8192);
					}
					finally
					{
						out.close();
					}
				}
				finally
				{
					try
					{
						in.close();
					}
					catch (IOException e) {}
				}
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to unzip jar: " + jarFile, e);
		}
		finally
		{
			try
			{
				jar.close();
			}
			catch (Throwable e) {}
		}
	}
}
