package com.anttoolkit.pig.tasks;

import java.io.*;
import java.util.*;
import java.util.regex.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.types.*;
import org.apache.tools.ant.types.resources.*;

import org.apache.pig.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;

public abstract class GenericPigScriptTask
		extends GenericHadoopTask
{
	private static final String REGISTER_KEYWORD = "REGISTER";
	private static final String USING_KEYWORD = "USING";
	private static final String IMPORT_KEYWORD = "IMPORT";

	private static final Pattern JAVA_UDF_PATTERN = Pattern.compile("(?i)" + REGISTER_KEYWORD + "[\\s]+[a-zA-Z0-9-_:\\.\\\\/]+\\.jar[\\s]*;");
	private static final Pattern SCRIPT_UDF_PATTERN = Pattern.compile("(?i)" + REGISTER_KEYWORD + "[\\s]+'[a-zA-Z0-9-_:\\.\\\\/]+'[\\s]" + USING_KEYWORD);
	private static final Pattern IMPORT_PATTERN = Pattern.compile("(?i)" + IMPORT_KEYWORD + "[\\s]+'[\\s]*[a-zA-Z0-9-_:\\.\\\\/]+\\.[a-zA-Z]+[\\s]*'[\\s]*;");

	private String mode = "mapreduce";
	private String script;
	private String scriptFile;
	private Map<String, String> params;
	private List<String> paramsFiles;

	private PigServer server;

	public void setMode(String mode)
	{
		if (mode == null || (!"local".equals(mode.trim().toLowerCase()) && !"mapreduce".equals(mode.trim().toLowerCase())))
		{
			throw new BuildException("Incorrect Pig execution mode specified: " + mode);
		}

		this.mode = mode.trim().toLowerCase();
	}

	public void setScriptFile(String file)
	{
		scriptFile = file;
	}

	public String getScriptFile()
	{
		return scriptFile;
	}

	public void setScript(String script)
	{
		this.script = script;
	}

	public String getScript()
	{
		return script;
	}

	public void setParams(String ref)
	{
		Object obj = getReference(ref);
		if (!(obj instanceof PropertySet))
		{
			throw new IllegalArgumentException("Incorrect script params specified");
		}

		params = new HashMap<String, String>();

		for (Resource res : (PropertySet)obj)
		{
			PropertyResource prop = (PropertyResource)res;
			params.put(prop.getName(), prop.getValue());
		}
	}

	public void setParamFiles(String ref)
	{
		try
		{
			Object obj = getReference(ref);
			if (!(obj instanceof ResourceCollection))
			{
				throw new IllegalArgumentException("Incorrect paramFiles were specififed");
			}

			paramsFiles = getParamsFiles((ResourceCollection)obj);
		}
		catch (Throwable e)
		{
			paramsFiles = getParamsFiles(ref);
		}
	}

	protected PigServer getPigServer()
	{
		if (server != null)
		{
			return server;
		}

		if (script == null && scriptFile == null)
		{
			return null;
		}

		String scriptText = script != null ?
				expandRelativeFilePaths(substituteProperties(script)) :
				expandRelativeFilePaths(substituteProperties(loadFileContent(getFileFullPath(scriptFile))));

		return server = newPigServer(scriptText);
	}

	@Override
	protected void releaseHadoopResources()
	{
		if (server != null)
		{
			try
			{
				server.shutdown();
				server = null;
			}
			catch (Throwable e) {}
		}

		super.releaseHadoopResources();
	}

	@Override
	protected void hadoopValidate()
	{
		if (getScriptFile() != null && !getScriptFile().trim().isEmpty())
		{
			File file = new File(getFileFullPath(getScriptFile()));
			if (!file.exists() || file.isDirectory())
			{
				throw new IllegalArgumentException("Incorrect script file specified: " + getScriptFile());
			}
		}
	}

	private String expandRelativeFilePaths(String scriptText)
	{
		return expandPathsInUdfRegistrations(expandPathsInMacrosImports(scriptText));
	}

	private String expandPathsInUdfRegistrations(String scriptText)
	{
		return expandPathsInJavaUdfRegistrations(expandPathsInScriptUdfRegistrations(scriptText));
	}

	private String expandPathsInJavaUdfRegistrations(String scriptText)
	{
		String result = scriptText;

		Matcher matcher = JAVA_UDF_PATTERN.matcher(scriptText);

		while (matcher.find())
		{
			String definition = matcher.group().trim();

			String fileName = definition.substring(REGISTER_KEYWORD.length() + 1).replace(";", "").trim();
			String fullName = resolveImportedFilePath(fileName);

			if (!checkFileExists(fullName))
			{
				throw new BuildException("Failed to find UDF definition file: " + fileName);
			}

			String _definition = definition.replace(fileName, fullName);

			result = result.replace(definition, _definition);
		}

		return result;
	}

	private String expandPathsInScriptUdfRegistrations(String scriptText)
	{
		String result = scriptText;

		Matcher matcher = SCRIPT_UDF_PATTERN.matcher(scriptText);

		while (matcher.find())
		{
			String definition = matcher.group().trim();

			int index = definition.toUpperCase().indexOf(USING_KEYWORD);

			String fileName = definition.substring(REGISTER_KEYWORD.length() + 1, index).replace("'", "").trim();
			String fullName = resolveImportedFilePath(fileName);

			if (!checkFileExists(fullName))
			{
				throw new BuildException("Failed to find UDF definition file: " + fileName);
			}

			String _definition = definition.replace(fileName, fullName);

			result = result.replace(definition, _definition);
		}

		return result;
	}

	private String expandPathsInMacrosImports(String scriptText)
	{
		String result = scriptText;

		Matcher matcher = IMPORT_PATTERN.matcher(scriptText);

		while (matcher.find())
		{
			String definition = matcher.group().trim();

			String fileName = definition.substring(IMPORT_KEYWORD.length() + 1).replace(";", "").replace("'", "").trim();
			String fullName = resolveImportedFilePath(fileName);

			if (!checkFileExists(fullName))
			{
				throw new BuildException("Failed to find macros definition file: " + fileName);
			}

			String _definition = definition.replace(fileName, fullName);

			result = result.replace(definition, _definition);
		}

		return result;
	}

	private String resolveImportedFilePath(String importedFile)
	{
		if (importedFile.startsWith("/") ||						// UNIX absolute file path
			importedFile.startsWith("\\\\") || 					// Windows absolute network path
			importedFile.matches("^(?i)[a-zA-Z]{1}:\\\\.+"))	// Windows absolute local path
		{
			return importedFile;
		}

		// resolving imported file path against Ant project file
		if (scriptFile == null)
		{
			return getFileFullPath(importedFile);
		}

		// resolving imported file path against Pig script file
		String scriptPath = getFileFullPath(scriptFile);
		int index = scriptPath.lastIndexOf("/");
		int _index = scriptPath.lastIndexOf("\\");
		index = index > _index ? index : _index;

		scriptPath = scriptPath.substring(0, index + 1);

		return (scriptPath + importedFile).replace("\\", "/");
	}

	private boolean checkFileExists(String filePath)
	{
		File file = new File(filePath);
		return file.exists() && file.isFile();
	}

	private List<String> getParamsFiles(String str)
	{
		if (str == null || str.trim().isEmpty())
		{
			return null;
		}

		List<String> filesList = new LinkedList<String>();

		String[] files = str.split(",", -1);
		for (String file : files)
		{
			file = getFileFullPath(substituteProperties(file));
			File _file = new File(file);
			if (!_file.exists() || _file.isDirectory())
			{
				throw new IllegalArgumentException("Incorrect params files specififed: " + file);
			}

			if (!filesList.contains(file))
			{
				filesList.add(file);
			}
		}

		return filesList;
	}

	private List<String> getParamsFiles(ResourceCollection resources)
	{
		List<String> filesList = new LinkedList<String>();

		for (Resource res : resources)
		{
			if (!(res instanceof FileResource))
			{
				throw new IllegalArgumentException("Incorrect resource specified as params file: " + res);
			}

			File file = ((FileResource)res).getFile();
			if (!file.exists() || file.isDirectory())
			{
				throw new IllegalArgumentException("Incorrect resource specified as params file: " + file);
			}

			if (!filesList.contains(file.getAbsolutePath()))
			{
				filesList.add(file.getAbsolutePath());
			}
		}

		return filesList.isEmpty() ? null : filesList;
	}

	private PigServer newPigServer(String scriptText)
	{
		InputStream in = null;

		try
		{
			mode = mode.trim().toLowerCase();
			if (!"local".equals(mode) && !"mapreduce".equals(mode))
			{
				throw new BuildException("Incorrect PigServer execution mode specified: " + mode);
			}

			in = new ByteArrayInputStream(scriptText.getBytes("UTF-8"));

			PigServer server = new PigServer(mode);
			server.setBatchOn();

			if (params != null && paramsFiles != null)
			{
				server.registerScript(in, params, paramsFiles);
			}
			else if (params != null)
			{
				server.registerScript(in, params);
			}
			else if (paramsFiles != null)
			{
				server.registerScript(in, paramsFiles);
			}
			else
			{
				server.registerScript(in);
			}

			return server;
		}
		catch (UnsupportedEncodingException e)
		{
			throw new BuildException("Failed to convert script text to UTF-8 byte stream");
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to create new PigServer", e);
		}
		finally
		{
			if (in != null)
			{
				try
				{
					in.close();
				}
				catch (IOException e) {}
			}
		}
	}
}
