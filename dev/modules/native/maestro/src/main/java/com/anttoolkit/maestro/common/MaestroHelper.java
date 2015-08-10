package com.anttoolkit.maestro.common;

import java.util.*;

import org.apache.tools.ant.*;

import com.maestro.cli.service.*;
import com.maestro.cli.service.output.*;
import com.maestro.xml.*;

import com.anttoolkit.general.tasks.*;

public class MaestroHelper
{
	private static final Properties CLI_PROPERTIES;

	static
	{
		Properties properties = new Properties();
		try
		{
			properties.load(MaestroHelper.class.getClassLoader().getResourceAsStream("cli-system.properties"));
			CLI_PROPERTIES = properties;
		}
		catch (Throwable e)
		{
			throw new BuildException("Cannot load system properties for Maestro CLI!", e);
		}
	}

	private static String DEFAULT_USER_ID;
	private static String DEFAULT_TOKEN;
	private static Boolean DEFAULT_CREDENTIALS_INITIALIZED = false;

	private static ThreadLocal<CliExecutionService> cliExecService = new ThreadLocal<CliExecutionService>();
	private static ThreadLocal<ICliOutputService> cliOutputService = new ThreadLocal<ICliOutputService>();
	private static ThreadLocal<IApiExecutionService> apiExecService = new ThreadLocal<IApiExecutionService>();

	public static CliExecutionService getCliExecutionService()
	{
		CliExecutionService executionService = cliExecService.get();
		if (executionService != null)
		{
			return executionService;
		}

		executionService = new CliExecutionService();
		executionService.init(CLI_PROPERTIES);

		cliExecService.set(executionService);

		return executionService;
	}

	public static ICliOutputService getCliOutputService()
	{
		ICliOutputService outputService = cliOutputService.get();
		if (outputService != null)
		{
			return outputService;
		}

		outputService = new CliOutputService(CLI_PROPERTIES.getProperty("ftl.root"));

		cliOutputService.set(outputService);

		return outputService;
	}

	public static IApiExecutionService getApiExecutionService()
	{
		IApiExecutionService executionService = apiExecService.get();
		if (executionService != null)
		{
			return executionService;
		}

		executionService = new ApiExecutionService(CLI_PROPERTIES);

		apiExecService.set(executionService);

		return executionService;
	}

	public static String[] parseCredentialsFile(String file, GenericTask task)
	{
		if (!task.fileExists(file))
		{
			return null;
		}

		String text = task.loadFileContent(file);
		String[] chunks = text.split("\n", -1);

		String userId = null;
		String token = null;

		for (String chunk : chunks)
		{
			String[] keyVal = chunk.split("=", -1);
			String key = keyVal[0].trim().toLowerCase();
			String val = keyVal[1].trim();

			if ("id".equals(key))
			{
				userId = val;
			}
			else if ("token".equals(key))
			{
				token = val;
			}
		}

		return userId == null || token == null ? null : new String[] {userId, token};
	}

	public static String getDefaultUsedId(GenericTask task)
	{
		initDefaultCredentials(task);
		return DEFAULT_USER_ID;
	}

	public static String getDefaultToken(GenericTask task)
	{
		initDefaultCredentials(task);
		return DEFAULT_TOKEN;
	}

	public static String obtainToken(String userId, String password)
	{
		CliExecutionService service = getCliExecutionService();

		String md5password = service.getMD5Password(userId.toLowerCase(), password);

		Map<String, String> params = new HashMap<String, String>();
		params.put("Maestro-access-id", userId.toLowerCase());
		params.put("Maestro-authorization", md5password);
		params.put("pmc-id", userId.toLowerCase());
  		params.put("pwd", md5password);

		try
		{
			XMLElement result = service.execute("POST", "access", params);
			return service.parseTokenFromResponse(result);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to obtain authentication token for user: " + userId);
		}
	}

	private static void initDefaultCredentials(GenericTask task)
	{
		synchronized (DEFAULT_CREDENTIALS_INITIALIZED)
		{
			if (DEFAULT_CREDENTIALS_INITIALIZED)
			{
				return;
			}

			String[] credentials = parseCredentialsFile(getCliExecutionService().resolveDefaultCredentialsFilePath(), task);

			DEFAULT_USER_ID = credentials == null ? null : credentials[0];
			DEFAULT_TOKEN = credentials == null ? null : credentials[1];

			DEFAULT_CREDENTIALS_INITIALIZED = true;
		}
	}
}
