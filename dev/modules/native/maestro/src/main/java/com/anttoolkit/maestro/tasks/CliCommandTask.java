package com.anttoolkit.maestro.tasks;

import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.security.*;
import java.security.cert.*;
import java.util.*;

import javax.net.ssl.*;

import org.apache.commons.io.*;
import org.apache.tools.ant.*;

import freemarker.template.*;

import com.maestro.cli.entity.*;
import com.maestro.cli.service.*;
import com.maestro.xml.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.maestro.common.*;
import com.anttoolkit.maestro.common.Method;

@Method("POST")
public abstract class CliCommandTask
		extends GenericMaestroTask
{
	public static final TrustManager[] TRAST_ALL = { new X509TrustManager()
		{
			public X509Certificate[] getAcceptedIssuers()
			{
				return null;
			}

			public void checkClientTrusted(X509Certificate[] certs, String authType)
			{
			}

			public void checkServerTrusted(X509Certificate[] certs, String authType)
			{
			}
		}
	};

	public static final HostnameVerifier ALL_HOSTS_VALID = new HostnameVerifier()
	{
		public boolean verify(String hostname, SSLSession session)
		{
			return true;
		}
	};

	protected static final int READ_BUFFER_SIZE = 1024;

	@Param("project-id")
	private String project;

	@Param
	private String region;

	public void setProject(String project)
	{
		this.project = project;
	}

	public void setRegion(String region)
	{
		this.region = region;
	}

	protected String getMaestroProject()
	{
		return this.project;
	}

	protected String getMaestroRegion()
	{
		return this.region;
	}

	protected Map<String, String> regionAndProjectParams()
	{
		Map<String, String> map = new HashMap<String, String>();
		map.put("region", region);
		map.put("project-id", project);
		return map;
	}

	protected com.maestro.xml.XMLElement executeCommand()
	{
		if (!this.getClass().isAnnotationPresent(Action.class))
		{
			throw new BuildException("Class '" + this.getClass().getName() + "' should be annotated by '" + Action.class.getName() + "' annotation to be used as CLI command");
		}

		String method = this.getClass().getAnnotation(Method.class).value();
		String action = this.getClass().getAnnotation(Action.class).value();

		return executeCommand(method, action, getCommandParams());
	}

	protected com.maestro.xml.XMLElement executeCommand(String method, String action, Map params)
	{
		if (!params.containsKey(ACCESS_ID_PARAM) || !params.containsKey(AUTHORIZATION_PARAM))
		{
			params.put(ACCESS_ID_PARAM, MaestroSessionManager.getUserId(this));
			params.put(AUTHORIZATION_PARAM, MaestroSessionManager.getToken(this));
		}

		boolean throwException = SystemExitHandler.isThrowException();

		try
		{
			SystemExitHandler.setThrowException(true);
			return getExecutionService().execute(method, action, params);
		}
		finally
		{
			SystemExitHandler.setThrowException(throwException);
		}
	}

	protected void saveLinkContentToStream(String link, OutputStream out)
	{
		StringBuilder sb = new StringBuilder(ExternalPropertiesHolder.resolveServerUrl("ORCH_URL"));
		if ((sb.charAt(sb.length() - 1) == '/') && (link.startsWith("/")))
		{
			sb.append(link.substring(1));
		}
		else
		{
			sb.append(link);
		}

		InputStream in = null;

		try
		{
			SSLContext sc = SSLContext.getInstance("SSL");
			sc.init(null, TRAST_ALL, new SecureRandom());
			HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());
			HttpsURLConnection.setDefaultHostnameVerifier(ALL_HOSTS_VALID);

			in = new BufferedInputStream(new URL(sb.toString()).openStream(), READ_BUFFER_SIZE);

			IOUtils.copy(in, out);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to get content from Maestro server provided by the link: " + link, e);
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

	protected Map<String, String> getCommandParams()
	{
		Map<String, String> params = new HashMap<String, String>();

		Field[] fields = ReflectionHelper.getDeclaredFields(this.getClass(), null);
		for (Field field: fields)
		{
			if (!field.isAnnotationPresent(Param.class))
			{
				continue;
			}

			String[] nameValue = getFieldNameAndValue(field);
			if (nameValue != null)
			{
				params.put(nameValue[0], nameValue[1]);
			}
		}

		if (this.getClass().isAnnotationPresent(DefaultParams.class))
		{
			DefaultParams defaults = this.getClass().getAnnotation(DefaultParams.class);
			if (defaults.params().length != defaults.values().length)
			{
				throw new BuildException("Default parameters incorrectly configured for class '" + this.getClass() + "'");
			}

			for (int i = 0; i < defaults.params().length; i++)
			{
				params.put(defaults.params()[i], defaults.values()[i]);
			}
		}

		if (this.getClass().isAnnotationPresent(SupressParams.class))
		{
			String[] supressParams = this.getClass().getAnnotation(SupressParams.class).value();
			for (String param : supressParams)
			{
				params.remove(param);
			}
		}

		return params;
	}

	protected CliExecutionService getExecutionService()
	{
		return MaestroHelper.getCliExecutionService();
	}

	protected void populateParamValues(String values, Collection<String> param)
	{
		if (values == null || values.trim().isEmpty())
		{
			param.clear();
			return;
		}

		String[] chunks = values.split(",", -1);
		for (String chunk : chunks)
		{
			param.add(chunk.trim());
		}
	}

	protected void printResult(XMLElement element)
	{
		if (element != null)
		{
			prettyPrint(element);
		}
	}

	protected void prettyPrint(XMLElement element)
	{
		prettyPrint(element, true, true, null);
	}

	protected void prettyPrint(XMLElement element, boolean full, boolean tableView, List<String> filterList)
	{
		prettyPrint(element, full, tableView, filterList, null);
	}

	protected void prettyPrint(XMLElement element, boolean full, boolean tableView, List<String> filterList, String templateName) {
		try
		{
			Template template = getFreemarkerTemplate(tableView, filterList, templateName);
			if (template == null)
			{
				simplePrint(element);
				return;
			}

			ResponseObject response = ResponseObject.parseFromXml(element, full, filterList);
			Map params = new HashMap();
			params.put("Response", response);
			Writer out = new StringWriter();
			template.process(params, out);
			log(out.toString());
			out.close();
		}
		catch (Throwable throwable)
		{
			simplePrint(element);
		}
	}

	protected void simplePrint(XMLElement element)
	{
		XMLData data = new XMLData();
		data.setRoot(element);
		this.log(data.toText(true, false));
	}

	protected String getStringFromCollection(Collection<String> collection)
	{
		StringBuilder result = new StringBuilder(256);
		if ((collection != null) && (collection.size() > 0))
		{
			Iterator<String> iter = collection.iterator();
			while (iter.hasNext())
			{
				result.append(iter.next().trim());
				if (iter.hasNext())
				{
					result.append(",");
				}
			}
		}

		return result.toString();
	}

	protected String getSortedStringFromCollection(Collection<String> collection)
	{
		List<String> list = new ArrayList<String>(collection);
		Collections.sort(list);
		return getStringFromCollection(list);
	}

	protected String getStringFromKeyValuePairList(List<String> list)
	{
		if (list == null || list.isEmpty())
		{
			return "";
		}

		StringBuilder result = new StringBuilder(256);

		Iterator<String> iter = list.iterator();
		while (iter.hasNext())
		{
			String data = iter.next().trim();
			int eqIndex = data.indexOf("=");
			if (eqIndex != -1)
			{
				result.append(data.substring(0, eqIndex)).append("::").append(data.substring(eqIndex + 1));
			}

			if (iter.hasNext())
			{
				result.append(",");
			}
		}

		return result.toString();
	}

	protected String getStringFromMap(Map<String, String> map)
	{
		if (map == null || map.isEmpty())
		{
			return "";
		}

		StringBuilder result = new StringBuilder(256);

		Iterator<String> iter = map.keySet().iterator();
		while (iter.hasNext())
		{
			String key = iter.next();
			result.append(key).append("::").append(map.get(key));

			if (iter.hasNext())
			{
				result.append(",");
			}
		}

		return result.toString();
	}

	private String[] getFieldNameAndValue(Field field)
	{
		//make private and protected fields also accessible
		field.setAccessible(true);

		Param fieldMetadata = field.getAnnotation(Param.class);

		String fieldName = fieldMetadata.value();
		fieldName = fieldName.trim().isEmpty() ? field.getName() : fieldName;

		String fieldValue;

		try
		{
			Object value = field.get(this);
			if (value == null)
			{
				return null;
			}

			if (value instanceof Map)
			{
				if (((Map)value).isEmpty())
				{
					return null;
				}

				fieldValue = getStringFromMap((Map)value);
			}
			else if (value instanceof List && fieldMetadata.keyValueList())
			{
				if (((List)value).isEmpty())
				{
					return null;
				}

				fieldValue = getStringFromKeyValuePairList((List) value);
			}
			else if (value instanceof Collection)
			{
				if (((Collection)value).isEmpty())
				{
					return null;
				}

				fieldValue = fieldMetadata.sorted() ?
						getSortedStringFromCollection((Collection)value) :
						getStringFromCollection((Collection)value);
			}
			else
			{
				fieldValue = value.toString();
			}
		}
		catch (IllegalAccessException e)
		{
			throw new BuildException("Failed to get value of the field '" + field.getName() + "'", e);
		}

		return new String[] {fieldName, fieldValue};
	}

	private Template getFreemarkerTemplate(boolean tableView, List<String> filterList, String templateName)
	{
		Template template = null;
		boolean loaded = false;

		if ((!loaded) && (org.apache.commons.lang.StringUtils.isNotBlank(ExternalPropertiesHolder.getCustomFreemarkerTemplateFile())))
		{
			try
			{
				template = this.getExecutionService().getExternalTemplate(ExternalPropertiesHolder.getCustomFreemarkerTemplateFile());
				if (template != null)
				{
					loaded = true;
					filterList.clear();
				}
			}
			catch (IOException ioe) {}
		}

		if (!loaded)
		{
			try
			{
				if (org.apache.commons.lang.StringUtils.isNotEmpty(templateName))
				{
					template = this.getExecutionService().getTemplate(new StringBuilder().append("ftl/").append(templateName).append(".ftl").toString());
				}
				else if (tableView)
				{
					template = this.getExecutionService().getTemplate("ftl/cli-console-table.ftl");
				}
				else
				{
					template = this.getExecutionService().getTemplate("ftl/cli-console.ftl");
				}
			}
			catch (IOException ioe) {}
		}

		return template;
	}
}
