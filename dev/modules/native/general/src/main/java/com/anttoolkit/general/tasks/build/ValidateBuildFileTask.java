package com.anttoolkit.general.tasks.build;

import java.io.*;
import java.util.*;
import javax.xml.parsers.*;
import javax.xml.xpath.*;

import org.xml.sax.*;
import org.w3c.dom.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.build.util.*;

public class ValidateBuildFileTask extends GenericTask
{
	private static final String NUMBER_ATTR = "number";
	private static final String QUALIFIER_ATTR = "qualifier";

	private String file;
	private String xPath;

	public void setFile(String file)
	{
		this.file = file;
	}

	public void setXPath(String xpath)
	{
		xPath = xpath;
	}

	public void doWork() throws BuildException
	{
		try
		{
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();

			Document doc = builder.parse(getFileFullPath(file));

			XPathFactory xPathfactory = XPathFactory.newInstance();
			XPath xpath = xPathfactory.newXPath();
			XPathExpression expr = xpath.compile(xPath);

			NodeList list = (NodeList)expr.evaluate(doc, XPathConstants.NODESET);
			if (list == null || list.getLength() == 0)
			{
				return;
			}

			Set<String> processedBuilds = new HashSet<String>();
			Map<String, BuildVersion> prevBuilds = new HashMap<String, BuildVersion>();
			int count = list.getLength();

			for (int i = 0; i < count; i++)
			{
				Element node = (Element)list.item(i);
				BuildVersion buildVersion = BuildVersion.parse(node.getAttribute(NUMBER_ATTR).trim());
				String qualifier = node.hasAttribute(QUALIFIER_ATTR) ? node.getAttribute(QUALIFIER_ATTR) : "";

				if (processedBuilds.contains(buildVersion.toString() + qualifier))
				{
					throw new BuildException("There are several build containers with the same build version number: " + buildVersion);
				}

				BuildVersion prevBuildVersion = prevBuilds.get(qualifier);

				if (prevBuildVersion != null && prevBuildVersion.compareTo(buildVersion) > 0)
				{
					String qualifierTitle = qualifier.isEmpty() ? "" : "[" + qualifier + "]";
					throw new BuildException("Incorrect build containers sequence, build having version number " +
							buildVersion + qualifierTitle + " should follow before the build " +
							prevBuildVersion + qualifierTitle);
				}

				processedBuilds.add(buildVersion.toString() + qualifier);
				prevBuilds.put(qualifier, buildVersion);
			}
		}
		catch (ParserConfigurationException e)
		{
			throw new BuildException("Failed to configure XML DOM parser", e);
		}
		catch (SAXException e)
		{
			throw new BuildException("Failed to parse specified xml file: " + file, e);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to parse specified xml file: " + file, e);
		}
		catch (XPathExpressionException e)
		{
			throw new BuildException("Failed to compile specified XPath expression: " + xPath, e);
		}
	}

	protected void validate()
	{
		if (file == null)
		{
			throw new BuildException("File name should be specified");
		}

		File file = new File(getFileFullPath(this.file));
		if (!file.exists() || !file.isFile())
		{
			throw new BuildException("Specified file \"" + this.file + "\" doesn't exist");
		}

		if (xPath == null)
		{
			throw new BuildException("XPath should be specified");
		}
	}
}
