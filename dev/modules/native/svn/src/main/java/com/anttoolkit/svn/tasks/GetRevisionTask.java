package com.anttoolkit.svn.tasks;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;
import org.xml.sax.helpers.*;
import org.xml.sax.*;

import javax.xml.parsers.*;
import java.io.*;

public class GetRevisionTask
		extends GenericTask
{
	private static final String REVISION_TYPE_HEAD = "HEAD";
	private static final String REVISION_TYPE_COMMITTED = "COMMITTED";

	private String infoFile;
	private String property;
	private String revisionType = REVISION_TYPE_HEAD;

	private class InfoHandler
			extends DefaultHandler
	{
		private static final String ENTRY_NODE = "entry";
		private static final String COMMIT_NODE = "commit";
		private static final String REVISION_ATTRIBUTE = "revision";

		public void startElement(String namespaceURI,
								 String localName,
								 String qName,
								 Attributes attributes)
				throws SAXException
		{
			if (qName.equals(COMMIT_NODE))
			{
				 commitRevision = attributes.getValue(REVISION_ATTRIBUTE);
			}

			if (qName.equals(ENTRY_NODE))
			{
				headRevision = attributes.getValue(REVISION_ATTRIBUTE);
			}
		}

		public String getCommitRevision()
		{
			return commitRevision;
		}

		public String getHeadRevision()
		{
			return headRevision;
		}

		private String commitRevision;
		private String headRevision;
	}

	public void setRevisionType(String type)
	{
		revisionType = type.toUpperCase();
	}

	public void setInfoFile(String file)
	{
		infoFile = file;
	}

	public void setProperty(String name)
	{
		property = name;
	}

	public void doWork()
			throws BuildException
	{
		InfoHandler handler = new InfoHandler();

		try
		{
			SAXParserFactory factory = SAXParserFactory.newInstance();
			SAXParser saxParser = factory.newSAXParser();
			saxParser.parse(new File(getFileFullPath(infoFile)), handler);
		}
		catch (SAXException e)
		{
			throw new BuildException(e);
		}
		catch (ParserConfigurationException e)
		{
			throw new BuildException(e);
		}
		catch (IOException e)
		{
			throw new BuildException(e);
		}
		
		this.setPropertyThreadSafe(property,
				REVISION_TYPE_HEAD.equals(revisionType) ? handler.getHeadRevision() : handler.getCommitRevision());
	}

	protected void validate()
	{
		if (infoFile == null || infoFile.trim().length() == 0)
		{
			throw new BuildException("Info file doesn't specified");
		}

		if (property == null || property.trim().length() == 0)
		{
			throw new BuildException("Property name doesn't specified");	
		}

		if (revisionType == null || revisionType.trim().length() == 0)
		{
			throw new BuildException("Revision type doesn't specified");	
		}

		if (!REVISION_TYPE_HEAD.equals(revisionType) &&
			!REVISION_TYPE_COMMITTED.equals(revisionType))
		{
			throw new BuildException("Invalid revision type: " + revisionType);
		}
	}
}
