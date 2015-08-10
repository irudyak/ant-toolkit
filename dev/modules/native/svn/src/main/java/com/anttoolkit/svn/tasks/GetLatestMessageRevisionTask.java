package com.anttoolkit.svn.tasks;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;

import javax.xml.parsers.*;
import java.io.*;
import java.util.regex.*;

public class GetLatestMessageRevisionTask
		extends GenericTask
{
	private String logFile;
	private String pattern;
	private String property;

	private class LogHandler
			extends DefaultHandler
	{
		public static final int UNDEFINED_REVISION = -1;

		private static final String LOGENTRY_ELEMENT = "logentry";
		private static final String REVISION_ATTRIBUTE = "revision";
		private static final String MSG_ELEMENT = "msg";

		public LogHandler(String pattern)
		{
			matcher = Pattern.compile(pattern).matcher("");
		}

		public void startElement(String namespaceURI,
								 String localName,
								 String qName,
								 Attributes attributes)
				throws SAXException
		{
			if (qName.equals(LOGENTRY_ELEMENT))
			{
				currentRevision = parseRevisionNumber(attributes.getValue(REVISION_ATTRIBUTE));
			}
		}

		public void endElement(String namespaceURI,
							   String localName,
							   String qName)
                throws SAXException, BuildException
		{
			try
			{
				if (qName.equals(MSG_ELEMENT))
				{
					message = characters.toString().trim();
				}

				if (qName.equals(LOGENTRY_ELEMENT))
				{
					matcher.reset(message);
					if (matcher.find() && currentRevision > revision)
					{
						revision = currentRevision;
						return;
					}

					currentRevision = UNDEFINED_REVISION;
					message = null;
				}
			}
			finally
			{
				characters.delete(0, characters.length());
			}
		}

		public void characters(char[] buffer,
							   int offset,
							   int length)
                throws SAXException
		{
			String text = (new String(buffer, offset, length)).trim();
			if (text.length() == 0)
			{
				return;
			}

			characters.append(text);
		}

		public int getRevision()
		{
			return revision;
		}

		private int parseRevisionNumber(String number)
		{
			try
			{
				return Integer.parseInt(number);
			}
			catch (NumberFormatException e)
			{
				throw new BuildException("Incorrect revision number in log file: " + number);	
			}
		}

		private Matcher matcher;
		
		private int currentRevision = UNDEFINED_REVISION;
		private int revision = UNDEFINED_REVISION;
		private String message;
		private StringBuffer characters = new StringBuffer();
	}
	
	public void setLogFile(String file)
	{
		logFile = file;
	}

	public void setMessagePattern(String pattern)
	{
		this.pattern = pattern;
	}

	public void setProperty(String name)
	{
		property = name;
	}

	public void doWork()
			throws BuildException
	{
		LogHandler handler = new LogHandler(pattern);

		try
		{
			SAXParserFactory factory = SAXParserFactory.newInstance();
			SAXParser saxParser = factory.newSAXParser();
			saxParser.parse(new File(getFileFullPath(logFile)), handler);
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

		if (handler.getRevision() != LogHandler.UNDEFINED_REVISION)
		{
			this.setPropertyThreadSafe(property, Integer.toString(handler.getRevision()));
		}
	}

	protected void validate()
	{
		if (logFile == null || logFile.trim().length() == 0)
		{
			throw new BuildException("Log file should be specified");
		}

		if (pattern == null || pattern.trim().length() == 0)
		{
			throw new BuildException("Message pattern should be specified");	
		}

		if (property == null || property.trim().length() == 0)
		{
			throw new BuildException("Property name should be specified");	
		}
	}
}
