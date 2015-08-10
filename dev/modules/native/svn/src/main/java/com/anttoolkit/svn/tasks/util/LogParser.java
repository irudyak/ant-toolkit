package com.anttoolkit.svn.tasks.util;

import org.apache.tools.ant.*;

import org.xml.sax.*;
import org.xml.sax.helpers.*;

import javax.xml.parsers.*;
import java.io.*;
import java.util.*;

public class LogParser
		extends DefaultHandler
{
	private static final String LOGENTRY_ELEMENT = "logentry";
	private static final String REVISION_ATTRIBUTE = "revision";
	private static final String AUTHOR_ELEMENT = "author";
	private static final String DATE_ELEMENT = "date";
	private static final String MSG_ELEMENT = "msg";
	private static final String PATH_ELEMENT = "path";
	private static final String ACTION_ATTRIBUTE = "action";

	private String startElement;

	private List<PathEntry> pathEntries = new LinkedList<PathEntry>();

	private String revision;
	private String author;
	private String date;
	private String action;
	private String message;
	private StringBuffer characters;

	private LogReport logReport;

	public LogReport parseLog(String logFile)
	{
		init();

		try
		{
			SAXParserFactory factory = SAXParserFactory.newInstance();
			SAXParser saxParser = factory.newSAXParser();
			saxParser.parse(new File(logFile), this);
		}
		catch (SAXException e)
		{
			throw new BuildException("Failed to parse file: " + logFile, e);
		}
		catch (ParserConfigurationException e)
		{
			throw new BuildException("Failed to parse file: " + logFile, e);
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to parse file: " + logFile, e);
		}

		return logReport;
	}

	public void startElement(String namespaceURI,
							 String localName,
							 String qName,
							 Attributes attributes)
			throws SAXException
	{
		this.startElement = qName;

		if (startElement.equals(LOGENTRY_ELEMENT))
		{
			revision = attributes.getValue(REVISION_ATTRIBUTE);
		}

		if (startElement.equals(PATH_ELEMENT))
		{
			action = attributes.getValue(ACTION_ATTRIBUTE);
		}
	}

	public void endElement(String namespaceURI,
						   String localName,
						   String qName)
               throws SAXException, BuildException
	{
		try
		{
			if (qName.equals(AUTHOR_ELEMENT))
			{
				author = characters.toString();
			}

			if (qName.equals(DATE_ELEMENT))
			{
				date = characters.toString();
			}

			if (qName.equals(PATH_ELEMENT))
			{
				pathEntries.add(new PathEntry(characters.toString(), action));
				action = null;
			}

			if (qName.equals(MSG_ELEMENT))
			{
				message = characters.toString().trim();
				message = message == null || message.length() == 0 ? "" : message;
			}

			if (qName.equals(LOGENTRY_ELEMENT))
			{
				logReport.addLogEntry(new LogEntry(author, date, message, revision, pathEntries));

				revision = null;
				author = null;
				date = null;
				message = null;
				pathEntries = new LinkedList<PathEntry>();
			}
		}
		finally
		{
			if (qName.equals(LOGENTRY_ELEMENT) || qName.equals(AUTHOR_ELEMENT) ||
				qName.equals(DATE_ELEMENT) || qName.equals(MSG_ELEMENT) ||
				qName.equals(PATH_ELEMENT))
			{
				startElement = "";
			}

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

	private void init()
	{
		startElement = null;

		pathEntries = new LinkedList<PathEntry>();

		revision = null;
		author = null;
		date = null;
		action = null;
		message = null;
		characters = new StringBuffer();

		logReport = new LogReport();
	}
}
