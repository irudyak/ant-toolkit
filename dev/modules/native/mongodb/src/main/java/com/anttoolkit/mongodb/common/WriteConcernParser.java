package com.anttoolkit.mongodb.common;

import java.util.*;

import com.mongodb.*;

import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.node.*;
import com.fasterxml.jackson.core.*;

import org.apache.tools.ant.*;

public class WriteConcernParser
{
	private static final Map<String, WriteConcern> WRITE_CONCERNS = new HashMap<String, WriteConcern>()
	{{
		put("ERRORS_IGNORED", WriteConcern.ERRORS_IGNORED);
		put("ACKNOWLEDGED", WriteConcern.ACKNOWLEDGED);
		put("UNACKNOWLEDGED", WriteConcern.UNACKNOWLEDGED);
		put("FSYNCED", WriteConcern.FSYNCED);
		put("JOURNALED", WriteConcern.JOURNALED);
		put("REPLICA_ACKNOWLEDGED", WriteConcern.REPLICA_ACKNOWLEDGED);
		put("NONE", WriteConcern.NONE);
		put("NORMAL", WriteConcern.NORMAL);
		put("SAFE", WriteConcern.SAFE);
		put("MAJORITY", WriteConcern.MAJORITY);
		put("FSYNC_SAFE", WriteConcern.FSYNC_SAFE);
		put("JOURNAL_SAFE", WriteConcern.JOURNAL_SAFE);
		put("REPLICAS_SAFE", WriteConcern.REPLICAS_SAFE);
	}};

	public static WriteConcern parse(String text)
	{
		if (text == null || text.trim().isEmpty())
		{
			throw new IllegalArgumentException("Can't specify empty WriteConcern");
		}

		if (WRITE_CONCERNS.containsKey(text.trim().toUpperCase()))
		{
			return WRITE_CONCERNS.get(text.trim().toUpperCase());
		}

		if (!text.contains("{") || !text.contains("}"))
		{
			return new WriteConcern(text.trim());
		}

		ObjectMapper mapper = new ObjectMapper();
		mapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);

		JsonNode node;

		try
		{
			node = mapper.readTree(text);
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to parse WriteConcern JSON: " + text, e);
		}

		return parse(node);
	}

	public static WriteConcern parse(JsonNode node)
	{
		if (!node.getNodeType().equals(JsonNodeType.OBJECT) || !node.isContainerNode())
		{
			throw new BuildException("Incorrect WriteConcern JSON: " + node.toString());
		}

		JsonNode wNode = node.get("w");
		JsonNode jNode = node.get("j");
		JsonNode wtimeoutNode = node.get("wtimeout");
		JsonNode fsyncNode = node.get("fsync");

		if (wNode == null && jNode == null && wtimeoutNode == null && fsyncNode == null)
		{
			return new WriteConcern();
		}

		if (wNode == null)
		{
			throw new BuildException("Incorrect WriteConcern JSON: " + node.toString());
		}

		String w = wNode.textValue();
		boolean j = jNode != null && jNode.booleanValue();
		int wtimeout = wtimeoutNode == null ? 0 : wtimeoutNode.intValue();
		boolean fsync = fsyncNode != null && fsyncNode.booleanValue();

		Integer wNumber;

		try
		{
			wNumber = Integer.parseInt(w);
		}
		catch (NumberFormatException e)
		{
			wNumber = null;
		}

		return wNumber != null ?
				new WriteConcern(wNumber, wtimeout, fsync, j) :
				new WriteConcern(w, wtimeout, fsync, j);
	}
}
