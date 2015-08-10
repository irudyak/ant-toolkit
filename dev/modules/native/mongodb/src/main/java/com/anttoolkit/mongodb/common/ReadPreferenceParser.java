package com.anttoolkit.mongodb.common;

import java.util.*;

import com.mongodb.*;

import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.node.*;

import org.apache.tools.ant.*;

public class ReadPreferenceParser
{
	private static final HashMap<String, ReadPreference> READ_PREFERENCES = new HashMap<String, ReadPreference>()
	{{
		put("primary", ReadPreference.primary());
		put("primarypreferred", ReadPreference.primaryPreferred());
		put("secondary", ReadPreference.secondary());
		put("secondarypreferred", ReadPreference.secondaryPreferred());
		put("nearest", ReadPreference.nearest());
	}};

	public static ReadPreference parse(String text)
	{
		if (text == null || text.trim().isEmpty())
		{
			throw new IllegalArgumentException("Can't specify empty ReadPreference");
		}

		if (READ_PREFERENCES.containsKey(text.trim().toLowerCase()))
		{
			return READ_PREFERENCES.get(text.trim().toLowerCase());
		}

		if (!text.contains("{") || !text.contains("}") || !text.contains("mode"))
		{
			throw new BuildException("Incorrect ReadPreference specified: " + text);
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
			throw new BuildException("Failed to parse ReadPreference JSON: " + text, e);
		}

		return parse(node);
	}

	public static ReadPreference parse(JsonNode node)
	{
		if (!node.getNodeType().equals(JsonNodeType.OBJECT) || !node.isContainerNode())
		{
			throw new BuildException("Incorrect ReadPreference JSON: " + node.toString());
		}

		JsonNode modeNode = node.get("mode");
		String mode = modeNode == null ? null : modeNode.textValue().trim().toLowerCase();

		if (mode == null || !READ_PREFERENCES.containsKey(mode))
		{
			throw new BuildException("Incorrect ReadPreference mode specified in JSON: " + node.toString());
		}

		JsonNode tagsNode = node.get("tags");
		if (tagsNode == null)
		{
			return READ_PREFERENCES.get(mode);
		}

		if (!tagsNode.getNodeType().equals(JsonNodeType.ARRAY))
		{
			throw new BuildException("Incorrect ReadPreference tags specification in JSON: " + node.toString());
		}

		ArrayList<DBObject> tags = new ArrayList<DBObject>();

		for (JsonNode tagNode : tagsNode)
		{
			if (!tagNode.fieldNames().hasNext())
			{
				throw new BuildException("Incorrect ReadPreference tags specification in JSON: " + node.toString());
			}

			String key = tagNode.fieldNames().next();
			String value = tagNode.get(key).textValue();

			tags.add(new BasicDBObject(key, value));
		}

		if (tags.isEmpty())
		{
			return READ_PREFERENCES.get(mode);
		}

		DBObject firstTag = tags.get(0);
		tags.remove(0);

		DBObject[] restTags = tags.isEmpty() ? null : tags.toArray(new DBObject[0]);

		if (mode.equals("primary"))
		{
			return ReadPreference.primary();
		}
		else if (mode.equals("primarypreferred"))
		{
			return restTags != null ? ReadPreference.primaryPreferred(firstTag, restTags) : ReadPreference.primaryPreferred(firstTag);
		}
		else if (mode.equals("secondary"))
		{
			return restTags != null ? ReadPreference.secondary(firstTag, restTags) : ReadPreference.secondary(firstTag);
		}
		else if (mode.equals("secondarypreferred"))
		{
			return restTags != null ? ReadPreference.secondaryPreferred(firstTag, restTags) : ReadPreference.secondaryPreferred(firstTag);
		}
		else if (mode.equals("nearest"))
		{
			return restTags != null ? ReadPreference.nearest(firstTag, restTags) : ReadPreference.nearest(firstTag);
		}

		throw new BuildException("Incorrect ReadPreference mode specified in JSON: " + node.toString());
	}
}
