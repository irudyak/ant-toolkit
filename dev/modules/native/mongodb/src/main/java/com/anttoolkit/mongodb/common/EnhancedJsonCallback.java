package com.anttoolkit.mongodb.common;

import java.text.*;
import java.util.*;
import java.util.regex.*;

import com.anttoolkit.general.tasks.GenericTask;
import com.mongodb.*;
import com.mongodb.util.*;

import org.apache.tools.ant.BuildException;
import org.bson.*;
import org.bson.types.*;

public class EnhancedJsonCallback extends BasicBSONCallback
{
	private static final String MS_DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";
	private static final String SEC_DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
	private static final Base64Codec BASE64_CODEC = new Base64Codec();

	private static final Pattern REFERENCE_PATTERN = Pattern.compile("(?i)\\$REFERENCE[\\s]*\\(.*\\)");
	private static final Pattern FILE_PATTERN = Pattern.compile("(?i)\\$FILE[\\s]*\\(.*\\)");

	private boolean lastArray = false;
	private GenericTask task;

	public EnhancedJsonCallback(GenericTask task)
	{
		this.task = task;
	}

	@Override
	public BSONObject create()
	{
		return new BasicDBObject();
	}

	@Override
	protected BSONObject createList()
	{
		return new BasicDBList();
	}

	@Override
	public void objectStart(boolean array, String name)
	{
		lastArray = array;
		super.objectStart(array, name);
	}

	public Object objectDone()
	{
		String name = curName();
		Object obj = super.objectDone();
		if (lastArray)
		{
			return obj;
		}

		obj = processDoneObject((BSONObject)obj);

		if (!isStackEmpty())
		{
			_put(name, obj);
		}
		else
		{
			obj = !BSON.hasDecodeHooks() ? obj : BSON.applyDecodingHooks( obj );
			setRoot(obj);
		}

		return obj;
	}

	protected Object processDoneObject(BSONObject bson)
	{
		// override the object if it's a special type
		if (bson.containsField("$oid"))
		{
			return processOID(bson);
		}
		else if (bson.containsField("$date"))
		{
			return processDate(bson);
	 	}
		else if (bson.containsField("$regex"))
		{
			return processRegex(bson);
		}
		else if (bson.containsField("$ts"))
		{
			return processTs(bson);
		}
		else if (bson.containsField("$timestamp"))
		{
			return processTimestamp(bson);
		}
		else if (bson.containsField("$code"))
		{
			return processCode(bson);
		}
		else if (bson.containsField("$ref"))
		{
			return processRef(bson);
		}
		else if (bson.containsField("$minKey"))
		{
			return processMinKey(bson);
		}
		else if (bson.containsField("$maxKey"))
		{
			return processMaxKey(bson);
		}
		else if (bson.containsField("$uuid"))
		{
			return processUUID(bson);
		}
		else if (bson.containsField("$binary"))
		{
			return processBinary(bson);
		}
		else if (bson.containsField("$numberLong"))
		{
			return processNumberLong(bson);
		}

		return bson;
	}

	protected Object processOID(BSONObject bson)
	{
		return new ObjectId((String) bson.get("$oid"));
	}

	protected Object processDate(BSONObject bson)
	{
		if (bson.get("$date") instanceof Number)
		{
			return new Date(((Number) bson.get("$date")).longValue());
	 	}

		SimpleDateFormat format = new SimpleDateFormat(MS_DATE_FORMAT);
		format.setCalendar(new GregorianCalendar(new SimpleTimeZone(0, "GMT")));
		Object obj = format.parse(bson.get("$date").toString(), new ParsePosition(0));

		if (obj != null)
		{
			return obj;
		}

		// try older format with no ms
		format = new SimpleDateFormat(SEC_DATE_FORMAT);
		format.setCalendar(new GregorianCalendar(new SimpleTimeZone(0, "GMT")));
		return format.parse(bson.get("$date").toString(), new ParsePosition(0));
	}

	protected Object processRegex(BSONObject bson)
	{
		return Pattern.compile((String) bson.get("$regex"), BSON.regexFlags((String) bson.get("$options")));
	}

	protected Object processTs(BSONObject bson)
	{
		//Legacy timestamp format
		Integer ts = ((Number) bson.get("$ts")).intValue();
		Integer inc = ((Number) bson.get("$inc")).intValue();
		return new BSONTimestamp(ts, inc);
	}

	protected Object processTimestamp(BSONObject bson)
	{
		BSONObject tsObject = (BSONObject) bson.get("$timestamp");
		Integer ts = ((Number) tsObject.get("t")).intValue();
		Integer inc = ((Number) tsObject.get("i")).intValue();
		return new BSONTimestamp(ts, inc);
	}

	protected Object processCode(BSONObject bson)
	{
		return bson.containsField("$scope") ?
				new CodeWScope((String) bson.get("$code"), (DBObject) bson.get("$scope")) :
				new Code((String) bson.get("$code"));
	}

	protected Object processRef(BSONObject bson)
	{
		return new DBRef(null, (String) bson.get("$ref"), bson.get("$id"));
	}

	protected Object processMinKey(BSONObject bson)
	{
		return new MinKey();
	}

	protected Object processMaxKey(BSONObject bson)
	{
		return new MaxKey();
	}

	protected Object processUUID(BSONObject bson)
	{
		return UUID.fromString((String) bson.get("$uuid"));
	}

	protected Object processBinary(BSONObject bson)
	{
		String text = (String)bson.get("$binary");
		byte[] bytes;

		if (REFERENCE_PATTERN.matcher(text).matches())
		{
			bytes = processReferenceRef(text);
		}
		else if (FILE_PATTERN.matcher(text).matches())
		{
			bytes = processFileRef(text);
		}
		else
		{
			bytes = BASE64_CODEC.decode(text);
		}

		int type = (Integer)bson.get("$type");

		return new Binary((byte) type, bytes);
	}

	protected Object processNumberLong(BSONObject bson)
	{
		return Long.valueOf((String) bson.get("$numberLong"));
	}

	protected byte[] processFileRef(String text)
	{
		int start = text.indexOf("(");
		int finish = text.lastIndexOf(")");
		String fileName = text.substring(start + 1, finish).trim();

		return task.loadFileContentRaw(fileName);
	}

	protected byte[] processReferenceRef(String text)
	{
		int start = text.indexOf("(");
		int finish = text.lastIndexOf(")");
		String reference = text.substring(start + 1, finish).trim();

		Object obj = task.getReference(reference);
		if (obj == null || !(obj instanceof byte[]))
		{
			throw new BuildException("Specified reference '" + reference + "' doesn't contain byte[] object");
		}

		return (byte[])obj;
	}
}
