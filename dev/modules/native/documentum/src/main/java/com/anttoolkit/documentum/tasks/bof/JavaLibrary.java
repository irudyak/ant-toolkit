package com.anttoolkit.documentum.tasks.bof;

import com.anttoolkit.documentum.common.*;

public class JavaLibrary
		extends BasicDocAppObject
{
	public JavaLibrary(String objectId,
					   String name,
					   String type,
					   DocbaseSession session,
					   IContentStorage storage)
	{
		super(objectId, name, type, session, storage);
	}

	protected String getReadableTypeName()
	{
		return "Java Library";
	}

	protected String contentQueryTemplate()
	{
		return "select r_object_id, object_name, r_object_type " +
				"from dmc_jar where folder(ID(''{0}'')) " +
				"union " +
				"select r_object_id, object_name, r_object_type " +
				"from dm_document " +
				"where folder(ID(''{0}''))";
	}
}
