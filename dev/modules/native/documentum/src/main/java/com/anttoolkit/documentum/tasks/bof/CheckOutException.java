package com.anttoolkit.documentum.tasks.bof;

import org.apache.tools.ant.*;

import com.documentum.fc.common.*;

import java.text.*;

public class CheckOutException
	extends BuildException
{
	private static final String LOCKED_BY_KEYWORD = "locked by";
	private static final String PESSIMISTIC_LOCK_ERROR = "[DM_SYSOBJECT_E_CANT_LOCK]";
	private static final String GENERIC_ERROR_MESSAGE = "Failed to check out {0} \"{1}\"\r\n{2}";
	private static final String PESSIMISTIC_LOCK_SIMPLE_MESSAGE = "{0} \"{1}\" is already locked by {2}";
	private static final String PESSIMISTIC_LOCK_ADVANCED_MESSAGE = "Child object {0} \"{1}\" of {2} \"{3}\" is already locked by {4}";

	private String description = null;

	private String lockedBy = null;
	private String sourceObjectName = null;
	private String sourceObjectType = null;
	private String parentName = null;
	private String parentType = null;

	public CheckOutException(String sourceObjectName, String sourceObjectType, DfException e)
	{
		this.sourceObjectName = sourceObjectName;
		this.sourceObjectType = sourceObjectType;
		description = e.toString();

		if (description.indexOf(PESSIMISTIC_LOCK_ERROR) == -1)
		{
			return;
		}

		int index = description.indexOf(LOCKED_BY_KEYWORD);
		if (index == -1)
		{
			return;
		}

		String temp = description.substring(index + LOCKED_BY_KEYWORD.length());
		index = temp.indexOf(".\"");
		if (index == -1)
		{
			return;
		}

		lockedBy = temp.substring(0, index).trim();
	}

	public String getMessage()
	{
		return toString();
	}

	public String getLocalizedMessage()
	{
		return toString();
	}

	public String toString()
	{
		if (lockedBy != null &&
			parentName != null &&
			parentType != null)
		{
			return MessageFormat.format(PESSIMISTIC_LOCK_ADVANCED_MESSAGE,
					new String[] {sourceObjectType, sourceObjectName, parentType, parentName, lockedBy});
		}

		if (lockedBy != null)
		{
			return MessageFormat.format(PESSIMISTIC_LOCK_SIMPLE_MESSAGE,
					new String[] {sourceObjectType, sourceObjectName, lockedBy});
		}

		return MessageFormat.format(GENERIC_ERROR_MESSAGE,
				new String[] {sourceObjectType, sourceObjectName, description});
	}

	public void setParentObjectInfo(String name, String readableType)
	{
		parentName = name;
		parentType = readableType;
	}
}
