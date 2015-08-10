package com.anttoolkit.documentum.tasks.bof;

import org.apache.tools.ant.*;

import com.documentum.fc.common.*;

import java.text.*;

public class UpdateException
		extends BuildException
{
	private static final String GENERIC_ERROR_MESSAGE = "Failed to update {0} \"{1}\"\r\n{2}";
	private static final String ENHANCED_ERROR_MESSAGE = "Failed to update {0} \"{1}\" - child object of {2} \"{3}\"\r\n{4}";

	private String description = null;

	private String sourceObjectName = null;
	private String sourceObjectType = null;
	private String parentName = null;
	private String parentType = null;

	public UpdateException(String sourceObjectName, String sourceObjectType, DfException e)
	{
		this.sourceObjectName = sourceObjectName;
		this.sourceObjectType = sourceObjectType;
		description = e.toString();
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
		if (parentName != null &&
			parentType != null)
		{
			return MessageFormat.format(ENHANCED_ERROR_MESSAGE,
					new String[] {sourceObjectType, sourceObjectName, parentType, parentName, description});
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
