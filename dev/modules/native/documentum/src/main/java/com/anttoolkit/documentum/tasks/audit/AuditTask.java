package com.anttoolkit.documentum.tasks.audit;

import com.anttoolkit.general.common.*;

import java.util.*;

import org.apache.tools.ant.*;

import com.documentum.fc.common.*;

public class AuditTask
		extends GenericAuditTask
{
	private boolean includeSubtypes = false;
	private boolean signAudit = false;
	private int authentication = 0;
	private String eventDescription = null;

	private List<Attribute> attributes = new LinkedList<Attribute>();


	public class Attribute
	{
		public void setName(String name)
		{
			this.name = name;
		}

		public String getName()
		{
			return name;
		}

		private String name = null;
	}

	public Attribute createAttribute()
	{
		Attribute attribute = new Attribute();
		attributes.add(attribute);
		return attribute;
	}

	public void setIncludeSubtypes(String include)
	{
		includeSubtypes = BooleanHelper.getBoolean(include);
	}

	public void setSignAudit(String signFlag)
	{
		signAudit = BooleanHelper.getBoolean(signFlag);
	}

	public void setAuthentication(int authentication)
	{
		this.authentication = authentication;
	}

	public void setEventDescription(String description)
	{
		eventDescription = description;
	}

	public void doWork()
			throws BuildException
	{
		IDfList attributes = getAttributesList();
		String policyId = this.getPolicyId();
		String[] types = this.getTypeNames();
		String[] events = this.getEventNames();

		for (String type : types)
		{
			for (String event : events)
			{
				this.registerEventForType(type.trim(), event.trim(), includeSubtypes, this.getApplication(),
						policyId, this.getState(), signAudit, authentication,
						eventDescription, attributes);
			}
		}
	}

	private IDfList getAttributesList()
	{
		int count = attributes.size();
		if (count == 0)
		{
			return null;
		}

		IDfList attrList = new DfList();
		for (Attribute attr : this.attributes)
		{
			try
			{
				if (attrList.findStringIndex(attr.getName()) == -1)
				{
					attrList.appendString(attr.getName());
				}
			}
			catch (DfException e)
			{
				throw new BuildException(e);
			}
		}

		return attrList;
	}
}
