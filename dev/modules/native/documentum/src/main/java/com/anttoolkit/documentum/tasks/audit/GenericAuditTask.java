package com.anttoolkit.documentum.tasks.audit;

import com.anttoolkit.documentum.common.*;

import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import java.text.*;

public abstract class GenericAuditTask
		extends GenericDocbaseTask
{
	private static final String DQL_GET_POLICY_ID_BY_NAME = "select r_object_id from dm_policy " +
			"where object_name=''{0}''";

	private String typeName = null;
	private String eventName = null;
	private String application = null;
	private String policy = null;
	private String stateName = null;

	public void setType(String name)
	{
		typeName = name;
	}

	public String[] getTypeNames()
	{
		return typeName.split(",", 1);
	}

	public void setEvent(String name)
	{
		eventName = name;
	}

	public String[] getEventNames()
	{
		return eventName.split(",", -1);
	}

	public void setApplication(String application)
	{
		this.application = application;
	}

	public String getApplication()
	{
		return application;
	}

	public void setPolicy(String policy)
	{
		this.policy = policy;
	}

	public String getPolicyId()
			throws BuildException
	{
		if (policy == null)
		{
			return null;
		}

		try
		{
			String dqlQuery = MessageFormat.format(DQL_GET_POLICY_ID_BY_NAME, new String[] {policy});
			return DqlHelper.getStringParamFromFirstString(this.getSession(), dqlQuery);
		}
		catch (DfException e)
		{
			throw new BuildException("Error occured while trying to get policy r_object_id " +
					"for policy: " + policy, e);
		}
		catch (DfEndOfCollectionException e)
		{
			throw new BuildException("There are no policy with name " + policy);
		}
	}

	public void setState(String name)
	{
		stateName = name;
	}

	public String getState()
	{
		return stateName;
	}

	protected void validate()
	{
		if (typeName == null || eventName == null)
		{
			throw new BuildException("typeName and eventName are mandatory attributes");
		}

		if (stateName != null && policy == null)
		{
			throw new BuildException("Policy should be specified for state " + stateName);
		}
	}

	protected void registerEventForType(String typeName,
										String event,
										boolean auditSubtypes,
										String controllingApp,
										String policyId,
										String stateName,
										boolean signAudit,
										int authentication,
										String eventDescription,
										IDfList attributeList)
			throws BuildException
	{
		this.unregisterEventForType(typeName, event, controllingApp, policyId, stateName);

		try
		{
			IDfId _policyId = policyId == null ? null : new DfId(policyId);
			this.getAuditTrailManager().registerEventForType(typeName, event, auditSubtypes,
					controllingApp, _policyId, stateName, signAudit, authentication,
					eventDescription, attributeList);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to register event \"" + event + "\" for type: " + typeName, e);
		}
	}

	protected void unregisterEventForType(String typeName,
										  String event,
										  String controllingApp,
										  String policyId,
										  String stateName)
			throws BuildException
	{
		if (!isEventAuditedForType(typeName, event, controllingApp, policyId, stateName))
		{
			return;
		}

		try
		{
			IDfId _policyId = policyId == null ? null : new DfId(policyId);
			this.getAuditTrailManager().unregisterEventForType(typeName, event, controllingApp, _policyId, stateName);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to unregister event \"" + event + "\" for type: " + typeName, e);
		}
	}

	private boolean isEventAuditedForType(String typeName,
										  String event,
										  String controllingApp,
										  String policyId,
										  String stateName)
			throws BuildException
	{
		try
		{
			return this.getAuditTrailManager().isEventAuditedForType(typeName, event, controllingApp, new DfId(policyId), stateName);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to verify if event \"" + event +
					"\" is already audited for type: " + typeName, e);
		}
	}

	private IDfAuditTrailManager getAuditTrailManager()
			throws BuildException
	{
		try
		{
			return this.getSession().getDfSession().getAuditTrailManager();
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get AuditTrailManager", e);
		}
	}
}
