package com.anttoolkit.documentum.tasks.lifecycle;

import com.anttoolkit.documentum.common.*;

import org.apache.tools.ant.*;

import com.documentum.fc.common.*;
import com.documentum.fc.client.*;

import java.text.*;

public class SetLifecycleActionsModuleTask
		extends GenericDocbaseTask
{
	private static final String GET_POLICY_ID = "select r_object_id from dm_policy " +
			"where object_name=''{0}''";

	private String lifecycle = null;
	private String module = null;

	public void setLifecycle(String lifecycle)
	{
		this.lifecycle = lifecycle;
	}

	public void setModule(String module)
	{
		this.module = module;
	}

	public void doWork()
			throws BuildException
	{
		IDfPolicy policy = getPolicy();

		try
		{
			policy.unInstall(false);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to uninstall policy: " + lifecycle, e);
		}

		int count;

		try
		{
			count = policy.getStateNoCount();
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get policy states count", e);
		}

		try
		{
			for (int i = 0; i < count; i++)
			{
				policy.setUserActionService(i, module);
			}
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to set state action module", e);
		}

		try
		{
			policy.save();
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to save modified policy: " + lifecycle, e);
		}

		try
		{
			policy.validate();
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to validate policy: " + lifecycle, e);
		}

		try
		{
			policy.install(false);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to install policy: " + lifecycle, e);
		}
	}

	protected void validate()
	{
		if (lifecycle == null)
		{
			throw new BuildException("Lifecycle doesn't specified");
		}

		if (module == null)
		{
			throw new BuildException("Module doesn't specified");
		}
	}

	private IDfPolicy getPolicy()
			throws BuildException
	{
		try
		{
			String dqlQuery = MessageFormat.format(GET_POLICY_ID, new String[] {lifecycle});
			String policyId = DqlHelper.getStringParamFromFirstString(this.getSession(), dqlQuery);

			IDfSysObject policyObj = (IDfSysObject)this.getSession().getDfObject(policyId);

			return (IDfPolicy)policyObj;
		}
		catch (Exception e)
		{
			throw new BuildException("Failed to get IDfPolicy for policy: " + lifecycle, e);
		}
	}
}
