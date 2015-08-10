package com.anttoolkit.documentum.common;

import com.documentum.fc.common.*;
import org.apache.tools.ant.*;

import com.documentum.fc.client.*;

public class PermitAction
{
	private boolean isGrantAction = true;

	private String accessorName = null;
	private int permit = -1;
	private String xpermit = null;

	public void setAccessor(String name)
	{
		accessorName = name;
	}

	public void setPermit(String permit)
	{
		String _permit = permit.trim().toUpperCase();
		if (_permit.equals("NONE"))
		{
			this.permit = IDfACL.DF_PERMIT_NONE;
		}
		else if (_permit.equals("BROWSE"))
		{
			this.permit = IDfACL.DF_PERMIT_BROWSE;
		}
		else if (_permit.equals("READ"))
		{
			this.permit = IDfACL.DF_PERMIT_READ;
		}
		else if (_permit.equals("RELATE"))
		{
			this.permit = IDfACL.DF_PERMIT_RELATE;
		}
		else if (_permit.equals("VERSION"))
		{
			this.permit = IDfACL.DF_PERMIT_VERSION;
		}
		else if (_permit.equals("WRITE"))
		{
			this.permit = IDfACL.DF_PERMIT_WRITE;
		}
		else if (_permit.equals("DELETE"))
		{
			this.permit = IDfACL.DF_PERMIT_DELETE;
		}
		else
		{
			throw new BuildException("Unknown permit " + permit);
		}
	}

	public void setXpermit(String xpermit)
	{
		this.xpermit = AclHelper.ALL_XPERMIT_SYNONIM.equals(xpermit.toUpperCase()) ? AclHelper.ALL_XPERMIT : xpermit;
	}

	public void markActionType(boolean isGrantAction)
	{
		this.isGrantAction = isGrantAction;
	}

	public void execute(IDfACL acl)
			throws DfException
	{
		if (accessorName == null || accessorName.trim().length() == 0)
		{
			throw new BuildException("Accessor name should be specified");
		}

		if (isGrantAction)
		{
			if (permit == -1)
			{
				throw new BuildException("Accessor permit should be specified");
			}

			try
			{
				acl.grant(accessorName, permit, xpermit);
			}
			catch (DfException e)
			{
				throw new DfException("Failed to grant permit to '" + accessorName + "'", e);
			}
		}
		else
		{
			try
			{
				acl.revoke(accessorName, xpermit);
			}
			catch (DfException e)
			{
				throw new DfException("Failed to revoke '" + accessorName + "' permits", e);
			}
		}
	}
}
