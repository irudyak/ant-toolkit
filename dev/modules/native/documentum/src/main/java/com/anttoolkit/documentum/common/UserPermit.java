package com.anttoolkit.documentum.common;

import com.documentum.fc.client.*;

import org.apache.tools.ant.*;

import java.util.*;

public class UserPermit
{
	private String name = null;
	private int permit = IDfACL.DF_PERMIT_NONE;
	private List xpermits = new Vector();

	public void setName(String name)
	{
		this.name = name;
	}

	public String getName()
	{
		return name;
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

	public void setIntPermit(int permit)
	{
		if (permit != IDfACL.DF_PERMIT_NONE &&
			permit != IDfACL.DF_PERMIT_BROWSE &&
			permit != IDfACL.DF_PERMIT_READ &&
			permit != IDfACL.DF_PERMIT_RELATE &&
			permit != IDfACL.DF_PERMIT_VERSION &&
			permit != IDfACL.DF_PERMIT_WRITE &&
			permit != IDfACL.DF_PERMIT_DELETE)
		{
			throw new BuildException("Unknown permit " + permit);
		}

		this.permit = permit;
	}

	public int getPermit()
	{
		return permit;
	}

	public void setXpermit(String xpermits)
	{
		this.xpermits.clear();

		if (AclHelper.ALL_XPERMIT_SYNONIM.equals(xpermits.toUpperCase()))
		{
			appendXpermits(AclHelper.ALL_XPERMIT);
		}
		else
		{
			appendXpermits(xpermits);
		}
	}

	public String getXpermit()
	{
		if (xpermits.size() == 0)
		{
			return null;
		}

		StringBuffer xpermits = new StringBuffer();
		int count = this.xpermits.size();
		for (int i = 0; i < count; i++)
		{
			if (xpermits.length() == 0)
			{
				xpermits.append(this.xpermits.get(i));
				continue;
			}

			xpermits.append(",");
			xpermits.append(this.xpermits.get(i));
		}

		return xpermits.toString();
	}

	public void appendXpermits(String xpermits)
	{
		if (xpermits == null)
		{
			return;
		}

		String[] xpermitsArray = xpermits.toUpperCase().split(",", -1);
		int count = xpermitsArray.length;
		for (int i = 0; i < count; i++)
		{
			appendXpermit(xpermitsArray[i]);
		}
	}

	private void appendXpermit(String xpermit)
	{
		if (!xpermit.equals(IDfACL.DF_XPERMIT_EXECUTE_PROC_STR) &&
			!xpermit.equals(IDfACL.DF_XPERMIT_CHANGE_LOCATION_STR) &&
			!xpermit.equals(IDfACL.DF_XPERMIT_CHANGE_STATE_STR) &&
			!xpermit.equals(IDfACL.DF_XPERMIT_CHANGE_PERMIT_STR) &&
			!xpermit.equals(IDfACL.DF_XPERMIT_CHANGE_OWNER_STR) &&
			!xpermit.equals(IDfACL.DF_XPERMIT_DELETE_OBJECT_STR) &&
			!xpermit.equals(IDfACL.DF_XPERMIT_CHANGE_FOLDER_LINKS_STR))
		{
			throw new BuildException("Unknown  Xpermit " + xpermit);
		}

		if (!xpermits.contains(xpermit))
		{
			xpermits.add(xpermit);
		}
	}
}
