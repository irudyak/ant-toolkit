package com.anttoolkit.documentum.common;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import org.apache.tools.ant.*;

public class AclHelper
{
	public static final String ALL_XPERMIT_SYNONIM = "ALL";

	public static final String ALL_XPERMIT =
			IDfACL.DF_XPERMIT_EXECUTE_PROC_STR + "," +
			IDfACL.DF_XPERMIT_CHANGE_LOCATION_STR + "," +
			IDfACL.DF_XPERMIT_CHANGE_STATE_STR + "," +
			IDfACL.DF_XPERMIT_CHANGE_OWNER_STR + "," +
			IDfACL.DF_XPERMIT_CHANGE_PERMIT_STR + "," +
			IDfACL.DF_XPERMIT_DELETE_OBJECT_STR + "," +
			IDfACL.DF_XPERMIT_CHANGE_FOLDER_LINKS_STR;

	public static final String DQL_PERMIT_LEVEL_NONE = "NONE";
	public static final String DQL_PERMIT_LEVEL_BROWSE = "BROWSE";
	public static final String DQL_PERMIT_LEVEL_READ = "READ";
	public static final String DQL_PERMIT_LEVEL_NOTE = "NOTE";
	public static final String DQL_PERMIT_LEVEL_VERSION = "VERSION";
	public static final String DQL_PERMIT_LEVEL_WRITE = "WRITE";
	public static final String DQL_PERMIT_LEVEL_DELETE = "DELETE";

	private static final String DM_OWNER = "dm_owner";
	private static final String DM_WORLD = "dm_world";
	
	public static void setPermitForUser(IDfSysObject sysObj,
										String userName,
								  		int permit,
										String xPermit)
			throws BuildException
	{
		if (sysObj == null)
		{
			return;
		}

		if (permit < IDfACL.DF_PERMIT_NONE || permit > IDfACL.DF_PERMIT_DELETE)
		{
			throw new BuildException("Invalid permit level specified. Permit level=" + permit);
		}

		int _permit = permit < IDfACL.DF_PERMIT_NONE ? IDfACL.DF_PERMIT_NONE : permit;
		String _xPermit = xPermit == null || xPermit.trim().length() == 0 ? null : xPermit;

		//removes all access control entries for a specified user
		//excepting cases when user is dm_world or dm_owner
		if (!userName.equals(DM_WORLD) &&
			!userName.equals(DM_OWNER))
		{
			try
			{
				sysObj.revoke(userName, null);
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to revoke permits for user: " + userName, e);
			}
		}
		else if (userName.equals(DM_OWNER))	//dm_owner shoul have at least READ permissions
		{
			_permit = permit < IDfACL.DF_PERMIT_READ ? IDfACL.DF_PERMIT_READ : permit;
		}

		//if user has at least browse permission
		if (_permit <= IDfACL.DF_PERMIT_NONE &&
			!userName.equals(DM_OWNER) &&
			!userName.equals(DM_WORLD))
		{
			return;
		}

		if (_xPermit == null)
		{
			try
			{
				sysObj.revoke(userName, ALL_XPERMIT);
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to revoke permits for user: " + userName, e);
			}
		}

		try
		{
			sysObj.grant(userName, _permit, _xPermit);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to grant permits to user: " + userName, e);
		}

		if (!userName.equals(DM_WORLD) && _xPermit == null)
		{
			try
			{
				sysObj.revoke(userName, ALL_XPERMIT);
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to revoke permits for user: " + userName, e);
			}

		}
	}

	public static void resetObjectACL(IDfSysObject sysObj)
			throws BuildException
	{
		try
		{
			while (sysObj.getAccessorCount() > 2)
			{
				for (int i = 0; i < 3; i++)
				{
					String accessorName = sysObj.getAccessorName(i);

					if (!accessorName.equals(DM_OWNER) &&
						!accessorName.equals(DM_WORLD))
					{
						sysObj.revoke(accessorName, null);
						break;
					}
				}
			}

			setPermitForUser(sysObj, DM_WORLD, IDfACL.DF_PERMIT_NONE, null);
			setPermitForUser(sysObj, DM_OWNER, IDfACL.DF_PERMIT_DELETE, ALL_XPERMIT);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to reset object ACL", e);
		}
	}

	public static String getDqlPermitLevel(int permit)
	{
		switch (permit)
		{
			case IDfACL.DF_PERMIT_NONE:
				return DQL_PERMIT_LEVEL_NONE;
			case IDfACL.DF_PERMIT_BROWSE:
				return DQL_PERMIT_LEVEL_BROWSE;
			case IDfACL.DF_PERMIT_READ:
				return DQL_PERMIT_LEVEL_READ;
			case IDfACL.DF_PERMIT_RELATE:
				return DQL_PERMIT_LEVEL_NOTE;
			case IDfACL.DF_PERMIT_VERSION:
				return DQL_PERMIT_LEVEL_VERSION;
			case IDfACL.DF_PERMIT_WRITE:
				return DQL_PERMIT_LEVEL_WRITE;
			case IDfACL.DF_PERMIT_DELETE:
				return DQL_PERMIT_LEVEL_DELETE;
		}

		throw new IllegalArgumentException("Unsupported permit level: " + permit);
	}
}
