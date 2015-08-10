package com.anttoolkit.documentum.tasks.permissions;

import com.anttoolkit.documentum.common.*;
import org.apache.tools.ant.*;

import com.documentum.fc.common.*;
import com.documentum.fc.client.*;

public class SetMaclSecurityTask
		extends GenericDocbaseTask
{
	private static final String MACL_SECURITY_SESSION_ARG = "MACL_SECURITY";
	private static final String SET_SESSION_PARAMETERS_FUNCTION = "SET_SESSION_PARAMETERS";

	private boolean maclSecurity = false;

	public void setMacl(boolean maclSecurity)
	{
		this.maclSecurity = maclSecurity;
	}

	public void doWork()
			throws BuildException
	{
		try
		{
			IDfList args = new DfList();
			args.appendString(MACL_SECURITY_SESSION_ARG);

			IDfList types = new DfList();
			types.appendString("b");

			IDfList values = new DfList();
			values.appendString(maclSecurity ? "T" : "F");

			IDfCollection coll = this.getSession().getDfSession().apply(null, SET_SESSION_PARAMETERS_FUNCTION, args, types, values);
			if(coll != null)
			{
				coll.close();
			}
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to set MACL security to: " + maclSecurity, e);
		}
	}
}
