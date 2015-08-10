package com.anttoolkit.documentum.tasks.usergroup;

import com.anttoolkit.documentum.common.*;

import com.anttoolkit.documentum.tasks.object.DocbaseObjectTask;
import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import org.apache.tools.ant.*;

import java.text.*;

public class CreateUserTask
		extends DocbaseObjectTask
{
	private static final String USER_SOURCE_INLINE = "inline password";
	private static final String DQL_IS_FOLDER_EXIST = "select r_object_id from dm_folder where any r_folder_path=''{0}''";

	private static final String USER_NAME = "user_name";
	private static final String USER_LOGIN_NAME = "user_login_name";
	private static final String USER_OS_NAME = "user_os_name";
	private static final String USER_DB_NAME = "user_db_name";
	private static final String USER_SOURCE = "user_source";
	private static final String USER_PASSWORD = "user_password";
	private static final String USER_LOGIN_DOMAIN = "user_login_domain";
	private static final String WINDOWS_DOMAIN = "user_os_domain";
	private static final String USER_EMAIL = "user_address";
	private static final String DESCRIPTION = "description";
	private static final String DEFAULT_FOLDER = "default_folder";
	private static final String CLIENT_CAPABILITY = "client_capability";
	private static final String USER_PRIVILEGES = "user_privileges";
	private static final String USER_EXTENDED_PRIVILEGES = "user_xprivileges";

	public void setUserName(String name)
	{
		this.addConfiguredProperty(new DocbaseObjectProperty(USER_NAME, name));
	}

	public void setLoginName(String name)
	{
		this.addConfiguredProperty(new DocbaseObjectProperty(USER_LOGIN_NAME, name));
	}

	public void setUserOSName(String name)
	{
		this.addConfiguredProperty(new DocbaseObjectProperty(USER_OS_NAME, name));
	}

	public void setUserDBName(String name)
	{
		this.addConfiguredProperty(new DocbaseObjectProperty(USER_DB_NAME, name));
	}

	public void setUserSource(String source)
	{
		this.addConfiguredProperty(new DocbaseObjectProperty(USER_SOURCE, source));
	}

	public void setPassword(String password)
	{
		this.addConfiguredProperty(new DocbaseObjectProperty(USER_PASSWORD, password));
	}

	public void setLoginDomain(String domain)
	{
		this.addConfiguredProperty(new DocbaseObjectProperty(USER_LOGIN_DOMAIN, domain));
	}

	public void setWindowsDomain(String domain)
	{
		this.addConfiguredProperty(new DocbaseObjectProperty(WINDOWS_DOMAIN, domain));
	}

	public void setEmail(String email)
	{
		this.addConfiguredProperty(new DocbaseObjectProperty(USER_EMAIL, email));
	}

	public void setUserDescription(String description)
	{
		this.addConfiguredProperty(new DocbaseObjectProperty(DESCRIPTION, description));
	}

	public void setDefaultFolder(String folder)
	{
		this.addConfiguredProperty(new DocbaseObjectProperty(DEFAULT_FOLDER, folder));
	}

	public void setClientCapability(int capability)
	{
		this.addConfiguredProperty(new DocbaseObjectProperty(CLIENT_CAPABILITY, Integer.toString(capability)));
	}

	public void setPrivileges(int privileges)
	{
		this.addConfiguredProperty(new DocbaseObjectProperty(USER_PRIVILEGES, Integer.toString(privileges)));
	}

	public void setExtendedPrivileges(int privileges)
	{
		this.addConfiguredProperty(new DocbaseObjectProperty(USER_EXTENDED_PRIVILEGES, Integer.toString(privileges)));
	}

	//unnecessary for CreateUserTask
	public void setObjectId(String objectId)
	{
		throw new BuildException("objectId attribute is not supported");
	}

	//unnecessary for CreateUserTask
	public void setFolder(String folder)
	{
		throw new BuildException("folder attribute is not supported");
	}

	public void doWork()
			throws BuildException
	{
		verifyAttributes();
		createDefaultFolder();

		super.doWork();

		resetDefaultFolderPermits();
	}

	protected void verifyAttributes()
			throws BuildException
	{
		if (this.getType() == null)
		{
			this.setType("dm_user");
		}

		if (this.getProperty(USER_NAME) == null)
		{
			throw new BuildException("userName should be specified");
		}

		if (this.getProperty(USER_LOGIN_NAME) == null)
		{
			throw new BuildException("loginName should be specified");
		}

		if (this.getProperty(USER_SOURCE) == null)
		{
			this.addConfiguredProperty(new DocbaseObjectProperty(USER_SOURCE, USER_SOURCE_INLINE));
		}

		if (this.getProperty(USER_SOURCE).getValue().equals(USER_SOURCE_INLINE) &&
			this.getProperty(USER_PASSWORD) == null)
		{
			throw new BuildException("password should be specified");
		}

		if (this.getProperty(CLIENT_CAPABILITY) == null)
		{
			this.addConfiguredProperty(new DocbaseObjectProperty(CLIENT_CAPABILITY, Integer.toString(IDfUser.DF_CAPABILITY_NONE)));
		}

		try
		{
			this.getProperty(CLIENT_CAPABILITY).setDataType(IDfType.DF_INTEGER);
			int capability = ((Integer)this.getProperty(CLIENT_CAPABILITY).resolvePropertyValue(this.getSession())).intValue();
			if (capability < IDfUser.DF_CAPABILITY_NONE ||
				capability > IDfUser.DF_CAPABILITY_SYSTEM_ADMIN)
			{
				throw new BuildException("Invalid client capability specified " + capability);
			}
		}
		catch (NumberFormatException e)
		{
			throw new BuildException("Invalid client capability specified " +
					this.getProperty(CLIENT_CAPABILITY).getValue());
		}

		if (this.getProperty(USER_PRIVILEGES) == null)
		{
			this.addConfiguredProperty(new DocbaseObjectProperty(USER_PRIVILEGES, Integer.toString(IDfUser.DF_PRIVILEGE_NONE)));
		}

		try
		{
			this.getProperty(USER_PRIVILEGES).setDataType(IDfType.DF_INTEGER);
			int privileges = ((Integer)this.getProperty(USER_PRIVILEGES).resolvePropertyValue(this.getSession())).intValue();
			if (privileges < IDfUser.DF_PRIVILEGE_NONE ||
				privileges > IDfUser.DF_PRIVILEGE_SUPERUSER)
			{
				throw new BuildException("Invalid privileges specified " + privileges);
			}
		}
		catch (NumberFormatException e)
		{
			throw new BuildException("Invalid privileges specified " +
					this.getProperty(USER_PRIVILEGES).getValue());
		}

		if (this.getProperty(USER_EXTENDED_PRIVILEGES) == null)
		{
			this.addConfiguredProperty(new DocbaseObjectProperty(USER_EXTENDED_PRIVILEGES, Integer.toString(IDfUser.DF_PRIVILEGE_NONE)));
		}

		try
		{
			this.getProperty(USER_EXTENDED_PRIVILEGES).setDataType(IDfType.DF_INTEGER);
			int privileges = ((Integer)this.getProperty(USER_EXTENDED_PRIVILEGES).resolvePropertyValue(this.getSession())).intValue();
			if (privileges < IDfUser.DF_PRIVILEGE_NONE ||
				privileges > IDfUser.DF_XPRIVILEGE_CONFIG_AUDIT +
						IDfUser.DF_XPRIVILEGE_PURGE_AUDIT + IDfUser.DF_XPRIVILEGE_VIEW_AUDIT)
			{
				throw new BuildException("Invalid extended privileges specified " + privileges);
			}
		}
		catch (NumberFormatException e)
		{
			throw new BuildException("Invalid extended privileges specified " +
					this.getProperty(USER_EXTENDED_PRIVILEGES).getValue());
		}
	}

	protected void createDefaultFolder()
			throws BuildException
	{
		try
		{
			DocbaseObjectProperty folderProperty = this.getProperty(DEFAULT_FOLDER);
			if (folderProperty == null)
			{
				return;
			}

			folderProperty.setDataType(IDfType.DF_STRING);
			String defaultFolder = (String)folderProperty.resolvePropertyValue(this.getSession());
			if (defaultFolder == null)
			{
				return;	
			}

			if (isFolderExist(defaultFolder))
			{
				return;
			}

			String rootPath = defaultFolder.substring(1);
			String folderName = null;
			int index = defaultFolder.lastIndexOf("/");
			if (index > 0)
			{
				rootPath = defaultFolder.substring(0, index);
				folderName = defaultFolder.substring(index + 1);
			}

			IDfSysObject folder;
			if (folderName == null)
			{
				folder = (IDfSysObject)this.createDfObject("dm_cabinet");
				folder.setObjectName(rootPath);
			}
			else
			{
				folder = (IDfSysObject)this.createDfObject("dm_folder");
				folder.setObjectName(folderName);
				folder.link(rootPath);
			}

			this.saveDfObject(folder);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to create default folder", e);
		}
	}

	private boolean isFolderExist(String folder)
	{
		try
		{
			String query = MessageFormat.format(DQL_IS_FOLDER_EXIST, new String[]{folder});
			return DqlHelper.exist(this.getSession(), query);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to verify default folder for existens", e);
		}
	}

	protected void resetDefaultFolderPermits()
			throws BuildException
	{
		DocbaseObjectProperty folderProperty = this.getProperty(DEFAULT_FOLDER);
		if (folderProperty == null ||
			folderProperty.resolvePropertyValue(this.getSession()) == null)
		{
			return;
		}

		String defaultFolder = (String)folderProperty.resolvePropertyValue(this.getSession());
		if (defaultFolder == null)
		{
			return;
		}

		IDfFolder folder;
		try
		{
			folder = this.getSession().getDfSession().getFolderByPath(defaultFolder);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to get folder object " + defaultFolder +
				" from repository", e);
		}

		AclHelper.resetObjectACL(folder);

		try
		{
			folder.setOwnerName((String)this.getProperty(USER_NAME).resolvePropertyValue(this.getSession()));
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to set folder owner", e);
		}

		this.saveDfObject(folder);
	}
}
