package com.anttoolkit.documentum.tasks.bof;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;
import com.anttoolkit.documentum.common.*;

import org.apache.tools.ant.*;

import java.text.*;
import java.util.*;

public abstract class BasicDocAppObject
		extends DocAppObject
{
	private static final String DQL_GET_APP_REF_OBJECT = "select r_object_id " +
			"from dm_app_ref where application_obj_id = ''{0}''";

	private static final String DQL_REMOVE_PREVIOUS_VERSIONS = "delete dm_sysobject (ALL) object " +
			"where r_object_id in (select r_object_id from dm_sysobject (ALL) " +
			"where folder(ID(''{0}'')) and r_object_type in (''dm_document'', ''dmc_jar'') " +
			"and not (any r_version_label=''CURRENT''))";

	private boolean isAppRefCheckedOut = false;
	private boolean isContentCheckedOut = false;
	private boolean wasAppRefUpdated = false;
	private boolean wasContentUpdated = false;

	private IDfSysObject appRef = null;
	private boolean triedToLoadAppRef = false;

	private boolean isContentLoaded = false;
	private List<DocAppObject> content = new LinkedList<DocAppObject>();
	private IContentStorage storage = null;

	public BasicDocAppObject(String objectId,
							 String name,
							 String type,
							 DocbaseSession session,
							 IContentStorage storage)
	{
		super(objectId, name, type, session);
		this.storage = storage;
	}

	public void checkOut()
			throws CheckOutException
	{
		chechOutAppRef();
		loadContent();
		checkOutContent();
	}

	public void cancelCheckOut()
	{
		cancelAppRefCheckOut();
		cancelContentCheckOut();
	}

	public void update()
			throws UpdateException
	{
		updateContent();
		updateAppRef();
	}

	public void removePreviousVersions()
			throws BuildException
	{
		String query = MessageFormat.format(DQL_REMOVE_PREVIOUS_VERSIONS, new String[] {this.getObjectId()});

		try
		{
			DqlHelper.executeQuery(this.getSession(), query);
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to remove previous versions of BOF objects for " +
					this.getReadableTypeName() + " \"" + this.getName() + "\"", e);
		}
	}

	protected abstract String getReadableTypeName();

	protected abstract String contentQueryTemplate();

	protected Iterator contentIterator()
	{
		return content.iterator();
	}

	private void loadContent()
			throws BuildException
	{
		if (isContentLoaded)
		{
			return;
		}

		IDfCollection coll = null;
		try
		{
			String query = MessageFormat.format(contentQueryTemplate(), new String[] {this.getObjectId()});
			coll = DqlHelper.executeReadQuery(this.getSession(), query);

			while (coll.next())
			{
				this.putContent(coll.getString(OBJECT_ID_ATTRIBUTE),
								coll.getString(NAME_ATTRIBUTE),
								coll.getString(TYPE_ATTRIBUTE));
			}
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to load content for " + this.getReadableTypeName() +
					" " + this.getName(), e);
		}
		finally
		{
			DqlHelper.closeCollection(coll);
		}

		isContentLoaded = true;
	}

	private void putContent(String objectId,
							  String name,
							  String type)
	{
		content.add(storage.putContent(objectId, name, type));
	}

	private void chechOutAppRef()
			throws CheckOutException
	{
		if (isAppRefCheckedOut)
		{
			return;
		}

		try
		{
			if (this.getAppRef() != null && !this.isCheckedOutByMe(this.getAppRef()))
			{
				this.getAppRef().checkout();
			}
		}
		catch (DfException e)
		{
			throw new CheckOutException(this.getName(), this.getReadableTypeName(), e);
		}

		isAppRefCheckedOut = true;
		wasAppRefUpdated = false;
	}

	private void checkOutContent()
			throws CheckOutException
	{
		if (isContentCheckedOut)
		{
			return;
		}

		for (DocAppObject _content : content)
		{
			Content obj = (Content)_content;

			try
			{
				obj.checkOut();
			}
			catch (CheckOutException e)
			{
				e.setParentObjectInfo(this.getName(), this.getReadableTypeName());
				throw e;
			}
		}

		isContentCheckedOut = true;
		wasContentUpdated = false;
	}

	private void cancelAppRefCheckOut()
	{
		if (!isAppRefCheckedOut)
		{
			return;
		}

		try
		{
			if (this.getAppRef() != null)
			{
				this.getAppRef().cancelCheckout();
			}
		}
		catch (DfException e)
		{
			System.out.println("Failed to cancel checkout object of type=" + this.getType() +
					" with name=" + this.getName() + " and r_object_id=" + this.getObjectId() +
					"\r\n" + e.toString());
		}

		isAppRefCheckedOut = false;
	}

	private void cancelContentCheckOut()
	{
		if (!isContentCheckedOut)
		{
			return;
		}

		for (DocAppObject _сontent : content)
		{
			Content obj = (Content)_сontent;
			obj.cancelCheckOut();
		}

		isContentCheckedOut = false;
	}

	private void updateAppRef()
			throws UpdateException
	{
		if (wasAppRefUpdated)
		{
			isAppRefCheckedOut = false;
			return;
		}

		try
		{
			if (this.getAppRef() != null)
			{
				this.getAppRef().save();
			}
		}
		catch (DfException e)
		{
			throw new UpdateException(this.getName(), this.getReadableTypeName(), e);
		}

		wasAppRefUpdated = true;
		isAppRefCheckedOut = false;
	}

	private void updateContent()
			throws UpdateException
	{
		if (wasContentUpdated)
		{
			isContentCheckedOut = false;
			return;
		}

		for (DocAppObject _сontent : content)
		{
			Content obj = (Content)_сontent;

			try
			{
				obj.update();
			}
			catch (UpdateException e)
			{
				e.setParentObjectInfo(this.getName(), this.getReadableTypeName());
				throw e;
			}
		}

		wasContentUpdated = true;
		isContentCheckedOut = false;
	}

	private IDfSysObject getAppRef()
			throws DfException
	{
		if (triedToLoadAppRef)
		{
			return appRef;
		}

		try
		{
			String query = MessageFormat.format(DQL_GET_APP_REF_OBJECT, new String[] {this.getObjectId()});
			String appRefId = DqlHelper.getStringParamFromFirstString(this.getSession(), query);
			appRef = (IDfSysObject)this.getSession().getDfObject(appRefId);
		}
		catch (DfEndOfCollectionException e)
		{
		}
		finally
		{
			triedToLoadAppRef = true;
		}

		return appRef;
	}
}
