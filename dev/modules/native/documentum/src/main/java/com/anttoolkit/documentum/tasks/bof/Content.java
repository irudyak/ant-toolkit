package com.anttoolkit.documentum.tasks.bof;

import com.anttoolkit.documentum.common.*;

import com.documentum.fc.common.*;

import org.apache.tools.ant.*;

public class Content
		extends DocAppObject
{
	private boolean isCheckedOut = false;
	private boolean wasUpdated = false;

	private String localFile = null;

	public static Content instance(String objectId,
								   String name,
								   String objectType,
								   DocbaseSession session,
								   String localFile)
			throws BuildException
	{
		return new Content(objectId, name, objectType, session, localFile);
	}

	private Content(String objectId,
					String name,
					String type,
					DocbaseSession session,
					String localFile)
	{
		super(objectId, name, type, session);
		this.localFile = localFile;
	}

	public void checkOut()
			throws CheckOutException
	{
		if (isCheckedOut || localFile == null)
		{
			return;
		}

		try
		{
			if (!this.isCheckedOutByMe(this.getDfObject()))
			{
				this.getDfObject().checkout();
			}
		}
		catch (DfException e)
		{
			throw new CheckOutException(this.getName(), this.getType(), e);
		}

		isCheckedOut = true;
		wasUpdated = false;
	}

	public void cancelCheckOut()
	{
		if (!isCheckedOut)
		{
			return;
		}

		try
		{
			this.getDfObject().cancelCheckout();
		}
		catch (DfException e)
		{
			System.out.println("Failed to cancel checkout object of type=" + this.getType() +
					" with name=" + this.getName() + " and r_object_id=" + this.getObjectId() +
					"\r\n" + e.toString());
		}

		isCheckedOut = false;
	}

	public void update()
			throws UpdateException
	{
		if (wasUpdated || localFile == null)
		{
			isCheckedOut = false;
			return;
		}

		try
		{
			this.getDfObject().setFile(localFile);
			this.getDfObject().checkin(false, null);
		}
		catch (DfException e)
		{
			throw new UpdateException(this.getName(), this.getType(), e);
		}
		finally
		{
			wasUpdated = true;
		}

		isCheckedOut = false;
	}
}
