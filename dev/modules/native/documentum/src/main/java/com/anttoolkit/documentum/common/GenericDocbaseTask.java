package com.anttoolkit.documentum.common;

import java.util.*;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

public abstract class GenericDocbaseTask
		extends GenericTask
{
	private boolean showObjectInfoBeforeIteration = false;
	private boolean showObjectInfoAfterIteration = false;
	private String batchSuccessIterationsProperty = null;
	private String batchFailedIterationsProperty = null;
	private String batchIterationsProperty = null;

	protected DocbaseSession getSession()
			throws BuildException
	{
		return DocbaseSessionManager.getSession();
	}

	protected IDfPersistentObject createDfObject(String type)
			throws BuildException
	{
		return this.getSession().createDfObject(type);
	}

	protected IDfPersistentObject getDfObject(String objectId)
			throws BuildException
	{
		return this.getSession().getDfObject(objectId);
	}

	public IDfPersistentObject getDfObject(IDfId objectId)
			throws BuildException
	{
		return this.getSession().getDfObject(objectId);
	}

	public void setShowObjectInfoBeforeIteration(boolean show)
	{
		showObjectInfoBeforeIteration = show;
	}

	public void setShowObjectInfoAfterIteration(boolean show)
	{
		showObjectInfoAfterIteration = show;
	}

	public void setBatchIterationsProperty(String propertyName)
	{
		batchIterationsProperty = propertyName;
	}

	public void setBatchSuccessIterationsProperty(String propertyName)
	{
		batchSuccessIterationsProperty = propertyName;
	}

	public void setBatchFailedIterationsProperty(String propertyName)
	{
		batchFailedIterationsProperty = propertyName;
	}

	protected String saveDfObject(IDfPersistentObject obj)
			throws BuildException
	{
		return DocbaseSession.saveDfObject(obj);	
	}

	protected void processObjectsBatch(String batchExpression)
			throws BuildException
	{
		if (batchExpression == null)
		{
			throw new BuildException("Incorrect batch expression specified: " + batchExpression);
		}

		if (batchExpression.length() == 16 && DfId.isObjectId(batchExpression))
		{
			_processSingleObjectFromBatch(0, new DfId(batchExpression));
		}
		else if (batchExpression.startsWith("select"))
		{
			processObjectsBatchFromDqlQuery(batchExpression);
		}
		else
		{
			throw new BuildException("Incorrect batch expression specified: " + batchExpression);
		}
	}

	protected void processObjectsBatchFromDqlQuery(String query)
			throws BuildException
	{
		IDfCollection coll = null;
		try
		{
			try
			{
				coll = DqlHelper.executeReadQuery(getSession(), query);
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to execute query \"" + query + "\" to process objects batch", e);
			}

			int iteration = 0;
			int successIterations = 0;
			int failedIterations = 0;

			List <IDfId> objectIds = new LinkedList<IDfId>();

			log("Collecting object ids...");

			try
			{
				while (coll != null && coll.next())
				{
					IDfId objectId = coll.getValueAt(0).asId();
					objectIds.add(objectId);
				}
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to collect object ids", e);
			}
			finally
			{
				DqlHelper.closeCollection(coll);
			}

			log("Collected " + objectIds.size() + " object ids, start processing...");

			try
			{
				for (IDfId objectId : objectIds)
				{
					if (showObjectInfoBeforeIteration)
					{
						log("Start processing object: " + objectId);
					}

					onBeforeProcessSingleObjectFromBatch(iteration, objectId);
					boolean result = _processSingleObjectFromBatch(iteration, objectId);
					onAfterProcessSingleObjectFromBatch(iteration, objectId);

					if (showObjectInfoAfterIteration)
					{
						log("Finished processing object: " + objectId);
					}

					iteration++;

					if (result)
					{
						successIterations++;
					}
					else
					{
						failedIterations++;
					}
				}
			}
			catch (Throwable e)
			{
				throw new BuildException("Failed to perform " + iteration + " iteration while trying to process objects batch", e);
			}
			finally
			{
				if (batchIterationsProperty != null)
				{
					this.setPropertyThreadSafe(batchIterationsProperty, Integer.toString(iteration));
				}

				if (batchSuccessIterationsProperty != null)
				{
					this.setPropertyThreadSafe(batchSuccessIterationsProperty, Integer.toString(successIterations));
				}

				if (batchFailedIterationsProperty != null)
				{
					this.setPropertyThreadSafe(batchFailedIterationsProperty, Integer.toString(failedIterations));
				}
			}
		}
		finally
		{
			DqlHelper.closeCollection(coll);
		}
	}

	protected void onBeforeProcessSingleObjectFromBatch(int number, IDfId objectId)
			throws BuildException
	{
	}

	protected void processSingleObjectFromBatch(int number, IDfId objectId)
			throws BuildException
	{
	}

	protected void onAfterProcessSingleObjectFromBatch(int number, IDfId objectId)
			throws BuildException
	{
	}

	private boolean _processSingleObjectFromBatch(int iteration, IDfId objectId)
			throws BuildException
	{
		try
		{
			processSingleObjectFromBatch(iteration, objectId);
			return true;
		}
		catch (Throwable e)
		{
			if (!failOnError())
			{
				getProject().log(this, "Handled exception occured during execution of batch object, r_object_id=" + objectId + ", iteration=" + iteration, e, Project.MSG_ERR);
				return false;
			}

			if (e instanceof BuildException)
			{
				throw (BuildException)e;
			}

			if (e instanceof RuntimeException)
			{
				throw (RuntimeException)e;
			}

			throw new BuildException("Exception occured during execution of batch object, r_object_id=" + objectId + ", iteration=" + iteration, e);
		}
	}
}
