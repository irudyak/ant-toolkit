package com.anttoolkit.documentum.tasks.workflow;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import org.apache.tools.ant.*;

import com.anttoolkit.documentum.tasks.workflow.util.*;
import com.anttoolkit.documentum.common.*;

public class CompleteWorkitemTask
	extends GenericDocbaseTask
{
	private String workitemId = null;
	private String nextActivity = null;

	public void setWorkitemId(String workitem)
	{
		workitemId = workitem;
	}

	public void setNextActivity(String activity)
	{
		nextActivity = activity;
	}

	public void doWork() throws BuildException
	{
		processObjectsBatch(workitemId);
	}

	protected void validate()
	{
		if (workitemId == null)
		{
			throw new BuildException("Workitem id should be specified");
		}
	}

	protected void processSingleObjectFromBatch(int iteration, IDfId workitemId)
			throws BuildException
	{
		try
		{
			IDfWorkitem workitem = (IDfWorkitem)getSession().getDfObject(workitemId);

			if (workitem.getRuntimeState() == IDfWorkitem.DF_WI_STATE_DORMANT)
			{
				workitem.acquire();
			}

			if (workitem.getRuntimeState() != IDfWorkitem.DF_WI_STATE_ACQUIRED)
			{
				throw new BuildException("Can't complete workitem [" + workitemId.getId() + "] " +
						"because its state=" + workitem.getRuntimeState() + " (not DORMANT & not ACQUIRED)");
			}

			IDfList nextActivities = null;
			if (nextActivity != null)
			{
				String[] activityNames = nextActivity == null ? null : nextActivity.split(",", -1);
				nextActivities = getNextActivities(workitem, activityNames);
			}

			if (nextActivities != null)
			{
				workitem.setOutputByActivities(nextActivities);
			}

			workitem.complete();
		}
		catch (DfException e)
		{
			throw new BuildException("Failed to complete workitem, " + workitemId, e);
		}
	}

	private DfList getNextActivities(IDfWorkitem workitem, String... activityNames)
	{
		DfList activities = new DfList();

		for (String activityName : activityNames)
		{
			try
			{
				IDfActivity activity = Workflow.getNextActivity(workitem, activityName);
				if (activity != null)
				{
					activities.append(activity);
				}
			}
			catch (DfException e)
			{
				try
				{
					throw new BuildException("Failed to get next activity '" + activityName + "' for workitem '" +
							workitem.getObjectId().toString(), e);
				}
				catch (DfException ex){}
			}
		}

		return activities.getCount() == 0 ? null : activities;
	}
}
