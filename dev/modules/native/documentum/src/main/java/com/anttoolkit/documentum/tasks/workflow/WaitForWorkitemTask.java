package com.anttoolkit.documentum.tasks.workflow;

import java.util.*;

import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import com.anttoolkit.documentum.common.*;
import com.anttoolkit.documentum.tasks.workflow.util.*;
import com.anttoolkit.general.tasks.collections.array.util.*;
import com.anttoolkit.general.entities.*;

public class WaitForWorkitemTask
		extends GenericDocbaseTask
		implements IEntityProcessor<List<String>, List<String>, Void>
{
	private String workflowId = null;
	private String activity = null;
	private String performer = null;
	private String performersArray = null;
	private String sleepTimeout = null;
	private String waitTime = null;
	private String workitemIdProperty = null;
	private String workitemIdArray = null;
	private String workitemPerformerProperty = null;
	private String workitemPerformerArray = null;

	public void setWorkflowId(String workflowId)
	{
		this.workflowId = workflowId;
	}

	public void setActivity(String activity)
	{
		this.activity = activity;
	}

	public void setPerformer(String performer)
	{
		this.performer = performer;
	}

	public void setPerformersArray(String performersArray)
	{
		this.performersArray = performersArray;
	}

	public void setSleepTimeout(String sleepTimeout)
	{
		this.sleepTimeout = sleepTimeout;
	}

	public void setWaitTime(String waitTime)
	{
		this.waitTime = waitTime;
	}

	public void setWorkitemIdProperty(String workitemIdProperty)
	{
		this.workitemIdProperty = workitemIdProperty;
	}

	public void setWorkitemIdArray(String workitemIdArray)
	{
		this.workitemIdArray = workitemIdArray;
	}

	public void setWorkitemPerformerProperty(String property)
	{
		workitemPerformerProperty = property;
	}

	public void setWorkitemPerformerArray(String performerArray)
	{
		workitemPerformerArray = performerArray;
	}

	public void doWork() throws BuildException
	{
		long startTime = System.currentTimeMillis();
		long currentTime = startTime;
		long waitTime = getWaitTime();
		long sleepTimeout = getSleepTimeout();

		List<String> performersToWaitFor = getPerformersToWaitItemsFor();
		Map<String, List<String>> detectedPerformerWorkitems = new HashMap<String, List<String>>();

		while (currentTime - startTime < waitTime)
		{
			//try to find existing workitems
			if (performersToWaitFor.isEmpty())
			{
				List<IDfWorkitem> workitems = Workflow.getQueuedWorkitems(getSession(), workflowId, activity, null);
				appendDetectedPerformerWorkitems(workitems, detectedPerformerWorkitems);
			}
			else
			{
				List<String> detectedPerformers = new LinkedList<String>();

				for (String performer : performersToWaitFor)
				{
					List<IDfWorkitem> workitems = Workflow.getQueuedWorkitems(getSession(), workflowId, activity, performer);
					if (workitems != null && !workitems.isEmpty())
					{
						appendDetectedPerformerWorkitems(workitems, detectedPerformerWorkitems);
						detectedPerformers.add(performer);
					}
				}

				performersToWaitFor.removeAll(detectedPerformers);
			}

			//check if all workitems were found
			if (performersToWaitFor.isEmpty() && !detectedPerformerWorkitems.isEmpty())
			{
				break;
			}

			//wait before starting next cycle
			try
			{
				Thread.sleep(sleepTimeout);
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Wait cycle was interrupted", e);
			}

			currentTime = System.currentTimeMillis();
		}

		//all workitems were found
		if (performersToWaitFor.isEmpty() && !detectedPerformerWorkitems.isEmpty())
		{
			initVariables(detectedPerformerWorkitems);
			return;
		}

		//throw exceptions
		if (performersToWaitFor.isEmpty())
		{
			throw new BuildException("Wait time exceed " + waitTime + "ms, but no workitems were detected");
		}

		StringBuilder notDetectedPerformers = new StringBuilder();
		for (String performer : performersToWaitFor)
		{
			if (notDetectedPerformers.length() != 0)
			{
				notDetectedPerformers.append(", ");
			}

			notDetectedPerformers.append(performer);
		}

		StringBuilder detectedPerformers = new StringBuilder();
		for (String performer : detectedPerformerWorkitems.keySet())
		{
			if (detectedPerformers.length() != 0)
			{
				detectedPerformers.append(", ");
			}

			detectedPerformers.append(performer);
		}

		throw new BuildException("Wait time exceed " + waitTime + "ms, " +
				"but no workitems were detected for performers: " + notDetectedPerformers.toString() +
				". Only workitems for performers detected: " + detectedPerformers.toString());
	}

	@Override
	public Void processEntity(List<String> data, List<String> performers)
	{
		for (String item : data)
		{
			performers.add(item);
		}

		return null;
	}

	@Override
	public boolean readOnly()
	{
		return true;
	}

	protected void validate()
	{
		if (workflowId == null)
		{
			throw new BuildException("Workflow id should be specified");
		}

		if (activity == null)
		{
			throw new BuildException("Activity name should be specified");
		}
	}

	private void initVariables(Map<String, List<String>> detectedPerformerWorkitems)
	{
		if (workitemIdProperty == null && workitemIdArray == null &&
			workitemPerformerProperty == null && workitemPerformerArray == null)
		{
			return;
		}

		int iteration = 0;
		Set<String> performers = detectedPerformerWorkitems.keySet();

		for (String performer : performers)
		{
			List<String> workitems = detectedPerformerWorkitems.get(performer);

			if (iteration == 0 && workitemIdProperty != null)
			{
				setPropertyThreadSafe(workitemIdProperty, workitems.get(0));
			}

			if (iteration == 0 && workitemPerformerProperty != null)
			{
				setPropertyThreadSafe(workitemPerformerProperty, performer);
			}

			for (String workitemId : workitems)
			{
				if (workitemIdArray != null)
				{
					boolean contains = ArrayManager.contains(workitemIdArray, workitemId);

					if (!contains)
					{
						ArrayManager.add(workitemIdArray, workitemId);
					}
					else
					{
						continue;
					}
				}

				if (workitemPerformerArray != null)
				{
					ArrayManager.add(workitemPerformerArray, performer);
				}
			}

			iteration++;
		}
	}

	private void appendDetectedPerformerWorkitems(List<IDfWorkitem> workitems, Map<String, List<String>> detectedPerformerWorkitems)
	{
		if (workitems == null || workitems.isEmpty())
		{
			return;
		}

		for (IDfWorkitem workitem : workitems)
		{
			String performer = null;
			String workitemId = null;

			try
			{
				int state = workitem.getRuntimeState();
				if (state != IDfWorkitem.DF_WI_STATE_DORMANT &&
					state != IDfWorkitem.DF_WI_STATE_ACQUIRED)
				{
					continue;
				}

				performer = workitem.getPerformerName();
				workitemId = workitem.getObjectId().getId();
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to get workitem info", e);
			}

			if (!detectedPerformerWorkitems.containsKey(performer))
			{
				List<String> list = new LinkedList<String>();
				list.add(workitemId);
				detectedPerformerWorkitems.put(performer, list);
				continue;
			}

			List<String> list = detectedPerformerWorkitems.get(performer);
			if (!list.contains(workitemId))
			{
				list.add(workitemId);
			}
		}
	}

	private long getSleepTimeout()
	{
		if (sleepTimeout == null)
		{
			return 1000;	// 1 second by default
		}

		return parseTimeout(sleepTimeout);
	}

	private long getWaitTime()
	{
		if (waitTime == null)
		{
			return 300000;	// 5 minutes by default
		}

		return parseTimeout(waitTime);
	}

	private long parseTimeout(String timeout)
	{
		try
		{
			if (timeout.endsWith("s"))
			{
				return Long.parseLong(timeout.substring(0, timeout.length() - 1)) * 1000;
			}
			else if (timeout.endsWith("m"))
			{
				return Long.parseLong(timeout.substring(0, timeout.length() - 1)) * 60000;
			}
			else if (timeout.endsWith("h"))
			{
				return Long.parseLong(timeout.substring(0, timeout.length() - 1)) * 3600000;
			}
			else if (timeout.endsWith("d"))
			{
				return Long.parseLong(timeout.substring(0, timeout.length() - 1)) * 86400000;
			}

			return Long.parseLong(timeout.substring(0, timeout.length()));
		}
		catch (NumberFormatException e)
		{
			throw new BuildException("Invalid timeout specification: " + timeout, e);
		}
	}

	private List<String> getPerformersToWaitItemsFor()
	{
		List<String> performers = new LinkedList<String>();

		if (performer != null)
		{
			String[] performerNames = performer.split(",", -1);
			for (String name : performerNames)
			{
				performers.add(name.trim());
			}
		}

		if (performersArray != null)
		{
			try
			{
				EntityManager.processEntity(ArrayEntityType.instance, performersArray, this, performers);
			}
			catch (EntityNotFoundException e)
			{
				throw new BuildException("Array " + performersArray + " wasn't previously initialized", e);
			}
		}

		return performers;
	}
}
