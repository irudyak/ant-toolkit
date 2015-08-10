package com.anttoolkit.documentum.tasks.workflow;

import java.util.*;

import org.apache.tools.ant.*;

import com.documentum.fc.client.*;
import com.documentum.fc.common.*;

import com.anttoolkit.documentum.common.*;
import com.anttoolkit.documentum.tasks.workflow.util.*;
import com.anttoolkit.general.entities.*;

public abstract class GenericWorkflowTask
		extends GenericDocbaseTask
		implements IEntityProcessor<List<String>, List<String>, Void>
{
	private List<Package> packages = new LinkedList<Package>();
	private List<Alias> aliases = new LinkedList<Alias>();
	private List<Variable> variables = new LinkedList<Variable>();
	private List<Attachment> attachments = new LinkedList<Attachment>();
	private List<Activity> activities = new LinkedList<Activity>();
	private List<CompleteWorkitem> workitemsToComplete = new LinkedList<CompleteWorkitem>();
	private List<PostEvent> events = new LinkedList<PostEvent>();

	private Supervisor supervisor = new Supervisor();

	public class Package
	{
		private String name = null;
		private String objectId = null;

		public void setName(String name)
		{
			this.name = name;
		}

		public String getName()
		{
			return name;
		}

		public void setObjectId(String id)
		{
			objectId = id;
		}

		public String getObjectId()
		{
			return objectId;
		}
	}

	public class Alias
	{
		private String name = null;
		private String value = null;

		public void setName(String name)
		{
			this.name = name;
		}

		public String getName()
		{
			return name;
		}

		public void setValue(String value)
		{
			this.value = value;
		}

		public String getValue()
		{
			return value;
		}
	}

	public class Variable
	{
		private String name = null;
		private String value = null;
		private String timeFormat = null;

		public void setName(String name)
		{
			this.name = name;
		}

		public String getName()
		{
			return name;
		}

		public void setValue(String value)
		{
			this.value = value;
		}

		public String getValue()
		{
			return value;
		}

		public void setFormat(String format)
		{
			timeFormat = format;
		}

		public String getFormat()
		{
			return timeFormat;
		}
	}

	public class Attachment
	{
		private String type = null;
		private String objectId = null;
		private boolean remove = false;

		public void setType(String type)
		{
			this.type = type;
		}

		public String getType()
		{
			return type;
		}

		public void setObjectId(String id)
		{
			objectId = id;
		}

		public String getObjectId()
		{
			return objectId;
		}

		public void setRemove(boolean remove)
		{
			this.remove = remove;
		}

		public boolean getRemove()
		{
			return remove;
		}
	}

	public class Activity
	{
		private String name = null;
		private String performer = null;
		private String performersArray = null;

		public void setName(String name)
		{
			this.name = name;
		}

		public String getName()
		{
			return name;
		}

		public void setPerformer(String performer)
		{
			this.performer = performer;
		}

		public String getPerformer()
		{
			return performer;
		}

		public void setPerformersArray(String performersArray)
		{
			this.performersArray = performersArray;
		}

		public String getPerformersArray()
		{
			return performersArray;
		}
	}

	public class CompleteWorkitem
	{
		private String activity = null;
		private String nextActivity = null;
		private String performer = null;

		public void setActivity(String activity)
		{
			this.activity = activity;
		}

		public void setNextActivity(String activity)
		{
			nextActivity = activity;
		}

		public void setForPerformer(String performer)
		{
			this.performer = performer;
		}

		public void complete(Workflow workflow)
		{
			String[] targetActivityNames = nextActivity == null ? null : nextActivity.split(",", -1);
			DfList targetActivities = new DfList();

			List<IDfWorkitem> workitems = workflow.getQueuedWorkitems(GenericWorkflowTask.this.getSession(), activity, performer);

			if (workitems == null || workitems.isEmpty())
			{
				throw new BuildException("There are no workitems for workflow [" + workflow.getWorkflowId() +
					"] activity: " + activity);
			}

			if (targetActivityNames != null)
			{
				for (String activityName : targetActivityNames)
				{
					try
					{
						IDfActivity activity = Workflow.getNextActivity(workitems.get(0), activityName.trim());
						if (activity != null)
						{
							targetActivities.append(activity);
						}
					}
					catch (DfException e)
					{
						throw new BuildException("Failed to get next activity '" + activityName + "' for activity '" +
								activity + "' in workflow [" + workflow.getWorkflowId().getId() + "]", e);
					}
				}
			}

			if (targetActivityNames != null && targetActivities.getCount() == 0)
			{
				throw new BuildException("There are no next activities for activity '" + activity +
						"' in workflow [" + workflow.getWorkflowId().getId() + "]");
			}

			for (IDfWorkitem workitem : workitems)
			{
				try
				{
					if (workitem.getRuntimeState() == IDfWorkitem.DF_WI_STATE_DORMANT)
					{
						workitem.acquire();
					}
				}
				catch (DfException e)
				{
					throw new BuildException("Failed to acquire workflow [" + workflow.getWorkflowId().getId()
							+ "] workitem form activity: " + activity, e);
				}

				try
				{
					if (targetActivityNames != null)
					{
						workitem.setOutputByActivities(targetActivities);
					}
				}
				catch (DfException e)
				{
					throw new BuildException("Failed to set next activities for workflow [" + workflow.getWorkflowId().getId()
							+ "] activity: " + activity, e);
				}

				try
				{
					if (workitem.getRuntimeState() == IDfWorkitem.DF_WI_STATE_ACQUIRED)
					{
						workitem.complete();
					}
				}
				catch (DfException e)
				{
					throw new BuildException("Failed to complete workflow [" + workflow.getWorkflowId().getId()
							+ "] workitem form activity: " + activity, e);
				}
			}
		}
	}

	public class Supervisor
	{
		private String name = null;

		public void setName(String name)
		{
			this.name = name;
		}

		public String getName()
		{
			return name;
		}
	}

	public class PostEvent
	{
		private String event = null;
		private String message = null;
		private boolean sendMail = false;

		public void setEvent(String event)
		{
			this.event = event;
		}

		public void setMessage(String message)
		{
			this.message = message;
		}

		public void setSendMail(boolean send)
		{
			sendMail = send;
		}

		public void postEvent(Workflow workflow)
		{
			workflow.postEvent(event, 0, sendMail, new DfTime(), message);
		}
	}

	public Package createPackage()
	{
		Package pack = new Package();
		packages.add(pack);
		return pack;
	}

	public Alias createAlias()
	{
		Alias alias = new Alias();
		aliases.add(alias);
		return alias;
	}

	public Variable createVariable()
	{
		Variable variable = new Variable();
		variables.add(variable);
		return variable;
	}

	public Attachment createAttachment()
	{
		Attachment attachment = new Attachment();
		attachments.add(attachment);
		return attachment;
	}

	public Activity createActivity()
	{
		Activity activity = new Activity();
		activities.add(activity);
		return activity;
	}

	public Supervisor createSupervisor()
	{
		return supervisor;
	}

	public CompleteWorkitem createCompleteWorkitem()
	{
		CompleteWorkitem workitem = new CompleteWorkitem();
		workitemsToComplete.add(workitem);
		return workitem;
	}

	public PostEvent createPostEvent()
	{
		PostEvent event = new PostEvent();
		events.add(event);
		return event;
	}

	@Override
	public Void processEntity(List<String> data, List<String> performerNames)
	{
		for (String name : data)
		{
			if (!performerNames.contains(name.trim()))
			{
				performerNames.add(name.trim());
			}
		}

		return null;
	}

	@Override
	public boolean readOnly()
	{
		return true;
	}

	protected List<Package> getPackages()
	{
		return packages;
	}

	protected List<Alias> getAliases()
	{
		return aliases;
	}

	protected List<Variable> getVariables()
	{
		return variables;
	}

	protected List<Attachment> getAttachments()
	{
		return attachments;
	}

	protected List<Activity> getActivities()
	{
		return activities;
	}

	protected void initWorkflowSettings(Workflow workflow)
	{
		if (workflow == null)
		{
			return;
		}

		initAliases(workflow);
		initVariables(workflow);
		initAttachments(workflow);
		initActivities(workflow);
		initSupervisor(workflow);
	}

	protected void completeWorkitems(Workflow workflow)
	{
		for (CompleteWorkitem workitem : workitemsToComplete)
		{
			workitem.complete(workflow);
		}
	}

	protected void postEvents(Workflow workflow)
	{
		for (PostEvent event : events)
		{
			event.postEvent(workflow);
		}
	}

	private void initAliases(Workflow workflow)
	{
		List<Alias> aliases = getAliases();
		for (Alias alias : aliases)
		{
			workflow.setAliasValue(alias.getName(), alias.getValue());
		}
	}

	private void initVariables(Workflow workflow)
	{
		List<Variable> variables = getVariables();
		for (Variable variable : variables)
		{
			workflow.setVariable(variable.getName(), variable.getValue(), variable.getFormat());
		}
	}

	private void initAttachments(Workflow workflow)
	{
		List<Attachment> attachments = getAttachments();
		for (Attachment attachment : attachments)
		{
			if (attachment.getRemove())
			{
				workflow.removeAttachment(attachment.getObjectId());
			}
			else
			{
				workflow.addAttachment(attachment.getType(), attachment.getObjectId());
			}
		}
	}

	private void initActivities(Workflow workflow)
	{
		List<Activity> activities = getActivities();
		for (Activity activity : activities)
		{
			List<String> performerNames = new LinkedList<String>();

			if (activity.getPerformer() != null)
			{
				String[] names = activity.getPerformer().split(",", -1);
				for (String name : names)
				{
					if (!performerNames.contains(name.trim()))
					{
						performerNames.add(name.trim());
					}
				}
			}
			else
			{
				try
				{
					EntityManager.processEntity(ArrayEntityType.instance, activity.getPerformersArray(), this, performerNames);
				}
				catch (EntityNotFoundException e)
				{
					throw new BuildException("Array " + activity.getPerformersArray() + " wasn't previously initialized", e);
				}
			}

			workflow.setActivityPerformers(activity.getName(), performerNames);
		}
	}

	private void initSupervisor(Workflow workflow)
	{
		workflow.setSupervisor(supervisor.getName());
	}
}