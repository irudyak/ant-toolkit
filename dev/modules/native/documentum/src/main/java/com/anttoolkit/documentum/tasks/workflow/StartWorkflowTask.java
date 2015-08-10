package com.anttoolkit.documentum.tasks.workflow;

import com.anttoolkit.documentum.tasks.workflow.util.Workflow;
import com.documentum.fc.common.DfId;
import org.apache.tools.ant.BuildException;

import java.text.SimpleDateFormat;
import java.util.*;

public class StartWorkflowTask
	extends GenericWorkflowTask
{
	private static final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat("dd.MM.yyyy");

	private String template = null;
	private String parentId = null;
	private String workflowName = null;
	private String newWorkflowIdProperty = null;

	public void setTemplate(String template)
	{
		this.template = template;
	}

	public void setParentId(String id)
	{
		parentId = id;
	}

	public void setWorkflowName(String name)
	{
		workflowName = name;
	}

	public void setNewWorkflowIdProperty(String property)
	{
		newWorkflowIdProperty = property;
	}

	public void doWork() throws BuildException
	{
		if (workflowName == null)
		{
			workflowName = template + ", " + DATE_FORMATTER.format(new Date());
		}

		List<Package> packages = getPackages();
		Map<String, List<String>> packageDocuments = new HashMap<String, List<String>>();

		for (Package _package : packages)
		{
			List<String> ids = new LinkedList<String>();
			ids.add(_package.getObjectId());
			packageDocuments.put(_package.getName(), ids);
		}

		Workflow workflow = parentId == null ?
				Workflow.newInstance(getSession(), template) :
				Workflow.newInstance(getSession(), template, new DfId(parentId));

		initWorkflowSettings(workflow);

		workflow.startWorkflow(getSession(), workflowName, packageDocuments);

		if (newWorkflowIdProperty != null)
		{
			this.setPropertyThreadSafe(newWorkflowIdProperty, workflow.getWorkflowId().getId());
		}
	}

	protected void validate()
	{
		if (template == null)
		{
			throw new BuildException("Workflow template should be specified");
		}

		if (getPackages().isEmpty())
		{
			throw new BuildException("No packages specified");
		}
	}
}
