package com.anttoolkit.documentum.tasks.workflow;

import com.anttoolkit.documentum.tasks.workflow.util.*;

import com.documentum.fc.common.*;

import org.apache.tools.ant.*;

public class UpdateWorkflowTask
		extends GenericWorkflowTask
{
	private String workflowId = null;

	public void setWorkflowId(String id)
	{
		workflowId = id;
	}

	public void doWork() throws BuildException
	{
		processObjectsBatch(workflowId);
	}

	protected void validate()
	{
		if (workflowId == null)
		{
			throw new BuildException("Workflow id should be specified");
		}
	}

	protected void processSingleObjectFromBatch(int iteration, IDfId workflowId)
			throws BuildException
	{
		Workflow workflow = Workflow.getInstance(getSession(), new DfId(this.workflowId));

		initWorkflowSettings(workflow);

		workflow.save(getSession());

		completeWorkitems(workflow);
		postEvents(workflow);
	}
}
