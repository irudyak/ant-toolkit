package com.anttoolkit.documentum.tasks.workflow;

import java.text.MessageFormat;
import java.util.*;

import org.apache.tools.ant.*;

import com.documentum.bpm.sdt.*;
import com.documentum.fc.common.*;

import com.anttoolkit.documentum.tasks.workflow.util.*;

import com.anttoolkit.documentum.common.*;

public class GetWorkflowInfoTask
	extends GenericDocbaseTask
{
	private List<Variable> variables = new LinkedList<Variable>();
	private List<Alias> aliases = new LinkedList<Alias>();
	private List<Package> packages = new LinkedList<Package>();

	private String workflowId = null;

	public class Variable
	{
		private String name = null;
		private String property = null;
		private String format = null;

		public void setName(String name)
		{
			this.name = name;
		}

		public void setProperty(String property)
		{
			this.property = property;
		}

		public void setFormat(String format)
		{
			this.format = format;
		}

		public void setPropertyValue(Workflow workflow)
		{
			IDfPrimitiveType type = workflow.getVariableType(name);

			if (IDfPrimitiveType.BOOLEAN.equals(type))
			{
				GetWorkflowInfoTask.this.setPropertyThreadSafe(property, Boolean.toString(workflow.getBooleanVariable(name)));
			}
			else if (IDfPrimitiveType.STRING.equals(type))
			{
				GetWorkflowInfoTask.this.setPropertyThreadSafe(property, workflow.getStringVariable(name));
			}
			else if (IDfPrimitiveType.INT.equals(type))
			{
				GetWorkflowInfoTask.this.setPropertyThreadSafe(property, Integer.toString(workflow.getIntVariable(name)));
			}
			else if (IDfPrimitiveType.FLOAT.equals(type))
			{
				GetWorkflowInfoTask.this.setPropertyThreadSafe(property, Double.toString(workflow.getDoubleVariable(name)));
			}
			else if (IDfPrimitiveType.DATE.equals(type))
			{
				IDfTime time = workflow.getTimeVariable(name);
				GetWorkflowInfoTask.this.setPropertyThreadSafe(property, time.asString(format != null ? format : "MM/dd/yyyy hh:mm:ss"));
			}
		}
	}

	public class Alias
	{
		private String name = null;
		private String property = null;

		public void setName(String name)
		{
			this.name = name;
		}

		public void setProperty(String property)
		{
			this.property = property;
		}

		public void setPropertyValue(Workflow workflow)
		{
			String value = workflow.getAliasValue(GetWorkflowInfoTask.this.getSession(), name);
			GetWorkflowInfoTask.this.setPropertyThreadSafe(property, value);
		}
	}

	public class Package
	{
		private static final String DQL_GET_WORKFLOW_PACKAGE_OBJECT = "select r_component_id " +
				"from dmi_package " +
				"where r_workflow_id = ''{0}'' and r_package_name=''{1}'' " +
				"order by i_acceptance_date desc enable(return_top 1)";

		private String name = null;
		private String objectIdProperty = null;

		public void setName(String name)
		{
			this.name = name;
		}

		public void setObjectIdProperty(String property)
		{
			objectIdProperty = property;
		}

		public void setProperty(Workflow workflow)
		{
			if (name == null || objectIdProperty == null)
			{
				throw new BuildException("Package name and objectId property name should be specified");
			}

			String query = MessageFormat.format(DQL_GET_WORKFLOW_PACKAGE_OBJECT, workflow.getWorkflowId().getId(), name);

			try
			{
				String objectId = DqlHelper.getStringParamFromFirstString(GetWorkflowInfoTask.this.getSession(), query);
				GetWorkflowInfoTask.this.setPropertyThreadSafe(objectIdProperty, objectId);
			}
			catch (DfException e)
			{
				throw new BuildException("Failed to get objectId for workflow [" +
						workflow.getWorkflowId().getId() + "] package: " + name, e);
			}
			catch (DfEndOfCollectionException e)
			{
				throw new BuildException("No package '" + name + "' found for workflow [" +
						workflow.getWorkflowId().getId() + "]", e);
			}
		}
	}

	public Variable createVariable()
	{
		Variable variable = new Variable();
		variables.add(variable);
		return variable;
	}

	public Alias createAlias()
	{
		Alias alias = new Alias();
		aliases.add(alias);
		return alias;
	}

	public Package createPackage()
	{
		Package _package = new Package();
		packages.add(_package);
		return _package;
	}

	public void setWorkflowId(String id)
	{
		workflowId = id;
	}

	public void doWork() throws BuildException
	{
		if (variables.isEmpty() && aliases.isEmpty() && packages.isEmpty())
		{
			return;
		}

		Workflow workflow = Workflow.getInstance(getSession(), new DfId(workflowId));

		for (Variable variable : variables)
		{
			variable.setPropertyValue(workflow);
		}

		for (Alias alias : aliases)
		{
			alias.setPropertyValue(workflow);
		}

		for (Package _package : packages)
		{
			_package.setProperty(workflow);
		}
	}

	protected void validate()
	{
		if (workflowId == null)
		{
			throw new BuildException("Workflow id should be specified");
		}
	}
}
