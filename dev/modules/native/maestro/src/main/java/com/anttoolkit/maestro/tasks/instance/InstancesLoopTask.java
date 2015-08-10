package com.anttoolkit.maestro.tasks.instance;

import java.util.*;

import com.anttoolkit.maestro.tasks.utils.*;
import com.anttoolkit.maestro.common.*;
import com.maestro.cli.entity.SystemExitException;
import org.apache.tools.ant.BuildException;

@Action("describe-instances")
public class InstancesLoopTask extends XmlItemLoopTask
{
	@Param
	private Set<String> instances = new HashSet<String>();

	@Param("filter-by-instance-state")
	private List<String> filterStates = new LinkedList<String>();

	@Param(value = "filter-by-props", keyValueList = true)
	private List<String> filterProperties = new LinkedList<String>();

	@Param("filter-by-props-existence")
	private List<String> filterPropertiesExistence = new LinkedList<String>();

	@Param("filter-by-vlan")
	private List<String> filterVlan = new LinkedList<String>();

	@Param("filter-by-schedule")
	private String filterSchedule;

	@Param(value = "filter-by-props-regexp", keyValueList = true)
	private List<String> filterPropertiesRegexp = new LinkedList<String>();

	@Param("auditView")
	private boolean audit = false;

	public void setInstances(String instances)
	{
		populateParamValues(instances, this.instances);
	}

	public void setStates(String states)
	{
		populateParamValues(states, filterStates);
	}

	public void setProperties(String properties)
	{
		populateParamValues(properties, filterProperties);
	}

	public void setPropertiesExistence(String properties)
	{
		populateParamValues(properties, filterPropertiesExistence);
	}

	public void setVlans(String vlans)
	{
		populateParamValues(vlans, filterVlan);
	}

	public void setFilterSchedule(String schedule)
	{
		this.filterSchedule = schedule;
	}

	public void setPropertiesRegexp(String properties)
	{
		populateParamValues(properties, filterPropertiesRegexp);
	}

	public void setAudit(boolean audit)
	{
		this.audit = audit;
	}

	@Override
	public void doWork() throws BuildException
	{
		try
		{
			super.doWork();
		}
		catch (SystemExitException e)
		{
			// project/region doesn't contain any VM instances
			if (e.getCode() == 61)
			{
				return;
			}
		}
	}
}
