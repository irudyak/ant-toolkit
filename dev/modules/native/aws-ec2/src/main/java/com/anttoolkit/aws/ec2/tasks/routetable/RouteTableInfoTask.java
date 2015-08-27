package com.anttoolkit.aws.ec2.tasks.routetable;

import com.amazonaws.services.ec2.model.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.aws.ec2.tasks.*;

public abstract class RouteTableInfoTask extends GenericEc2Task
{
	private String tableIdProperty;
	private String vpcIdProperty;
	private String routesProperty;
	private String associationsProperty;
	private String tagsProperty;
	private String propagationsProperty;

	public void setTableIdProperty(String property)
	{
		tableIdProperty = property;
	}

	public void setVpcIdProperty(String property)
	{
		vpcIdProperty = property;
	}

	public void setRoutesProperty(String property)
	{
		routesProperty = property;
	}

	public void setAssociationsProperty(String property)
	{
		associationsProperty = property;
	}

	public void setTagsProperty(String property)
	{
		tagsProperty = property;
	}

	public void setPropagationsPropertyProperty(String property)
	{
		propagationsProperty = property;
	}

	protected void setPropertiesFromRouteTable(RouteTable table)
	{
		if (tableIdProperty != null)
		{
			this.setPropertyThreadSafe(tableIdProperty, table.getRouteTableId());
		}

		if (vpcIdProperty != null)
		{
			this.setPropertyThreadSafe(vpcIdProperty, table.getVpcId());
		}

		if (associationsProperty != null)
		{
			this.setPropertyThreadSafe(associationsProperty, CollectionsHelper.toString(table.getAssociations()));
		}

		if (routesProperty != null)
		{
			this.setPropertyThreadSafe(routesProperty, CollectionsHelper.toString(table.getRoutes()));
		}

		if (tagsProperty != null)
		{
			this.setPropertyThreadSafe(tagsProperty, CollectionsHelper.toString(table.getTags()));
		}

		if (propagationsProperty != null)
		{
			this.setPropertyThreadSafe(propagationsProperty, CollectionsHelper.toString(table.getPropagatingVgws()));
		}
	}
}
