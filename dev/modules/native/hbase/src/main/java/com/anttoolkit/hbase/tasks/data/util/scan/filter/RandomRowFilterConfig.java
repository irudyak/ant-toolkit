package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import org.apache.hadoop.hbase.filter.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public class RandomRowFilterConfig implements IFilterConfig
{
	private Float chance;

	public void setChance(float chance)
	{
		this.chance = chance;
	}

	@Override
	public Filter getFilter(GenericTask task)
	{
		validate(task);
		return new RandomRowFilter(chance);
	}

	@Override
	public void validate(GenericTask task)
	{
		if (chance == null)
		{
			throw new BuildException("Chance should be specified");
		}
	}
}
