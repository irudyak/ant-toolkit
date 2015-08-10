package com.anttoolkit.general.tasks.math;

import java.util.*;

import com.anttoolkit.general.tasks.GenericTask;
import org.apache.tools.ant.*;


public class GenerateRandomIntegerTask
	extends GenericTask
{
	private static final Random RANDOM = new Random(System.currentTimeMillis());

	private int min = 0;
	private int max = Integer.MAX_VALUE;
	private String property = null;

	public static int getRandomInt(int min, int max)
	{
		if (min < 0)
		{
			throw new BuildException("Min boundary can't be less than 0");
		}

		if (max <= min)
		{
			throw new BuildException("Max boundary can't be less or equal to min range");
		}

		int range = max - min + 1;

		return RANDOM.nextInt(range) + min;
	}

	public void setMin(int min)
	{
		this.min = min;
	}

	public void setMax(int max)
	{
		this.max = max;
	}

	public void setProperty(String property)
	{
		this.property = property;
	}

	public void doWork() throws BuildException
	{
		int randomValue = getRandomInt(min, max);
		setPropertyThreadSafe(property, Integer.toString(randomValue));
	}

	protected void validate()
	{
		if (property == null)
		{
			throw new BuildException("Property should be specified");
		}
	}
}
