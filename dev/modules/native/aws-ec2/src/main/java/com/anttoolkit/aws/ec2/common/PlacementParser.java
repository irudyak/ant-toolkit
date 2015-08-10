package com.anttoolkit.aws.ec2.common;

import com.amazonaws.services.ec2.model.*;

public class PlacementParser
{
	public static Placement parsePlacement(String placement)
	{
		if (placement == null || placement.trim().isEmpty())
		{
			throw new IllegalArgumentException("Empty placement specified");
		}

		Placement pl = new Placement();

		String[] chunks = placement.split(",");

		try
		{
			for (String chunk : chunks)
			{
				update(pl, chunk);
			}
		}
		catch (IllegalArgumentException e)
		{
			throw new IllegalArgumentException("Incorrect placement specified '" + placement + "'");
		}

		return pl;
	}

	private static void update(Placement pl, String val)
	{
		String[] keyValue = val.split("=");
		if (keyValue.length != 2)
		{
			throw new IllegalArgumentException();
		}

		String key = keyValue[0].trim().toLowerCase();
		String value = keyValue[1].trim();

		if (key.isEmpty() || value.isEmpty())
		{
			throw new IllegalArgumentException();
		}

		if ("availabilityzone".equals(key))
		{
			pl.setAvailabilityZone(value);
		}
		else if ("groupname".equals(key))
		{
			pl.setGroupName(value);
		}
		else if ("tenancy".equals(key))
		{
			pl.setTenancy(value);
		}
		else
		{
			throw new IllegalArgumentException();
		}
	}
}
