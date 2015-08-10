package com.anttoolkit.aws.ec2.common;

import com.amazonaws.services.ec2.model.*;

public class IamInstanceProfileParser
{
	public static IamInstanceProfileSpecification parseProfile(String profile)
	{
		if (profile == null || profile.trim().isEmpty())
		{
			throw new IllegalArgumentException("Empty IAM instance profile specified");
		}

		IamInstanceProfileSpecification pr = new IamInstanceProfileSpecification();

		String[] chunks = profile.split(",");

		try
		{
			for (String chunk : chunks)
			{
				update(pr, chunk);
			}
		}
		catch (IllegalArgumentException e)
		{
			throw new IllegalArgumentException("Incorrect IAM instance profile specified '" + profile + "'");
		}

		return pr;
	}

	private static void update(IamInstanceProfileSpecification profile, String val)
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

		if ("arn".equals(key))
		{
			profile.setArn(value);
		}
		else if ("name".equals(key))
		{
			profile.setName(value);
		}
		else
		{
			throw new IllegalArgumentException();
		}
	}
}
