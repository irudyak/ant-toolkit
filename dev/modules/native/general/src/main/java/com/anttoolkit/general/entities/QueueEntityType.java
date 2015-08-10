package com.anttoolkit.general.entities;

public class QueueEntityType extends EntityType
{
	public static final QueueEntityType instance = new QueueEntityType();

	private QueueEntityType() {}

	@Override
	public String getName()
	{
		return "QUEUE";
	}

	public boolean allowedForLocalScope()
	{
		return false;
	}

	public boolean allowedForRootScope()
	{
		return false;
	}

	public boolean allowedForGlobalScope()
	{
		return true;
	}
}
