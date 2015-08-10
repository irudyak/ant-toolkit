package com.anttoolkit.general.conditions;

public class MoreOrEqual extends ComparisonCondition
{
	@Override
	public Operation operation()
	{
		return Operation.MORE_OR_EQUAL;
	}
}