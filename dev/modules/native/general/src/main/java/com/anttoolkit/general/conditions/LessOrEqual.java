package com.anttoolkit.general.conditions;

public class LessOrEqual extends ComparisonCondition
{
	@Override
	public Operation operation()
	{
		return Operation.LESS_OR_EQUAL;
	}
}