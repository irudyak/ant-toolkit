package com.anttoolkit.general.conditions;

public class Less extends ComparisonCondition
{
	@Override
	public Operation operation()
	{
		return Operation.LESS;
	}
}