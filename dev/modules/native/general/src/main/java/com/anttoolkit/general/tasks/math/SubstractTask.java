package com.anttoolkit.general.tasks.math;

import com.anttoolkit.general.tasks.math.operation.*;

public class SubstractTask extends ArithmeticOperationTask
{
	@Override
	protected IMathOperation getOperation()
	{
		return SubstractOperation.instance;
	}
}
