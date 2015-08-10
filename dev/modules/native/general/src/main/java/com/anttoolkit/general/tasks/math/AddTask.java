package com.anttoolkit.general.tasks.math;

import com.anttoolkit.general.tasks.math.operation.*;

public class AddTask extends ArithmeticOperationTask
{
	@Override
	protected IMathOperation getOperation()
	{
		return AddOperation.instance;
	}
}
