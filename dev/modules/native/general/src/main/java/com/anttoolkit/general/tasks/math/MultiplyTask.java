package com.anttoolkit.general.tasks.math;

import com.anttoolkit.general.tasks.math.operation.*;

public class MultiplyTask extends ArithmeticOperationTask
{
	@Override
	protected IMathOperation getOperation()
	{
		return MultiplyOperation.instance;
	}
}
