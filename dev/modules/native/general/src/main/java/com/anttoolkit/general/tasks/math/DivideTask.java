package com.anttoolkit.general.tasks.math;

import com.anttoolkit.general.tasks.math.operation.*;

public class DivideTask extends ArithmeticOperationTask
{
	@Override
	protected IMathOperation getOperation()
	{
		return DivideOperation.instance;
	}
}
