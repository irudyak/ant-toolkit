package com.anttoolkit.general.tasks.math;

import com.anttoolkit.general.tasks.math.operation.*;

public class RemainderTask extends ArithmeticOperationTask
{
	@Override
	protected IMathOperation getOperation()
	{
		return RemainderOperation.instance;
	}
}
