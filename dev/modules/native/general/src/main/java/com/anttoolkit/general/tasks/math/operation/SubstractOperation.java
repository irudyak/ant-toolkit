package com.anttoolkit.general.tasks.math.operation;

public class SubstractOperation
	extends MathOperationImpl
{
	public static final SubstractOperation instance = new SubstractOperation();

	private SubstractOperation() {}

	protected int execute(int arg1, int arg2)
	{
		return arg1 - arg2;
	}

	protected double execute(double arg1, double arg2)
	{
		return arg1 - arg2;
	}
}
