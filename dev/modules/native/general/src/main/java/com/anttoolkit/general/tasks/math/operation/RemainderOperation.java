package com.anttoolkit.general.tasks.math.operation;

public class RemainderOperation extends MathOperationImpl
{
	public static final RemainderOperation instance = new RemainderOperation();

	private RemainderOperation() {}

	protected int execute(int arg1, int arg2)
	{
		return arg1 % arg2;
	}

	protected double execute(double arg1, double arg2)
	{
		return arg1 % arg2;
	}
}
