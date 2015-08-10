package com.anttoolkit.general.tasks.math.operation;

public class MultiplyOperation
	extends MathOperationImpl
{
	public static final MultiplyOperation instance = new MultiplyOperation();

	private MultiplyOperation() {}

	protected int execute(int arg1, int arg2)
	{
		return arg1 * arg2;
	}

	protected double execute(double arg1, double arg2)
	{
		return arg1 * arg2;
	}
}
