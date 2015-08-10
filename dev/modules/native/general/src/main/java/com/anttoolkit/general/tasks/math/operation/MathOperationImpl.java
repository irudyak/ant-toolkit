package com.anttoolkit.general.tasks.math.operation;

import org.apache.tools.ant.*;

public abstract class MathOperationImpl
		implements IMathOperation
{
	public String execute(String arg1, String arg2)
	{
		if (!arg1.contains(".") && !arg1.contains(",") &&
			!arg2.contains(".") && !arg2.contains(","))
		{
			try
			{
				int _arg1 = Integer.parseInt(arg1);
				int _arg2 = Integer.parseInt(arg2);

				return Integer.toString(execute(_arg1, _arg2));
			}
			catch (NumberFormatException e) {}
		}

		double _arg1 = convertToDouble(arg1);
		double _arg2 = convertToDouble(arg2);

		return Double.toString(execute(_arg1, _arg2));
	}

	protected abstract int execute(int arg1, int arg2);

	protected abstract double execute(double arg1, double arg2);

	private double convertToDouble(String arg)
	{
		String _arg = arg.replace(".", ",");

		try
		{
			return Double.parseDouble(_arg);
		}
		catch (NumberFormatException e) {}

		_arg = arg.replace(",", ".");

		try
		{
			return Double.parseDouble(_arg);
		}
		catch (NumberFormatException e)
		{
			throw new BuildException("Incorrect format for arithmetic operation argument: " + arg);
		}
	}
}
