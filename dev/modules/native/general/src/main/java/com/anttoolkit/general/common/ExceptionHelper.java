package com.anttoolkit.general.common;

public class ExceptionHelper
{
	public static String stackTraceToString(Throwable exception)
	{
		StringBuilder buffer = new StringBuilder();

		if (exception == null)
		{
			return buffer.toString();
		}

		buffer.append("\r\n");

		Throwable throwable = exception;
		do
		{
			buffer.append("\t").append(throwable.toString()).append("\r\n");
			StackTraceElement[] elements = throwable.getStackTrace();

			for (StackTraceElement element : elements)
			{
				buffer.append("\t\t").append(element.toString()).append("\r\n");
			}

			throwable = throwable.getCause();
		}
		while (throwable != null);

		return buffer.toString();
	}

	public static boolean hasCauseInChain(Class cause, Throwable exception)
	{
		if (exception == null)
		{
			throw new IllegalArgumentException("Exception argument can't be null");
		}

		if (cause == null)
		{
			throw new IllegalArgumentException("Cause argument can't be null");
		}

		Throwable e = exception;

		while (e != null)
		{
			if (e.getClass().equals(cause))
			{
				return true;
			}

			e = e.getCause();
		}

		return false;
	}
}
