package com.anttoolkit.documentum.common;

public class ArrayHelper
{
	public static boolean hasElement(Object[] array, Object element)
	{
		if (array == null)
		{
			return false;
		}

		for (Object obj : array)
		{
			if (element == null)
			{
				if (obj == null)
				{
					return true;
				}
			}
			else if (element.equals(obj))
			{
				return true;
			}
		}

		return false;
	}
}
