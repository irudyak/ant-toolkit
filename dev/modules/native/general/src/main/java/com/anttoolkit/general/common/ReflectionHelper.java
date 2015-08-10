package com.anttoolkit.general.common;

import org.apache.tools.ant.BuildException;

import java.util.*;
import java.lang.reflect.*;

public class ReflectionHelper
{
	private static final Map<String, Class> PRIMITIVE_TYPES_CLASSES;
	private static final Map<String, String> PRIMITIVE_TYPES_MAPPING;

	static
	{
		Map<String, Class> classesMap = new HashMap<String, Class>();
		classesMap.put(byte.class.getName(), byte.class);
		classesMap.put(short.class.getName(), short.class);
		classesMap.put(int.class.getName(), int.class);
		classesMap.put(long.class.getName(), long.class);
		classesMap.put(float.class.getName(), float.class);
		classesMap.put(double.class.getName(), double.class);
		classesMap.put(boolean.class.getName(), boolean.class);
		classesMap.put(char.class.getName(), char.class);
		PRIMITIVE_TYPES_CLASSES = classesMap;

		Map<String, String> map = new HashMap<String, String>();
		map.put(byte.class.getName(), Byte.class.getName());
		map.put(short.class.getName(), Short.class.getName());
		map.put(int.class.getName(), Integer.class.getName());
		map.put(long.class.getName(), Long.class.getName());
		map.put(float.class.getName(), Float.class.getName());
		map.put(double.class.getName(), Double.class.getName());
		map.put(boolean.class.getName(), Boolean.class.getName());
		map.put(char.class.getName(), Character.class.getName());
		PRIMITIVE_TYPES_MAPPING = map;
	}

	public static boolean isPrimitiveType(String className)
	{
		return PRIMITIVE_TYPES_CLASSES.containsKey(className);
	}

	public static Class forName(String className)
			throws ClassNotFoundException
	{
		if (className == null)
		{
			return null;
		}

		//primitive type
		if (isPrimitiveType(className))
		{
			return PRIMITIVE_TYPES_CLASSES.get(className);
		}

		// not array
		if (!className.endsWith("[]"))
		{
			return getClassInstance(className);
		}

		String primaryClass = className.replace("[]", "");

		return Array.newInstance(getClassInstance(primaryClass), 0).getClass();
	}

	public static Object newInstance(String className)
			throws ClassNotFoundException, InstantiationException, IllegalAccessException
	{
		if (isPrimitiveType(className))
		{
			throw new UnsupportedOperationException("Can't instantiate primitive type instance '" + className + "' without any provided value");
		}

		Class clazz = forName(className);
		if (clazz == null)
		{
			return null;
		}

		if (clazz.isEnum())
		{
			throw new UnsupportedOperationException("Can't instantiate enum '" + className + "' without any provided value");
		}

		if (clazz.isArray())
		{
			throw new UnsupportedOperationException("To instantiate array of type '" + className + "' please use newArrayInstance method");
		}

		try
		{
			return clazz.newInstance();
		}
		catch (InstantiationException e)
		{
			throw new InstantiationException("Failed to instantiate class \"" + className +
					"\" using default consructor.\r\n" + ExceptionHelper.stackTraceToString(e));
		}
		catch (IllegalAccessException e)
		{
			throw new IllegalAccessException("Failed to instantiate class \"" + className +
					"\" using default consructor.\r\n" + ExceptionHelper.stackTraceToString(e));
		}
	}

	public static Object newInstance(String className, Object[] params, Class[] paramTypes)
			throws ClassNotFoundException, NoSuchMethodException, SecurityException,
			InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException
	{
		String clazzName = PRIMITIVE_TYPES_MAPPING.containsKey(className) ?
				PRIMITIVE_TYPES_MAPPING.get(className) : className;

		if (params == null || params.length == 0 ||
			paramTypes == null || paramTypes.length == 0)
		{
			return newInstance(clazzName);
		}

		Class clazz = forName(clazzName);
		if (clazz == null)
		{
			return null;
		}

		if (clazz.isEnum())
		{
			if (params.length == 1 && paramTypes.length == 1 && paramTypes[0].equals(String.class))
			{
				return Enum.valueOf(clazz, params[0].toString());
			}

			throw new UnsupportedOperationException("Instance of '" + className + "' can only be created from String " +
					"representation of appropriate enum constant");
		}

		if (clazz.isArray())
		{
			throw new UnsupportedOperationException("To instantiate array of type '" + className + "' please use newArrayInstance method");
		}

		Constructor constructor;
		try
		{
			constructor = clazz.getConstructor(paramTypes);
		}
		catch (NoSuchMethodException e)
		{
			throw new NoSuchMethodException("Class \"" + clazzName + "\" has no public constructor \"" +
					getMethodSignatureDescription(clazzName, paramTypes) +
					"\"\r\n" + ExceptionHelper.stackTraceToString(e));
		}
		catch (SecurityException e)
		{
			throw new SecurityException("Failed to get constructor \"" +
					getMethodSignatureDescription(clazzName, paramTypes) + "\" for class \"" +
					clazzName + "\"\r\n" + ExceptionHelper.stackTraceToString(e));
		}

		try
		{
			return constructor.newInstance(params);
		}
		catch (InstantiationException e)
		{
			throw new InstantiationException("Failed to instantiate class \"" + clazzName +
					"\" using consructor \"" + getMethodSignatureDescription(clazzName, paramTypes) +
					".\r\n" + ExceptionHelper.stackTraceToString(e));
		}
		catch (IllegalAccessException e)
		{
			throw new InstantiationException("Failed to instantiate class \"" + clazzName +
					"\" using consructor \"" + getMethodSignatureDescription(clazzName, paramTypes) +
					".\r\n" + ExceptionHelper.stackTraceToString(e));
		}
		catch (IllegalArgumentException e)
		{
			throw new IllegalArgumentException("Illegal argument was provided for constructor \"" +
					getMethodSignatureDescription(clazzName, paramTypes) + "\" of the class \"" +
					clazzName + "\".\r\n" + ExceptionHelper.stackTraceToString(e));
		}
		catch (InvocationTargetException e)
		{
			throw new InvocationTargetException(e.getTargetException(), "Exception occured in constructor \"" +
					getMethodSignatureDescription(clazzName, paramTypes) + "\" of the class \"" +
					clazz.getName() + "\" while trying to instantiate it");
		}
	}

	public static Object newArrayInstance(String className, Object[] values)
			throws ClassNotFoundException, NoSuchMethodException, SecurityException,
			InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException
	{
		Class clazz = forName(className);
		if (clazz == null)
		{
			return null;
		}

		Object array = Array.newInstance(clazz, values == null || values.length == 0 ? 0 : values.length);

		if (values == null || values.length == 0)
		{
			return array;
		}

		for (int i = 0; i < values.length; i++)
		{
			Object value = values[i];
			Object valueToSet = null;

			if (value == null || clazz.equals(value.getClass()))
			{
				valueToSet = value;
			}
			else if (isPrimitiveType(className))
			{
				valueToSet = primitiveTypeInstance(className, value);
			}
			else if (hasConstructor(className, new Class[] {value.getClass()}))
			{
				valueToSet = newInstance(className, new Object[] {value}, new Class[] {value.getClass()});
			}
			else if (hasConstructor(className, new Class[] {String.class}))
			{
				valueToSet = newInstance(className, new Object[] {value.toString()}, new Class[] {String.class});
			}
			else
			{
				throw new IllegalArgumentException("Can't instantiate array of " + className + " having element of " +
						value.getClass() + " class");
			}

			Array.set(array, i, valueToSet);
		}

		return array;
	}

	public static Object primitiveTypeInstance(String type, Object value)
	{
		if (type == null || !PRIMITIVE_TYPES_CLASSES.containsKey(type))
		{
			throw new IllegalArgumentException("Incorrect primitive type specified");
		}

		if (value == null)
		{
			throw new IllegalArgumentException("Can't convert null value to primitive type");
		}

		try
		{
			if (byte.class.getName().equals(type))
			{
				return Byte.parseByte(value.toString());
			}
			else if (short.class.getName().equals(type))
			{
				return Short.parseShort(value.toString());
			}
			else if (int.class.getName().equals(type))
			{
				return Integer.parseInt(value.toString());
			}
			else if (long.class.getName().equals(type))
			{
				return Long.parseLong(value.toString());
			}
			else if (float.class.getName().equals(type))
			{
				return Float.parseFloat(value.toString());
			}
			else if (double.class.getName().equals(type))
			{
				return Double.parseDouble(value.toString());
			}
			else if (boolean.class.getName().equals(type))
			{
				return Boolean.parseBoolean(value.toString());
			}
			else
			{
				return value.toString().charAt(0);
			}
		}
		catch (NumberFormatException e)
		{
			throw new BuildException("Failed to value '" + value.toString() + "' to " + type + " primitive type", e);
		}
	}

	public static boolean hasConstructor(String className, Class[] paramTypes)
			throws ClassNotFoundException
	{
		String clazzName = PRIMITIVE_TYPES_MAPPING.containsKey(className) ?
				PRIMITIVE_TYPES_MAPPING.get(className) : className;

		if (clazzName == null || clazzName.trim().length() == 0)
		{
			return false;
		}

		Class clazz = forName(clazzName);
		if (clazz == null)
		{
			return false;
		}

		try
		{
			Constructor constructor = clazz.getConstructor(paramTypes);
			return constructor != null;
		}
		catch (NoSuchMethodException e)
		{
			return false;
		}
		catch (SecurityException e)
		{
			return false;
		}
	}

	public static Class[] getInterfaces(Class clazz)
	{
		if (clazz == null)
		{
			return null;
		}

		ArrayList<Class> list = null;

		Class[] classInterfaces = clazz.getInterfaces();
		if (classInterfaces != null && classInterfaces.length > 0)
		{
			list = new ArrayList<Class>(Arrays.asList(classInterfaces));

			for (Class _interface : classInterfaces)
			{
				Class[] interfaceInterfaces = _interface.getInterfaces();
				if (interfaceInterfaces == null || interfaceInterfaces.length == 0)
				{
					continue;
				}

				Class[] superInterfaces = getInterfaces(_interface);
				for (Class __interface : superInterfaces)
				{
					if (!list.contains(__interface))
					{
						list.add(__interface);
					}
				}
			}
		}

		Class superClass = clazz.getSuperclass();
		if (superClass == null)
		{
			return list == null || list.size() == 0 ? null : list.toArray(new Class[]{});
		}

		Class[] superInterfaces = getInterfaces(superClass);
		if (superInterfaces == null || superInterfaces.length == 0)
		{
			return list == null || list.size() == 0 ? null : list.toArray(new Class[]{});
		}

		list = list != null ? list : new ArrayList<Class>();
		for (Class _interface : superInterfaces)
		{
			if (!list.contains(_interface))
			{
				list.add(_interface);
			}
		}

		return list == null || list.size() == 0 ? null : list.toArray(new Class[]{});
	}

	public static Field[] getDeclaredFields(Class clazz, Class classInheritanceRestriction)
	{
		return getDeclaredFields(clazz, classInheritanceRestriction, true);
	}

	public static Field[] getDeclaredFields(Class clazz, Class classInheritanceRestriction, boolean includeRestrictionClass)
	{
		if (clazz == null || (clazz.equals(classInheritanceRestriction) && !includeRestrictionClass))
		{
			return null;
		}

		Field[] fields = clazz.getDeclaredFields();
		if (clazz.equals(classInheritanceRestriction))
		{
			return fields.length == 0 ? null : fields;
		}

		Class superClass = clazz.getSuperclass();
		if (superClass == null || (superClass.equals(classInheritanceRestriction) && !includeRestrictionClass))
		{
			return fields.length == 0 ? null : fields;
		}

		Field[] inheritedFields = getDeclaredFields(clazz.getSuperclass(), classInheritanceRestriction, includeRestrictionClass);
		if (inheritedFields == null || inheritedFields.length == 0)
		{
			return fields.length == 0 ? null : fields;
		}

		ArrayList<Field> summary = new ArrayList<Field>(Arrays.asList(fields));

		for (Field inheritedField : inheritedFields)
		{
			boolean exist = false;
			for (Field field : fields)
			{
				if (inheritedField.getName().equals(field.getName()))
				{
					exist = true;
					break;
				}
			}

			if (!exist)
			{
				summary.add(inheritedField);
			}
		}

		return summary.toArray(new Field[]{});
	}

	public static boolean extendsClass(Class child, Class parent)
	{
		if (child == null || parent == null)
		{
			return false;
		}

		Class superClass = child.getSuperclass();
		while (superClass != null && !superClass.getName().equals(Object.class.getName()))
		{
			if (parent.equals(superClass))
			{
				return true;
			}

			superClass = superClass.getSuperclass();
		}

		return false;
	}

	public static boolean implementsInterface(Class clazz, Class _interface)
	{
		if (clazz == null || _interface == null)
		{
			return false;
		}

		Class[] interfaces = getInterfaces(clazz);
		if (interfaces == null || interfaces.length == 0)
		{
			return false;
		}

		for (Class implementedInterface : interfaces)
		{
			if (_interface.equals(implementedInterface))
			{
				return true;
			}
		}

		return false;
	}

	public static boolean implementsInterface(Class clazz, String interfaceName)
	{
		if (clazz == null || interfaceName == null || interfaceName.trim().length() == 0)
		{
			return false;
		}

		Class[] interfaces = getInterfaces(clazz);
		if (interfaces == null || interfaces.length == 0)
		{
			return false;
		}

		for (Class _interface : interfaces)
		{
			if (interfaceName.equals(_interface.getName()))
			{
				return true;
			}
		}

		return false;
	}

	public static boolean implementsInterface(Object object, Class _interface)
	{
		return object != null && implementsInterface(object.getClass(), _interface);
	}

	public static boolean implementsInterface(Object object, String interfaceName)
	{
		return object != null && implementsInterface(object.getClass(), interfaceName);
	}

	public static Object invokeMethod(Object obj, String methodName)
			throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, IllegalArgumentException
	{
		return invokeMethod(obj, methodName, null, null);
	}

	public static Object invokeMethod(Object obj, String methodName, Object[] params, Class[] paramTypes)
			throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, IllegalArgumentException
	{
		if (obj == null || methodName == null)
		{
			throw new IllegalArgumentException("obj and methodName arguments couldn't be null");
		}

		Class clazz = obj instanceof Class ? (Class)obj : obj.getClass();

		Method method = findMethod(clazz, methodName, paramTypes);

		return invokeMethod(obj, method, params, clazz);
	}

	public static Object invokeMethod(Object obj, String methodName, Object[] params)
			throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, IllegalArgumentException
	{
		if (obj == null || methodName == null)
		{
			throw new IllegalArgumentException("\"obj\" and \"methodName\" arguments couldn't be null");
		}

		if (params == null || params.length == 0)
		{
			return invokeMethod(obj, methodName, null, null);
		}

		Class paramTypes[] = new Class[params.length];
		for (int i = 0; i < params.length; i++)
		{
			paramTypes[i] = params[i].getClass();
		}

		Class clazz = obj instanceof Class ? (Class)obj : obj.getClass();

		Method method = getSuitableMethod(clazz.getMethods(), methodName, paramTypes);
		if (method != null)
		{
			return invokeMethod(obj, method, params, clazz);
		}

		method = getSuitableMethod(clazz.getDeclaredMethods(), methodName, paramTypes);
		if (method != null)
		{
			return invokeMethod(obj, method, params, clazz);
		}

		Class superClass = clazz.getSuperclass();

		while (superClass != null && !superClass.getName().equals(Object.class.getName()))
		{
			method = getSuitableMethod(superClass.getMethods(), methodName, paramTypes);
			if (method != null)
			{
				return invokeMethod(obj, method, params, clazz);
			}

			method = getSuitableMethod(superClass.getDeclaredMethods(), methodName, paramTypes);
			if (method != null)
			{
				return invokeMethod(obj, method, params, clazz);
			}

			superClass = superClass.getSuperclass();
		}

		throw new NoSuchMethodException("Failed to find suitable method \"" +
				getMethodSignatureDescription(methodName, params) + "\" for class \"" +
				clazz.getName() + "\" using reflection");
	}

	public static boolean checkMethodReturnsVoid(Object obj, String methodName, Class[] paramTypes)
			throws NoSuchMethodException
	{
		if (obj == null || methodName == null)
		{
			throw new IllegalArgumentException("obj and methodName arguments couldn't be null");
		}

		Class clazz = obj instanceof Class ? (Class)obj : obj.getClass();

		Method method = findMethod(clazz, methodName, paramTypes);

		return method.getReturnType().equals(void.class);
	}

	public static boolean checkMethodExists(Object obj, String methodName, Class[] paramTypes)
	{
		if (obj == null || methodName == null)
		{
			throw new IllegalArgumentException("obj and methodName arguments couldn't be null");
		}

		Class clazz = obj instanceof Class ? (Class)obj : obj.getClass();

		try
		{
			findMethod(clazz, methodName, paramTypes);
			return true;
		}
		catch (NoSuchMethodException e)
		{
			return false;
		}
	}

	private static Object invokeMethod(Object objectInstance, Method method, Object[] params, Class methodClass)
			throws InvocationTargetException, IllegalAccessException
	{
		if (!method.isAccessible())
		{
			method.setAccessible(true);
		}

		try
		{
			return method.invoke(objectInstance, params);
		}
		catch (InvocationTargetException e)
		{
			throw new InvocationTargetException(e.getTargetException(), "Exception occured in the method \"" +
					getMethodSignatureDescription(method.getName(), method.getParameterTypes()) + "\" of class \"" +
					methodClass.getName() + "\" while trying to invoke it using reflection");
		}
		catch (IllegalArgumentException e)
		{
			throw new IllegalArgumentException("Illegal argument was provided for the method \"" +
					getMethodSignatureDescription(method.getName(), method.getParameterTypes()) + "\" of class \"" +
					methodClass.getName() + "\" while trying to invoke it using reflection. \r\n" +
					ExceptionHelper.stackTraceToString(e));
		}
	}

	private static Method findMethod(Class clazz, String methodName, Class[] paramTypes)
			throws NoSuchMethodException
	{
		try
		{
			return clazz.getMethod(methodName, paramTypes);
		}
		catch (NoSuchMethodException e) {}

		try
		{
			return clazz.getDeclaredMethod(methodName, paramTypes);
		}
		catch (NoSuchMethodException e) {}

		Class superClass = clazz.getSuperclass();
		while (superClass != null && !superClass.getName().equals(Object.class.getName()))
		{
			try
			{
				return superClass.getMethod(methodName, paramTypes);
			}
			catch (NoSuchMethodException e) {}

			try
			{
				return superClass.getDeclaredMethod(methodName, paramTypes);
			}
			catch (NoSuchMethodException e) {}

			superClass = superClass.getSuperclass();
		}

		throw new NoSuchMethodException("Failed to find method \"" +
				getMethodSignatureDescription(methodName, paramTypes) + "\" for class \"" +
				clazz.getName() + "\" using reflection");
	}

	private static Method getSuitableMethod(Method[] methods, String methodName, Class[] paramTypes)
	{
		if (methods == null || methods.length == 0 ||
			methodName == null || methodName.trim().length() == 0)
		{
			return null;
		}

		for (Method method : methods)
		{
			if (!method.getName().equals(methodName))
			{
				continue;
			}

			Class _paramTypes[] = method.getParameterTypes();
			if (_paramTypes.length != paramTypes.length)
			{
				continue;
			}

			boolean isSuitableMethod = true;

			int paramsCount = paramTypes.length;
			for (int i = 0; i < paramsCount && isSuitableMethod; i++)
			{
				if (paramTypes[i].equals(_paramTypes[i]))
				{
					continue;
				}

				if (extendsClass(paramTypes[i], _paramTypes[i]))
				{
					continue;
				}

				if (implementsInterface(paramTypes[i], _paramTypes[i]))
				{
					continue;
				}

				isSuitableMethod = false;
			}

			if (isSuitableMethod)
			{
				return method;
			}
		}

		return null;
	}

	private static String getMethodSignatureDescription(String methodName, Object[] params)
	{
		if (params == null || params.length == 0)
		{
			return getMethodSignatureDescription(methodName, null);
		}

		int count = params.length;
		Class[] types = new Class[count];

		for (int i = 0; i < count; i++)
		{
			types[i] = params[i].getClass();
		}

		return getMethodSignatureDescription(methodName, types);
	}

	private static String getMethodSignatureDescription(String methodName, Class[] paramTypes)
	{
		if (paramTypes == null || paramTypes.length == 0)
		{
			return methodName + "()";
		}

		StringBuffer buffer = new StringBuffer();
		int count = paramTypes.length;
		for (int i = 0; i < count; i++)
		{
			if (buffer.length() == 0)
			{
				buffer.append("(").append(paramTypes[i].getName());
				continue;
			}

			buffer.append(", ").append(paramTypes[i].getName());
		}

		return buffer.insert(0, methodName).append(")").toString();
	}

	private static Class getClassInstance(String className)
			throws ClassNotFoundException
	{
		String clazzName = PRIMITIVE_TYPES_MAPPING.containsKey(className) ?
				PRIMITIVE_TYPES_MAPPING.get(className) : className;

		try
		{
			return Class.forName(clazzName);
		}
		catch (ClassNotFoundException e)
		{
		}

		try
		{
			return Class.forName(clazzName, true, Thread.currentThread().getContextClassLoader());
		}
		catch (ClassNotFoundException e)
		{
		}

		try
		{
			return Class.forName(clazzName, true, ReflectionHelper.class.getClassLoader());
		}
		catch (ClassNotFoundException e)
		{
		}

		try
		{
			return Class.forName(clazzName, true, ClassLoader.getSystemClassLoader());
		}
		catch (ClassNotFoundException e)
		{
		}

		throw new ClassNotFoundException("Failed to load class '" + clazzName + "' using reflection");
	}
}
