package com.anttoolkit.general.common;

import java.util.*;

public class SynchronizationManager
{
	private static Map<String, Object> synchObjectsMap = new HashMap<String, Object>();

	public static Object getSynchronizationObject(String namespace, String alias)
	{
		Map<String, Object> synchMap = getSynchObjectsMap();

		//noinspection SynchronizationOnLocalVariableOrMethodParameter
		synchronized (synchMap)
		{
			if (synchMap.containsKey(namespace + alias))
			{
				Object obj = synchMap.get(namespace + alias);
				return obj != null ? obj : System.class;
			}

			synchMap.put(namespace + alias, namespace + alias);

			Object obj = synchMap.get(namespace + alias);
			return obj != null ? obj : System.class;
		}
	}

	private static Map<String, Object> getSynchObjectsMap()
	{
		return synchObjectsMap;
	}
}
