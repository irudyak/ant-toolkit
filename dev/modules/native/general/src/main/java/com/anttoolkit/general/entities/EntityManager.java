package com.anttoolkit.general.entities;

import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.locks.*;

public class EntityManager
{
	private static final boolean MONITOR_LOCKS;

	public static final String GLOBAL_SCOPE_PREFIX = "g:";
	public static final String PARENT_SCOPE_PREFIX = "parent:";
	public static final String CURRENT_SCOPE_PREFIX = "this:";

	private static final List<EntityType> LOCAL_SCOPE_TYPES;
	private static final List<EntityType> ROOT_SCOPE_TYPES;
	private static final List<EntityType> GLOBAL_SCOPE_TYPES;

	static
	{
		List<EntityType> localTypes = new LinkedList<EntityType>();
		List<EntityType> rootTypes = new LinkedList<EntityType>();
		List<EntityType> globalTypes = new LinkedList<EntityType>();

		ServiceLoader<EntityTypesProvider> loader = ServiceLoader.load(EntityTypesProvider.class);
		for (EntityTypesProvider provider : loader)
		{
			EntityType[] types = provider.supportedTypes();
			if (types != null)
			{
				for (EntityType type : types)
				{
					if (type.allowedForLocalScope() && !localTypes.contains(type))
					{
						localTypes.add(type);
					}

					if (type.allowedForRootScope() && !rootTypes.contains(type))
					{
						rootTypes.add(type);
					}

					if (type.allowedForGlobalScope() && !globalTypes.contains(type))
					{
						globalTypes.add(type);
					}
				}
			}
		}

		LOCAL_SCOPE_TYPES = localTypes;
		ROOT_SCOPE_TYPES = rootTypes;
		GLOBAL_SCOPE_TYPES = globalTypes;

		boolean monitorLocks = false;

		try
		{
			ResourceBundle bundle = ResourceBundle.getBundle("entity-manager");
			monitorLocks = "true".equals(bundle.getString("monitor-locks"));
		}
		catch (MissingResourceException e)
		{
		}

		MONITOR_LOCKS = monitorLocks;
	}

	private static final Map<EntityType, Map<String, Object[]>> GLOBAL_SCOPE = createScope(GLOBAL_SCOPE_TYPES);
	private static final Map<EntityType, Map<String, Object[]>> ROOT_SCOPE = createScope(ROOT_SCOPE_TYPES);
	private static ThreadLocal<List<Map<EntityType, Map<String, Object[]>>>> scopeStack = new ThreadLocal<List<Map<EntityType, Map<String, Object[]>>>>()
		{
			protected List<Map<EntityType, Map<String, Object[]>>> initialValue()
			{
				List<Map<EntityType, Map<String, Object[]>>> stack = new LinkedList<Map<EntityType, Map<String, Object[]>>>();
				stack.add(ROOT_SCOPE);
				return stack;
			}
		};

	private static final ThreadLocal<Set<EntityType>> skippedEntities = new ThreadLocal<Set<EntityType>>()
		{
			protected Set<EntityType> initialValue()
			{
				return new HashSet<EntityType>();
			}
		};

	public static void enterLocalScope()
	{
		getScopeStack().add(createScope(LOCAL_SCOPE_TYPES));
	}

	public static void leaveLocalScope()
			throws LocalScopeAbsentException
	{
		if (getScopeStack().size() == 1)
		{
			throw new LocalScopeAbsentException("There are no local scope to leave");
		}

		getScopeStack().remove(getScopeStack().size() - 1);
	}

	public static List<Map<EntityType, Map<String, Object[]>>> createScopeStackForNewThread()
	{
		List<Map<EntityType, Map<String, Object[]>>> source = getScopeStack();
		List<Map<EntityType, Map<String, Object[]>>> target = new LinkedList<Map<EntityType, Map<String, Object[]>>>();

		for (Map<EntityType, Map<String, Object[]>> localScope : source)
		{
			target.add(localScope);
		}

		target.add(createScope(LOCAL_SCOPE_TYPES));

		return target;
	}

	public static void initScopeStackForNewThread(List<Map<EntityType, Map<String, Object[]>>> newStack)
	{
		if (newStack == null || newStack.isEmpty())
		{
			throw new IllegalArgumentException("Assigned thread stack can't be empty");
		}

		List<Map<EntityType, Map<String, Object[]>>> currentStack = getScopeStack();
		currentStack.clear();

		for (Map<EntityType, Map<String, Object[]>> scope : newStack)
		{
			currentStack.add(scope);
		}
	}

	public static void destroyScopeStackForCompletedThread()
	{
		getScopeStack().clear();
	}

	/**
	 * Checks if entity exists either in CURRENT or PARENT or GLOBAL scopes
	 * @param type entity type
	 * @param name entity name
	 * @return flag indicating if entity with the given name exists
	 */
	public static boolean existsInCPGScopes(EntityType type, String name)
			throws ParentScopeAbsentException
	{
		if (name == null)
		{
			throw new IllegalArgumentException("Entity name can't be empty");
		}

		if (isParentScopeEntity(name))
		{
			Map<String, Object[]> storage = getParentScopeStorage(type, name);
			return storage != null && storage.get(removeParentScopePrefix(name)) != null;
		}

		Map<String, Object[]> storage = isGlobalScopeEntity(name) ? GLOBAL_SCOPE.get(type) : getCurrentScopeStorage(type);

		return storage != null && storage.get(name) != null;
	}

	public static void setEntity(EntityType type, String name, Object value, boolean ignoreIfAlreadyExist)
			throws EntityStorageAbsentException
	{
		if (name == null)
		{
			throw new IllegalArgumentException("Entity name can't be empty");
		}

		if (skippedEntities.get().contains(type))
		{
			throw new EntityStorageAbsentException("There are no storage for the " + type + " entity '" + name + "'");
		}

		Map<String, Object[]> storage;

		if (isGlobalScopeEntity(name))
		{
			storage = GLOBAL_SCOPE.get(type);
		}
		else if (isParentScopeEntity(name))
		{
			storage = getParentScopeStorage(type, name);
		}
		else
		{
			storage = getCurrentScopeStorage(type);
		}

		if (storage == null)
		{
			throw new EntityStorageAbsentException("There are no storage for the " + type + " entity '" + name + "'");
		}

		// prevents from setting the same entity from different threads
		synchronized (storage)
		{
			String _name = isParentScopeEntity(name) ? removeParentScopePrefix(name) : name;

			if (!ignoreIfAlreadyExist || !storage.containsKey(_name))
			{
				storage.put(_name, wrapEntityWithLock(value));
			}
		}
	}

	public static Object removeEntity(EntityType type, String name)
			throws ParentScopeAbsentException
	{
		if (name == null)
		{
			throw new IllegalArgumentException("Entity name can't be empty");
		}

		if (skippedEntities.get().contains(type))
		{
			return null;
		}

		if (isGlobalScopeEntity(name))
		{
			Map<String, Object[]> storage = GLOBAL_SCOPE.get(type);
			return storage == null ? null : storage.remove(name)[0];
		}

		if (isCurrentScopeEntity(name))
		{
			Map<String, Object[]> storage = getCurrentScopeStorage(type);
			return storage == null ? null : storage.remove(name)[0];
		}

		if (isParentScopeEntity(name))
		{
			Map<String, Object[]> storage = getParentScopeStorage(type, name);
			return storage == null ? null : storage.remove(removeParentScopePrefix(name))[0];
		}

		List<Map<EntityType, Map<String, Object[]>>> stack = getScopeStack();
		for (int i = stack.size() - 1; i > -1; i--)
		{
			Map<EntityType, Map<String, Object[]>> scope = stack.get(i);
			Map<String, Object[]> storage = scope.get(type);
			if (storage == null)
			{
				continue;
			}

			Object[] entityLockPair = storage.remove(name);
			if (entityLockPair != null)
			{
				return entityLockPair[0];
			}
		}

		return null;
	}

	public static <T, P, R> Object processEntity(EntityType type, String name, IEntityProcessor<T, P, R> processor, P param)
			throws EntityNotFoundException
	{
		if (processor == null)
		{
			throw new IllegalArgumentException("No processor specified");
		}

		if (name == null)
		{
			throw new IllegalArgumentException("Entity name can't be empty");
		}

		Object[] entityLockPair = getEntityLockPair(type, name);
		if (entityLockPair == null)
		{
			throw new EntityNotFoundException("There is no '" + name + "' entity of the type " + type);
		}

		T castedEntity;

		try
		{
			castedEntity = (T)entityLockPair[0];
		}
		catch (ClassCastException e)
		{
			throw new IllegalStateException("Entity of type/name " + type + "/" + name +" has incorrect class " +
					entityLockPair[0].getClass() + " which can't be casted to appropriate processor type class" , e);
		}

		// NOTE: Leads to deadlock in IO intensive cases on some versions of JDK. Official JDK bug is registered JDK-6822370 http://bugs.java.com/view_bug.do?bug_id=6822370
		// to work correctly you should use appropriate JDK version or JDK Patch/Hotfix
		ReadWriteLock readWriteLock = (ReadWriteLock)entityLockPair[1];
		Lock lock = processor.readOnly() ? readWriteLock.readLock() : readWriteLock.writeLock();

		if (MONITOR_LOCKS)
		{
			System.out.println("[OBTAINING_LOCK] " + (processor.readOnly() ? "READ" : "WRITE") + " lock on '" + name + "' entity of type " + type);
		}

		lock.lock();

		if (MONITOR_LOCKS)
		{
			System.out.println("[OBTAINED_LOCK] " + (processor.readOnly() ? "READ" : "WRITE") + " lock on '" + name + "' entity of type " + type);
		}

		try
		{
			return processor.processEntity(castedEntity, param);
		}
		finally
		{
			lock.unlock();

			if (MONITOR_LOCKS)
			{
				System.out.println("[RELEASED_LOCK] " + (processor.readOnly() ? "READ" : "WRITE") + " lock on '" + name + "' entity of type " + type);
			}
		}
	}

	public static boolean exists(EntityType type, String name)
			throws ParentScopeAbsentException
	{
		return getEntityLockPair(type, name) != null;
	}

	public static String removeParentScopePrefix(String entityName)
	{
		return entityName.replace(PARENT_SCOPE_PREFIX, "");
	}

	public static void skipEntitiesProcessing(EntityType type)
	{
		skippedEntities.get().add(type);
	}

	public static void resumeEntitiesProcessing(EntityType type)
	{
		skippedEntities.get().remove(type);
	}

	private static Object[] getEntityLockPair(EntityType type, String name)
			throws ParentScopeAbsentException
	{
		if (name == null)
		{
			throw new IllegalArgumentException("Entity name can't be empty");
		}

		if (skippedEntities.get().contains(type))
		{
			return null;
		}

		if (isGlobalScopeEntity(name))
		{
			Map<String, Object[]> storage = GLOBAL_SCOPE.get(type);
			return storage == null ? null : storage.get(name);
		}

		if (isCurrentScopeEntity(name))
		{
			Map<String, Object[]> storage = getCurrentScopeStorage(type);
			return storage == null ? null : storage.get(name);
		}

		if (isParentScopeEntity(name))
		{
			Map<String, Object[]> storage = getParentScopeStorage(type, name);
			return storage == null ? null : storage.get(removeParentScopePrefix(name));
		}

		List<Map<EntityType, Map<String, Object[]>>> stack = getScopeStack();
		for (int i = stack.size() - 1; i > -1; i--)
		{
			Map<EntityType, Map<String, Object[]>> scope = stack.get(i);
			Map<String, Object[]> storage = scope.get(type);
			if (storage == null)
			{
				continue;
			}

			Object[] entityLockPair = storage.get(name);
			if (entityLockPair != null)
			{
				return entityLockPair;
			}
		}

		return null;
	}

	private static List<Map<EntityType, Map<String, Object[]>>> getScopeStack()
	{
		return scopeStack.get();
	}

	private static Map<EntityType, Map<String, Object[]>> createScope(List<EntityType> scopeTypes)
	{
		Map<EntityType, Map<String, Object[]>> scope = new ConcurrentHashMap<EntityType, Map<String, Object[]>>();
		for (EntityType type : scopeTypes)
		{
			scope.put(type, new ConcurrentHashMap<String, Object[]>());
		}

		return scope;
	}

	private static Object[] wrapEntityWithLock(Object entity)
	{
		return new Object[] {entity, new ReentrantReadWriteLock(true)};
	}

	private static boolean isGlobalScopeEntity(String name)
	{
		return name != null && name.trim().startsWith(GLOBAL_SCOPE_PREFIX);
	}

	private static boolean isCurrentScopeEntity(String name)
	{
		return name != null && name.trim().startsWith(CURRENT_SCOPE_PREFIX);
	}

	private static boolean isParentScopeEntity(String name)
	{
		return name != null && name.trim().startsWith(PARENT_SCOPE_PREFIX);
	}

	private static Map<String, Object[]> getCurrentScopeStorage(EntityType type)
	{
		List<Map<EntityType, Map<String, Object[]>>> stack = getScopeStack();
		Map<EntityType, Map<String, Object[]>> scope = stack.get(stack.size() - 1);
		return scope.get(type);
	}

	private static Map<String, Object[]> getParentScopeStorage(EntityType type, String entityName)
			throws ParentScopeAbsentException
	{
		int parentLevel = 0;
		int index = 0;

		while (index < entityName.length() - 1 && (index = entityName.indexOf(PARENT_SCOPE_PREFIX, index)) != -1)
		{
			parentLevel++;
			index += PARENT_SCOPE_PREFIX.length();
		}

		if (parentLevel == 0)
		{
			throw new IllegalArgumentException("Entity '" + entityName + "' doesn't contain any parent scope specification");
		}

		List<Map<EntityType, Map<String, Object[]>>> stack = getScopeStack();

		//if entity type is not allowed for ROOT scope it`s actual count of stack storages is smaller
		int storagesCount = stack.size() - (type.allowedForRootScope() ? 0 : 1);
		if (storagesCount <= parentLevel)
		{
			throw new ParentScopeAbsentException(storagesCount - 1, parentLevel, "Entity '" + entityName + "' specifies too high parent scope which is not exist");
		}

		Map<EntityType, Map<String, Object[]>> scope = stack.get(stack.size() - (parentLevel + 1));

		return scope.get(type);
	}
}
