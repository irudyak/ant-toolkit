package com.anttoolkit.hbase.tasks.data.util;

import org.apache.hadoop.hbase.client.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.entities.*;

public class BatchManager
{
	private static final IEntityProcessor ADD_ACTION = new IEntityProcessor<BatchAction, IAction, Void> ()
	{
		@Override
		public Void processEntity(BatchAction batch, IAction action)
		{
			batch.addAction(action);
			return null;
		}

		@Override
		public boolean readOnly()
		{
			return false;
		}
	};

	private static final IEntityProcessor EXECUTOR = new IEntityProcessor<BatchAction, HTableInterface, Void> ()
	{
		@Override
		public Void processEntity(BatchAction batch, HTableInterface table)
		{
			batch.execute(table);
			return null;
		}

		@Override
		public boolean readOnly()
		{
			return false;
		}
	};

	private static final IEntityProcessor ROW_MUTATION_EXECUTOR = new IEntityProcessor<BatchAction, HTableInterface, Void> ()
	{
		@Override
		public Void processEntity(BatchAction batch, HTableInterface table)
		{
			batch.executeAsRowMutation(table);
			return null;
		}

		@Override
		public boolean readOnly()
		{
			return false;
		}
	};

	public static void init(String batchId, BatchAction action)
	{
		try
		{
			EntityManager.setEntity(BatchEntityType.instance, batchId, action, false);
		}
		catch (EntityStorageAbsentException e)
		{
			throw new BuildException("There are no storage for HBase batch", e);
		}
	}

	public static void destroy(String batchId)
	{
		EntityManager.removeEntity(BatchEntityType.instance, batchId);
	}

	public static boolean exists(String batchId)
	{
		return EntityManager.exists(BatchEntityType.instance, batchId);
	}

	public static void addAction(String batchId, IAction action)
	{
		try
		{
			EntityManager.processEntity(BatchEntityType.instance, batchId, ADD_ACTION, action);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no batch " + batchId + " previously initialized", e);
		}
	}

	public static void execute(String batchId, HTableInterface table)
	{
		try
		{
			EntityManager.processEntity(BatchEntityType.instance, batchId, EXECUTOR, table);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no batch " + batchId + " previously initialized", e);
		}
	}

	public static void executeAsRowMutation(String batchId, HTableInterface table)
	{
		try
		{
			EntityManager.processEntity(BatchEntityType.instance, batchId, ROW_MUTATION_EXECUTOR, table);
		}
		catch (EntityNotFoundException e)
		{
			throw new BuildException("There was no batch " + batchId + " previously initialized", e);
		}
	}
}
