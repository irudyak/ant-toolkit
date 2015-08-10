package com.anttoolkit.aws.ec2.tasks.instance;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.aws.ec2.common.*;
import com.anttoolkit.aws.ec2.tasks.*;

public abstract class InstanceTask extends GenericEc2Task
{
	public static final InstanceStatus PENDING_STATUS = InstanceStatusHelper.getStatus("pending");
	public static final InstanceStatus RUNNING_STATUS = InstanceStatusHelper.getStatus("running", "ok");
	public static final InstanceStatus SHUTTING_DOWN_STATUS = InstanceStatusHelper.getStatus("shutting-down");
	public static final InstanceStatus TERMINATED_STATUS = InstanceStatusHelper.getStatus("terminated");
	public static final InstanceStatus STOPPING_STATUS = InstanceStatusHelper.getStatus("stopping");
	public static final InstanceStatus STOPPED_STATUS = InstanceStatusHelper.getStatus("stopped");

	public static final Map<String, InstanceStatus> SUPPORTED_STATES = new HashMap<String, InstanceStatus>()
	{{
		put(PENDING_STATUS.getInstanceState().getName(), PENDING_STATUS);
		put(RUNNING_STATUS.getInstanceState().getName(), RUNNING_STATUS);
		put(SHUTTING_DOWN_STATUS.getInstanceState().getName(), SHUTTING_DOWN_STATUS);
		put(TERMINATED_STATUS.getInstanceState().getName(), TERMINATED_STATUS);
		put(STOPPING_STATUS.getInstanceState().getName(), STOPPING_STATUS);
		put(STOPPED_STATUS.getInstanceState().getName(), STOPPED_STATUS);
	}};

	public static final long WAIT_TIME = 1200000;	//20 mins
	public static final long SLEEP_TIMEOUT = 60000;	//1min

	private boolean async = false;
	private long wait = WAIT_TIME;

	public void setAsync(boolean async)
	{
		this.async = async;
	}

	public boolean isAsync()
	{
		return async;
	}

	public void setWait(long wait)
	{
		if (wait <= 0)
		{
			throw new BuildException("Incorrect wait time specified: " + wait);
		}

		this.wait = wait;
	}

	protected Collection<String> getInstanceIds(Reservation res)
	{
		List<String> ids = new LinkedList<String>();

		if (res == null || res.getInstances() == null || res.getInstances().isEmpty())
		{
			return ids;
		}

		for (Instance inst : res.getInstances())
		{
			if (!ids.contains(inst.getInstanceId()))
			{
				ids.add(inst.getInstanceId());
			}
		}

		return ids;
	}

	protected Collection<String> getInstanceIds(List<InstanceStateChange> stateChanges)
	{
		List<String> ids = new LinkedList<String>();

		if (stateChanges == null || stateChanges.isEmpty())
		{
			return ids;
		}

		for (InstanceStateChange change : stateChanges)
		{
			if (!ids.contains(change.getInstanceId()))
			{
				ids.add(change.getInstanceId());
			}
		}

		return ids;
	}

	protected void waitInstancesStatusToChange(Collection<String> instanceIds, InstanceStatus status, boolean verbose)
	{
		if (instanceIds == null || instanceIds.isEmpty())
		{
			return;
		}

		String _instState = InstanceStatusHelper.getState(status);
		String _instStatus = InstanceStatusHelper.getInstanceStatus(status);
		String _sysStatus = InstanceStatusHelper.getSystemStatus(status);

		if (_instState == null || _instState.trim().isEmpty())
		{
			throw new IllegalArgumentException("Incorrect instance status specified: " + InstanceStatusHelper.toString(status));
		}

		long start = System.currentTimeMillis();

		Set<String> readyInstances = new HashSet<String>();

		while (System.currentTimeMillis() - start < wait)
		{
			try
			{
				Map<String, Instance> instances = getInstances(instanceIds);
				Map<String, InstanceStatus> statuses = _instStatus != null || _sysStatus != null ? getInstancesStatus(instanceIds) : null;

				for (String id : instanceIds)
				{
					InstanceStatus st = statuses != null ? statuses.get(id) : null;

					String instState = instances.get(id).getState().getName();
					String instStatus = InstanceStatusHelper.getInstanceStatus(st);
					String sysStatus = InstanceStatusHelper.getSystemStatus(st);

					if (!_instState.equals(instState) ||
						(_instStatus != null && !_instStatus.equals(instStatus)) ||
						(_sysStatus != null && !_sysStatus.equals(sysStatus)))
					{
						continue;
					}

					readyInstances.add(id);

					if (verbose)
					{
						log("Instance " + id + " switched to '" + InstanceStatusHelper.toString(status) + "' status");
					}
				}

				if (readyInstances.size() == instanceIds.size())
				{
					return;
				}

				Thread.sleep(SLEEP_TIMEOUT);
			}
			catch (InterruptedException e)
			{
				throw new BuildException("Waiting for instances '" + CollectionsHelper.toString(instanceIds) +
						"' to switch to '" + InstanceStatusHelper.toString(status) + "' status was interrupted", e);
			}
		}

		if (readyInstances.size() < instanceIds.size())
		{
			throw new BuildException("Wait time exceeded, but not all instances '" +
					CollectionsHelper.toString(instanceIds) + "' switched to '" + InstanceStatusHelper.toString(status) + "' status");
		}
	}

	protected Map<String, Instance> getInstances(Collection<String> instanceIds)
			throws InstanceNotFoundException
	{
		if (instanceIds.isEmpty())
		{
			return new HashMap<String, Instance>();
		}

		DescribeInstancesRequest request = new DescribeInstancesRequest();
		request.setInstanceIds(instanceIds);

		DescribeInstancesResult result;

		try
		{
			result = getEc2Client().describeInstances(request);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to obtain information about instances: " + CollectionsHelper.toString(instanceIds), e);
		}

		Map<String, Instance> instancesInfo = new HashMap<String, Instance>();

		for (Reservation res : result.getReservations())
		{
			List<Instance> instances = res.getInstances();
			if (instances == null || instances.isEmpty())
			{
				continue;
			}

			for (Instance instance : instances)
			{
				instancesInfo.put(instance.getInstanceId(), instance);
			}
		}

		for (String id : instanceIds)
		{
			if (!instancesInfo.containsKey(id))
			{
				throw new InstanceNotFoundException(id, "There is no instance with id: " + id);
			}
		}

		return instancesInfo;
	}

	protected Map<String, InstanceStatus> getInstancesStatus(Collection<String> instanceIds)
			throws InstanceNotFoundException
	{
		if (instanceIds.isEmpty())
		{
			return new HashMap<String, InstanceStatus>();
		}

		DescribeInstanceStatusRequest request = new DescribeInstanceStatusRequest();
		request.setInstanceIds(instanceIds);

		DescribeInstanceStatusResult result;

		try
		{
			result = getEc2Client().describeInstanceStatus(request);
		}
		catch (Throwable e)
		{
			throw new BuildException("Failed to obtain statuses information for the instances: " + CollectionsHelper.toString(instanceIds), e);
		}

		Map<String, InstanceStatus> statusesInfo = new HashMap<String, InstanceStatus>();

		for (InstanceStatus status : result.getInstanceStatuses())
		{
			statusesInfo.put(status.getInstanceId(), status);
		}

		return statusesInfo;
	}
}
