package com.anttoolkit.aws.ec2.common;

import com.amazonaws.services.ec2.model.*;

public class InstanceStatusHelper
{
	public static boolean equals(InstanceStatus status1, InstanceStatus status2)
	{
		if (status1 == null || status2 == null)
		{
			return false;
		}

		String state1 = status1.getInstanceState().getName();
		String instStat1 = status1.getInstanceStatus().getStatus();
		String sysStat1 = status1.getSystemStatus().getStatus();

		String state2 = status2.getInstanceState().getName();
		String instStat2 = status2.getInstanceStatus().getStatus();
		String sysStat2 = status2.getSystemStatus().getStatus();

		if (!state1.equals(state2))
		{
			return false;
		}

		if ((instStat1 != null && instStat2 == null) ||
			(instStat1 == null && instStat2 != null) ||
			(instStat1 != null && !instStat1.equals(instStat2)))
		{
			return false;
		}

		if ((sysStat1 != null && sysStat2 == null) ||
			(sysStat1 == null && sysStat2 != null) ||
			(sysStat1 != null && !sysStat1.equals(sysStat2)))
		{
			return false;
		}

		return true;
	}

	public static String toString(InstanceStatus status)
	{
		String state = getState(status);
		state = state != null ? state : "";

		String instStat = getInstanceStatus(status);
		instStat = instStat != null ? instStat : "";

		String sysStat = getSystemStatus(status);
		sysStat = sysStat != null ? sysStat : "";

		StringBuilder builder = new StringBuilder(state);

		if (!instStat.isEmpty() || !sysStat.isEmpty())
		{
			builder.append(",").append(instStat);
		}

		if (!sysStat.isEmpty())
		{
			builder.append(",").append(sysStat);
		}

		return builder.toString();
	}

	public static InstanceStatus getStatus(String state)
	{
		return getStatus(state, null, null);
	}

	public static InstanceStatus getStatus(String state, String instanceStatus)
	{
		return getStatus(state, instanceStatus, null);
	}

	public static InstanceStatus getStatus(String state, String instanceStatus, String systemStatus)
	{
		InstanceState st = new InstanceState();
		st.setName(state);

		InstanceStatus status = new InstanceStatus();
		status.setInstanceState(st);

		if (instanceStatus != null)
		{
			InstanceStatusSummary summary = new InstanceStatusSummary();
			summary.setStatus(instanceStatus);
			status.setInstanceStatus(summary);
		}

		if (systemStatus != null)
		{
			InstanceStatusSummary summary = new InstanceStatusSummary();
			summary.setStatus(systemStatus);
			status.setSystemStatus(summary);
		}

		return status;
	}

	public static String getState(InstanceStatus status)
	{
		return status != null && status.getInstanceState() != null &&
				status.getInstanceState().getName() != null ?
				status.getInstanceState().getName().trim() : null;
	}

	public static String getInstanceStatus(InstanceStatus status)
	{
		return status != null && status.getInstanceStatus() != null &&
				status.getInstanceStatus().getStatus() != null ?
				status.getInstanceStatus().getStatus().trim() : null;
	}

	public static String getSystemStatus(InstanceStatus status)
	{
		return status != null && status.getSystemStatus() != null &&
				status.getSystemStatus().getStatus() != null ?
				status.getSystemStatus().getStatus().trim() : null;
	}

	public static boolean hasInstanceOrSystemStatus(InstanceStatus status)
	{
		return getInstanceStatus(status) != null || getSystemStatus(status) != null;
	}
}
