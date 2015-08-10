package com.anttoolkit.hadoop.tasks.yarn.util;

import org.apache.hadoop.yarn.api.records.*;

public class ApplicationResourceUsageReportWrapper
{
	public final String usedContainers;
	public final String reservedContainers;
	public final String usedResources;
	public final String reservedResources;
	public final String neededResources;
	public final String memSeconds;
	public final String vcoreSeconds;

	public ApplicationResourceUsageReportWrapper(ApplicationResourceUsageReport report)
	{
		usedContainers = report == null ? "" : Integer.toString(report.getNumUsedContainers());
		reservedContainers = report == null ? "" : Integer.toString(report.getNumReservedContainers());
		usedResources = report == null ? "" : report.getUsedResources().toString();
		reservedResources = report == null ? "" : report.getReservedResources().toString();
		neededResources = report == null ? "" : report.getNeededResources().toString();
		memSeconds = report == null ? "" : Long.toString(report.getMemorySeconds());
		vcoreSeconds = report == null ? "" : Long.toString(report.getVcoreSeconds());
	}
}
