package com.anttoolkit.hadoop.tasks.yarn;

import java.text.*;
import java.util.*;

import org.apache.hadoop.yarn.api.records.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.yarn.util.*;

public class GetApplicationInfoTask extends GenericYarnTask
{
	private String applicationId;
	private String dateFormat;

	private String nameProperty;
	private String typeProperty;
	private String userProperty;
	private String queueProperty;
	private String startTimeProperty;
	private String finishTimeProperty;
	private String progressProperty;
	private String stateProperty;
	private String finalStateProperty;
	private String urlProperty;
	private String rpcPortProperty;
	private String amHostProperty;
	private String diagnosticsProperty;

	private String usedContainersProperty;
	private String reservedContainersProperty;
	private String usedResourcesProperty;
	private String reservedResourcesProperty;
	private String neededResourcesProperty;
	private String memSecondsProperty;
	private String vcoreSecondsProperty;

	public void setApplicationId(String id)
	{
		this.applicationId = id;
	}

	public void setDateFormat(String format)
	{
		dateFormat = format;
	}

	public void setNameProperty(String property)
	{
		this.nameProperty = property;
	}

	public void setTypeProperty(String property)
	{
		this.typeProperty = property;
	}

	public void setUserProperty(String property)
	{
		this.userProperty = property;
	}

	public void setQueueProperty(String property)
	{
		this.queueProperty = property;
	}

	public void setStartTimeProperty(String property)
	{
		this.startTimeProperty = property;
	}

	public void setFinishTimeProperty(String property)
	{
		this.finishTimeProperty = property;
	}

	public void setProgressProperty(String property)
	{
		this.progressProperty = property;
	}

	public void setStateProperty(String property)
	{
		this.stateProperty = property;
	}

	public void setFinalStateProperty(String property)
	{
		this.finalStateProperty = property;
	}

	public void setUrlProperty(String property)
	{
		this.urlProperty = property;
	}

	public void setRpcPortProperty(String property)
	{
		this.rpcPortProperty = property;
	}

	public void setAmHostProperty(String property)
	{
		this.amHostProperty = property;
	}

	public void setDiagnosticsProperty(String property)
	{
		this.diagnosticsProperty = property;
	}

	public void setUsedContainersProperty(String property)
	{
		this.usedContainersProperty = property;
	}

	public void setReservedContainersProperty(String property)
	{
		this.reservedContainersProperty = property;
	}

	public void setUsedResourcesProperty(String property)
	{
		this.usedResourcesProperty = property;
	}

	public void setReservedResourcesProperty(String property)
	{
		this.reservedResourcesProperty = property;
	}

	public void setNeededResourcesProperty(String property)
	{
		this.neededResourcesProperty = property;
	}

	public void setMemSecondsProperty(String property)
	{
		this.memSecondsProperty = property;
	}

	public void setVcoreSecondsProperty(String property)
	{
		this.vcoreSecondsProperty = property;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		ApplicationReport appReport = getApplicationReport(applicationId);

		SimpleDateFormat formatter = dateFormat == null ?
				new SimpleDateFormat("MM/dd/yyyy hh:mm:ss") :
				new SimpleDateFormat(dateFormat);

		if (nameProperty != null)
		{
			this.setPropertyThreadSafe(nameProperty, appReport == null ? "" : appReport.getName());
		}

		if (typeProperty != null)
		{
			this.setPropertyThreadSafe(typeProperty, appReport == null ? "" : appReport.getApplicationType());
		}

		if (userProperty != null)
		{
			this.setPropertyThreadSafe(userProperty, appReport == null ? "" : appReport.getUser());
		}

		if (queueProperty != null)
		{
			this.setPropertyThreadSafe(queueProperty, appReport == null ? "" : appReport.getQueue());
		}

		if (startTimeProperty != null)
		{
			this.setPropertyThreadSafe(startTimeProperty, appReport == null ? "" : formatter.format(new Date(appReport.getStartTime())));
		}

		if (finishTimeProperty != null)
		{
			this.setPropertyThreadSafe(finishTimeProperty, appReport == null ? "" : formatter.format(new Date(appReport.getFinishTime())));
		}

		if (progressProperty != null)
		{
			DecimalFormat decFormatter = new DecimalFormat("###.##");
			this.setPropertyThreadSafe(progressProperty, appReport == null ? "" : decFormatter.format(appReport.getProgress()));
		}

		if (stateProperty != null)
		{
			this.setPropertyThreadSafe(stateProperty, appReport == null ? "" : appReport.getYarnApplicationState().toString());
		}

		if (finalStateProperty != null)
		{
			this.setPropertyThreadSafe(finalStateProperty, appReport == null ? "" : appReport.getFinalApplicationStatus().toString());
		}

		if (urlProperty != null)
		{
			this.setPropertyThreadSafe(urlProperty, appReport == null ? "" : appReport.getOriginalTrackingUrl());
		}

		if (rpcPortProperty != null)
		{
			this.setPropertyThreadSafe(rpcPortProperty, appReport == null ? "" : Integer.toString(appReport.getRpcPort()));
		}

		if (amHostProperty != null)
		{
			this.setPropertyThreadSafe(amHostProperty, appReport == null ? "" : appReport.getHost());
		}

		if (diagnosticsProperty != null)
		{
			this.setPropertyThreadSafe(diagnosticsProperty, appReport == null ? "" : appReport.getDiagnostics());
		}

		if (diagnosticsProperty != null)
		{
			this.setPropertyThreadSafe(diagnosticsProperty, appReport == null ? "" : appReport.getDiagnostics());
		}

		if (usedContainersProperty != null)
		{
			this.setPropertyThreadSafe(usedContainersProperty, appReport == null ? "" : appReport.getDiagnostics());
		}

		ApplicationResourceUsageReportWrapper wrapper = new ApplicationResourceUsageReportWrapper(appReport == null ? null : appReport.getApplicationResourceUsageReport());

		if (usedContainersProperty != null)
		{
			this.setPropertyThreadSafe(usedContainersProperty, wrapper.usedContainers);
		}

		if (reservedContainersProperty != null)
		{
			this.setPropertyThreadSafe(reservedContainersProperty, wrapper.reservedContainers);
		}

		if (usedResourcesProperty != null)
		{
			this.setPropertyThreadSafe(usedResourcesProperty, wrapper.usedResources);
		}

		if (reservedResourcesProperty != null)
		{
			this.setPropertyThreadSafe(reservedResourcesProperty, wrapper.reservedResources);
		}

		if (neededResourcesProperty != null)
		{
			this.setPropertyThreadSafe(neededResourcesProperty, wrapper.neededResources);
		}

		if (memSecondsProperty != null)
		{
			this.setPropertyThreadSafe(memSecondsProperty, wrapper.memSeconds);
		}

		if (vcoreSecondsProperty != null)
		{
			this.setPropertyThreadSafe(vcoreSecondsProperty, wrapper.vcoreSeconds);
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (applicationId == null || applicationId.trim().isEmpty())
		{
			throw new BuildException("ApplicationId should be specified");
		}
	}
}
