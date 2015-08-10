package com.anttoolkit.maestro.tasks.report;

import java.util.*;

import org.apache.tools.ant.*;

import com.maestro.xml.*;
import com.maestro.cli.entity.*;

import com.anttoolkit.maestro.common.*;

@DefaultParams(params = {"report-type"}, values = {"subtotal"})
public class SubtotalReportTask extends ReportTask
{
	private String currencyProperty;
	private String baseCostProperty;
	private String cpuCostProperty;
	private String memoryCostProperty;
	private String activeCheckpointCostProperty;
	private String passiveCheckpointCostProperty;
	private String imageCostProperty;
	private String initCostProperty;
	private String activeVolumeCostProperty;
	private String passiveVolumeCostProperty;
	private String totalCostProperty;

	public void setCurrencyProperty(String property)
	{
		this.currencyProperty = property;
	}

	public void setBaseCostProperty(String property)
	{
		this.baseCostProperty = property;
	}

	public void setCpuCostProperty(String property)
	{
		this.cpuCostProperty = property;
	}

	public void setMemoryCostProperty(String property)
	{
		this.memoryCostProperty = property;
	}

	public void setActiveCheckpointCostProperty(String property)
	{
		this.activeCheckpointCostProperty = property;
	}

	public void setPassiveCheckpointCostProperty(String property)
	{
		this.passiveCheckpointCostProperty = property;
	}

	public void setImageCostProperty(String property)
	{
		this.imageCostProperty = property;
	}

	public void setInitCostProperty(String property)
	{
		this.initCostProperty = property;
	}

	public void setActiveVolumeCostProperty(String property)
	{
		this.activeVolumeCostProperty = property;
	}

	public void setPassiveVolumeCostProperty(String property)
	{
		this.passiveVolumeCostProperty = property;
	}

	public void setTotalCostProperty(String property)
	{
		this.totalCostProperty = property;
	}

	@Override
	public void doWork() throws BuildException
	{
		XMLElement element;
		try
		{
			element = executeCommand();
		}
		catch (SystemExitException e)
		{
			if (e.getCode() != 61)
			{
				throw e;
			}

			element = XMLElement.newInstance();
		}

		List<XMLElement> items = element.Items();

		Map<String, String> results = new HashMap<String, String>();

		for (XMLElement item : items)
		{
			XMLAttribute attr = item.getAttribute("type");
			if (attr == null)
			{
				throw new BuildException("Incorrect report item was returned: " + item.toText());
			}

			if ("Item".equals(attr.getValue()))
			{
				processItem(item, results);
			}
			else if ("Total".equals(attr.getValue()))
			{
				processTotal(item, results);
			}
		}

		setProperties(results);
	}

	private void processTotal(XMLElement element, Map<String, String> results)
	{
		XMLAttribute totalAttr = element.getAttribute("total");
		XMLAttribute currencyAttr = element.getAttribute("currency");
		if (totalAttr == null || currencyAttr == null)
		{
			throw new BuildException("Incorrect report item was returned: " + element.toText());
		}

		results.put("Total", totalAttr.getValue() == null ? "0" : totalAttr.getValue());
		results.put("Currency", currencyAttr.getValue() == null ? "0" : currencyAttr.getValue());
	}

	private void processItem(XMLElement element, Map<String, String> results)
	{
		XMLAttribute usageTypeAttr = element.getAttribute("usageType");
		if (usageTypeAttr == null)
		{
			throw new BuildException("Incorrect report item was returned: " + element.toText());
		}

		XMLAttribute totalAttr = element.getAttribute("total");
		if (totalAttr == null)
		{
			throw new BuildException("Incorrect report item was returned: " + element.toText());
		}

		String usageType = usageTypeAttr.getValue();
		String total = totalAttr.getValue();
		total = total == null ? "0" : total;

		results.put(usageType, total);
	}

	private void setProperties(Map<String, String> results)
	{
		if (totalCostProperty != null)
		{
			this.setPropertyThreadSafe(totalCostProperty, getResultValue("Total", results));
		}

		if (currencyProperty != null)
		{
			this.setPropertyThreadSafe(currencyProperty, getResultValue("Currency", results, "USD"));
		}

		if (baseCostProperty != null)
		{
			this.setPropertyThreadSafe(baseCostProperty, getResultValue("Base", results));
		}

		if (cpuCostProperty != null)
		{
			this.setPropertyThreadSafe(cpuCostProperty, getResultValue("CPU-Usage", results));
		}

		if (initCostProperty != null)
		{
			this.setPropertyThreadSafe(initCostProperty, getResultValue("Initialization", results));
		}

		if (memoryCostProperty != null)
		{
			this.setPropertyThreadSafe(memoryCostProperty, getResultValue("Memory-Usage", results));
		}

		if (activeCheckpointCostProperty != null)
		{
			this.setPropertyThreadSafe(activeCheckpointCostProperty, getResultValue("Checkpoint-Usage:Active", results));
		}

		if (passiveCheckpointCostProperty != null)
		{
			this.setPropertyThreadSafe(passiveCheckpointCostProperty, getResultValue("Checkpoint-Usage:Passive", results));
		}

		if (activeVolumeCostProperty != null)
		{
			this.setPropertyThreadSafe(activeVolumeCostProperty, getResultValue("Volume-Usage:Active", results));
		}

		if (passiveVolumeCostProperty != null)
		{
			this.setPropertyThreadSafe(passiveVolumeCostProperty, getResultValue("Volume-Usage:Passive", results));
		}

		if (imageCostProperty != null)
		{
			this.setPropertyThreadSafe(imageCostProperty, getResultValue("MachineImage-Usage", results));
		}
	}

	private String getResultValue(String key, Map<String, String> results)
	{
		return getResultValue(key, results, "0");
	}

	private String getResultValue(String key, Map<String, String> results, String defaultVal)
	{
		return results.containsKey(key) ? results.get(key) : defaultVal;
	}
}
