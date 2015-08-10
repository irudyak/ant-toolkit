package com.anttoolkit.jasperreports.tasks;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.jasperreports.report.*;
import com.anttoolkit.general.tasks.*;

public class DataSourceTask
		extends GenericTask
{
	private List<DataSourceColumn> columns = new LinkedList<DataSourceColumn>();
	private List<DataSourceData> dataList = new LinkedList<DataSourceData>();

	private String name;

	public void setName(String name)
	{
		this.name = name;
	}

	public void addConfiguredColumn(DataSourceColumn column)
	{
		column.validate();
		columns.add(column);
	}

	public void addConfiguredData(DataSourceData data)
	{
		if (data.getFile() != null)
		{
			data.setFile(getFileFullPath(data.getFile()));
		}

		data.resolveProperties(this);

		dataList.add(data);
	}

	@Override
	public void doWork() throws BuildException
	{
		DataSourceCollection coll = new DataSourceCollection(columns, dataList);
		DataSourceManager.initDataset(name, coll);
	}

	@Override
	protected void validate()
	{
		if (name == null || name.trim().isEmpty())
		{
			throw new BuildException("DataSource name should be specified");
		}
	}
}
