package com.anttoolkit.jasperreports.tasks;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.jasperreports.report.*;

public class AddToDataSourceTask
		extends GenericTask
		implements IDataSourceProcessor<Void>
{
	private List<DataSourceData> dataList = new LinkedList<DataSourceData>();

	private String dataSource;

	public void setDataSource(String dataSource)
	{
		this.dataSource = dataSource;
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
		DataSourceManager.processDataSource(dataSource, this);
	}

	@Override
	public Void processDataSource(Collection<?> coll)
	{
		if (!(coll instanceof DataSourceCollection))
		{
			throw new BuildException("Specified DataSource '" + dataSource + "' provides collection which is not of DataSourceCollection class");
		}

		((DataSourceCollection)coll).addData(dataList);

		return null;
	}

	@Override
	protected void validate()
	{
		if (dataSource == null || dataSource.trim().isEmpty())
		{
			throw new BuildException("DataSource name should be specified");
		}

		if (dataList.isEmpty())
		{
			throw new BuildException("There are no data to add to data source " + dataSource);
		}
	}
}
