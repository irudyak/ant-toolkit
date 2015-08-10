package com.anttoolkit.jasperreports.report;

import java.util.*;

import org.apache.tools.ant.*;

import net.sf.jasperreports.engine.*;
import net.sf.jasperreports.engine.data.*;

import com.anttoolkit.general.entities.*;
import com.anttoolkit.general.tasks.collections.array.util.*;
import com.anttoolkit.general.tasks.collections.map.util.*;

public class ReportParam
		extends TypeAdapter
		implements IDataSourceProcessor<JRDataSource>
{
	private class DataSourceArrayProcessor extends ArrayProcessor<Void, List<JRDataSource>>
	{
		@Override
		public List<JRDataSource> processEntity(List<String> entity, Void param)
		{
			List<JRDataSource> result = new LinkedList<JRDataSource>();

			for (String dataSource : entity)
			{
				result.add((JRDataSource)DataSourceManager.processDataSource(dataSource, ReportParam.this));
			}

			return result;
		}

		@Override
		public boolean readOnly()
		{
			return true;
		}
	}

	private class DataSourceMapProcessor extends MapProcessor<Void, Map<String, JRDataSource>>
	{
		@Override
		public Map<String, JRDataSource> processEntity(Map<String, String> entity, Void param)
		{
			Map<String, JRDataSource> result = new HashMap<String, JRDataSource>();

			for (String dataSource : entity.keySet())
			{
				result.put(dataSource, (JRDataSource)DataSourceManager.processDataSource(dataSource, ReportParam.this));
			}

			return result;
		}

		@Override
		public boolean readOnly()
		{
			return true;
		}
	}

	private String name;
	private String value;
	private String dataSource;
	private String dataSourceArray;
	private String dataSourceMap;

	public void setName(String name)
	{
		this.name = name;
	}

	public String getName()
	{
		return name;
	}

	public void setValue(String value)
	{
		this.value = value;
	}

	public void setDataSource(String dataSource)
	{
		this.dataSource = dataSource;
	}

	public void setDataSourceArray(String array)
	{
		this.dataSourceArray = array;
	}

	public void setDataSourceMap(String map)
	{
		this.dataSourceMap = map;
	}

	public Object getValue()
	{
		if (value != null)
		{
			return convertToPrimaryObject(value);
		}

		if (dataSource != null)
		{
			return DataSourceManager.processDataSource(dataSource, this);
		}

		if (dataSourceArray != null)
		{
			try
			{
				return EntityManager.processEntity(ArrayEntityType.instance, dataSourceArray, new DataSourceArrayProcessor(), null);
			}
			catch (EntityNotFoundException e)
			{
				throw new BuildException("There was no array " + name + " previously initialized", e);
			}
		}

		if (dataSourceMap != null)
		{
			try
			{
				return EntityManager.processEntity(MapEntityType.instance, dataSourceMap, new DataSourceMapProcessor(), null);
			}
			catch (EntityNotFoundException e)
			{
				throw new BuildException("There was no array " + name + " previously initialized", e);
			}
		}

		throw new BuildException("No values specified for the parameter: " + name);
	}

	@Override
	public JRDataSource processDataSource(Collection<?> coll)
	{
		Iterator<?> iter = coll.iterator();
		Object firstObj = !iter.hasNext() ? null : iter.next();

		return firstObj != null && firstObj instanceof Map ?
				new JRMapCollectionDataSource((Collection<Map<String,?>>)coll) :
				new JRBeanCollectionDataSource(coll);
	}

	public void validate()
	{
		if (name == null || name.trim().isEmpty())
		{
			throw new BuildException("Parameter name should be specified");
		}

		if ((value == null && dataSource == null && dataSourceArray == null && dataSourceMap == null) ||
			(value != null && dataSource != null && dataSourceArray != null && dataSourceMap != null))
		{
			throw new BuildException("Either parameter value or DataSource or DataSourceArray or DataSourceMap should be specified for report parameter");
		}

		if (dataSource != null && !DataSourceManager.exists(dataSource))
		{
			throw new BuildException("Specified DataSource '" + dataSource + "' doesn't exist");
		}

		if (dataSourceArray != null && !ArrayManager.exists(dataSourceArray))
		{
			throw new BuildException("Specified DataSourceArray '" + dataSourceArray + "' doesn't exist");
		}

		if (dataSourceMap != null && !MapManager.exists(dataSourceMap))
		{
			throw new BuildException("Specified DataSourceMap '" + dataSourceMap + "' doesn't exist");
		}
	}
}
