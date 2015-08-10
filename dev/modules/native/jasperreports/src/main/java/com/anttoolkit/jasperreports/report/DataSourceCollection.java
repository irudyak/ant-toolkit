package com.anttoolkit.jasperreports.report;

import java.io.*;
import java.util.*;

import net.sf.jasperreports.engine.*;
import net.sf.jasperreports.engine.data.*;

import org.apache.tools.ant.*;

public class DataSourceCollection extends LinkedList
{
	private List<DataSourceColumn> columns;
	private int maxColumnPosition = -1;

	public DataSourceCollection(List<DataSourceColumn> columns, List<DataSourceData> dataList)
	{
		this.columns = columns;
		validate();

		if (dataList != null && !dataList.isEmpty())
		{
			addData(dataList);
		}
	}

	public JRDataSource getReportDataSource()
	{
		Iterator<?> iter = iterator();
		Object firstObj = !iter.hasNext() ? null : iter.next();

		return firstObj != null && firstObj instanceof Map ?
				new JRMapCollectionDataSource((Collection<Map<String,?>>)this) :
				new JRBeanCollectionDataSource(this);
	}

	public void addData(List<DataSourceData> dataList)
	{
		if (dataList == null || dataList.isEmpty())
		{
			throw new IllegalArgumentException("Data list can't be empty");
		}

		for (DataSourceData data : dataList)
		{
			addData(data);
		}
	}

	public void addData(DataSourceData data)
	{
		if (data == null)
		{
			throw new IllegalArgumentException("Data can't be null");
		}

		if ((data.getText() == null || data.getText().trim().isEmpty()) &&
			(data.getFile() == null || data.getFile().trim().isEmpty()))
		{
			throw new IllegalArgumentException("Empty data provided");
		}

		if (data.getFile() != null)
		{
			File file = new File(data.getFile());
			if (!file.isFile() || !file.exists())
			{
				throw new BuildException("Incorrect data file specified: " + data.getFile());
			}
		}

		loadData(data);
	}

	private void validate()
	{
		if (columns.isEmpty())
		{
			throw new BuildException("Columns were not specified");
		}

		boolean posSpecified = false;

		for (int i = 0; i < columns.size(); i++)
		{
			DataSourceColumn column = columns.get(i);

			if (column.getPosition() == -1)
			{
				if (!posSpecified)
				{
					continue;
				}

				throw new BuildException("Either position for all or none data columns should be specified");
			}

			if (i == 0)
			{
				posSpecified = true;
				continue;
			}

			if (!posSpecified)
			{
				throw new BuildException("Either position for all or none data columns should be specified");
			}
		}
	}

	private void loadData(DataSourceData data)
	{
		InputStream in = null;
		InputStreamReader inReader = null;
		BufferedReader bufReader = null;

		if (data.getText() != null)
		{
			try
			{
				in = new ByteArrayInputStream(data.getText().getBytes("UTF-8"));
				inReader = new InputStreamReader(in);
				bufReader = new BufferedReader(inReader);

				loadData(bufReader, data);
			}
			catch (UnsupportedEncodingException e)
			{
				throw new BuildException("Failed to convert to UTF-8 stream from the data source data string", e);
			}
			finally
			{
				closeStream(bufReader);
				closeStream(inReader);
				closeStream(in);
			}
		}

		if (data.getFile() != null)
		{
			try
			{
				File file = new File(data.getFile());
				in = new FileInputStream(file);
				inReader = data.getFileEncoding() == null ? new InputStreamReader(in) : new InputStreamReader(in, data.getFileEncoding());
				bufReader = new BufferedReader(inReader);

				loadData(bufReader, data);
			}
			catch (UnsupportedEncodingException e)
			{
				throw new BuildException("Failed to convert to '" + data.getFileEncoding() + "' stream from the data source data file: " + data.getFile(), e);
			}
			catch (FileNotFoundException e)
			{
				throw new BuildException("Failed to find data source data file: " + data.getFile(), e);
			}
			finally
			{
				closeStream(bufReader);
				closeStream(inReader);
				closeStream(in);
			}
		}
	}

	private void loadData(BufferedReader reader, DataSourceData data)
	{
		String line;

		try
		{
			while ((line = reader.readLine()) != null)
			{
				if (line.trim().isEmpty())
				{
					continue;
				}

				String[] values = line.split(data.getSeparator(), -1);
				if (values.length < getMaxColumnPosition())
				{
					throw new BuildException("String provided from data source collection doesn't conform to columns definitions: " + line);
				}

				if (data.trimValues())
				{
					for (int i = 0; i < values.length; i++)
					{
						values[i] = values[i].trim();
					}
				}

				loadData(values);
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to process line from data source provided data", e);
		}
	}

	private void loadData(String[] values)
	{
		Map<String, Object> dataRow = new HashMap<String, Object>();

		for (int i = 0; i < columns.size(); i++)
		{
			DataSourceColumn column = columns.get(i);
			int position = column.getPosition() == -1 ? i : column.getPosition();
			String _value = position < values.length ? values[position] : null;
			Object value;

			try
			{
				value = column.parse(_value);
			}
			catch (IllegalArgumentException e)
			{
				throw new BuildException("Failed to parse cell value of a DataSource" , e);
			}

			dataRow.put(column.getName(), value);
		}

		this.add(dataRow);
	}

	private void closeStream(Closeable stream)
	{
		if (stream == null)
		{
			return;
		}

		try
		{
			stream.close();
		}
		catch (Throwable e) {}
	}

	private int getMaxColumnPosition()
	{
		if (maxColumnPosition != -1)
		{
			return maxColumnPosition;
		}

		maxColumnPosition = columns.size() - 1;

		for (DataSourceColumn column : columns)
		{
			if (maxColumnPosition < column.getPosition())
			{
				maxColumnPosition = column.getPosition();
			}
		}

		return maxColumnPosition;
	}
}
