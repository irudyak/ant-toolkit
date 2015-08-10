package com.anttoolkit.hbase.tasks.data.util.scan.filter;

import java.util.*;

import org.apache.hadoop.hbase.filter.*;
import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class TimestampsFilterConfig implements IFilterConfig
{
	private String timestamp;
	private String array;

	public void setTimestamp(String timestamp)
	{
		this.timestamp = timestamp;
	}

	public void addText(String timestamp)
	{
		this.timestamp = timestamp;
	}

	public void setTimestampArray(String array)
	{
		this.array = array;
	}

	@Override
	public Filter getFilter(GenericTask task)
	{
		validate(task);
		return new TimestampsFilter(getTimestamps());
	}

	@Override
	public void validate(GenericTask task)
	{
		if (timestamp == null && array == null)
		{
			throw new BuildException("Timestamp or timestamps array should be specified");
		}

		if (array != null && !ArrayManager.exists(array))
		{
			throw new BuildException("Specified array '" + array + "' doesn't exist");
		}
	}

	private List<Long> getTimestamps()
	{
		List<Long> list = new LinkedList<Long>();

		if (timestamp != null)
		{
			String[] chunks = timestamp.trim().split(",", -1);
			for (String ts : chunks)
			{
				try
				{
					list.add(Long.parseLong(ts));
				}
				catch (NumberFormatException e)
				{
					throw new BuildException("Incorrect timestamp '" + ts + "' specified");
				}
			}
		}

		if (array != null)
		{
			int size = ArrayManager.size(array);
			for (int i = 0; i < size; i++)
			{
				String ts = ArrayManager.get(array, i);

				try
				{
					list.add(Long.parseLong(ts));
				}
				catch (NumberFormatException e)
				{
					throw new BuildException("Incorrect timestamp '" + ts + "' exists in array '" + array + "'");
				}
			}
		}

		return list;
	}
}
