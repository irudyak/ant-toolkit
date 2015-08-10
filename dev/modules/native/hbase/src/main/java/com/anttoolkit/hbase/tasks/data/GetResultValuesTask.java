package com.anttoolkit.hbase.tasks.data;

import java.util.*;

import org.apache.tools.ant.*;
import org.apache.hadoop.hbase.client.*;

import com.anttoolkit.hbase.tasks.*;
import com.anttoolkit.hbase.tasks.data.util.*;

public class GetResultValuesTask extends GenericHBaseTask
{
	private String ref;
	private List<CellToGet> cells = new LinkedList<CellToGet>();


	public void setResultRef(String reference)
	{
		this.ref = reference;
	}

	public void addConfiguredCell(CellToGet cell)
	{
		cells.add(cell);
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		Result result = (Result)this.getReference(ref);

		for (CellToGet cell : cells)
		{
			if (cell.processResult(result, this))
			{
				continue;
			}

			if (cell.getVersion() != null)
			{
				throw new BuildException("There are no column " + cell.getColumnFamily() + ":" + cell.getColumn() +
						" with version number " + cell.getVersion() +
						" in the GET operation result '" + ref + "': " + toString());
			}

			if (cell.getTimestamp() != null)
			{
				throw new BuildException("There are no column " + cell.getColumnFamily() + ":" + cell.getColumn() +
						" with timestamp " + cell.getTimestamp() +
						" in the GET operation result '" + ref + "': " + toString());
			}

			throw new BuildException("There are no column " + cell.getColumnFamily() + ":" + cell.getColumn() +
					" in the GET operation result '" + ref + "': " + toString());
		}
	}

	@Override
	protected void hadoopValidate()
	{
		if (ref == null)
		{
			throw new BuildException("Reference to a RESULT object wasn't specified");
		}

		if (cells.isEmpty())
		{
			throw new BuildException("There are no cells specified to get values from");
		}

		if (!checkCellsValid())
		{
			throw new BuildException("Invalid cells configuration to get values from GET operation result: " + toString());
		}
	}

	private boolean checkCellsValid()
	{
		for (CellToGet cell : cells)
		{
			String property = cell.getProperty();
			boolean hasProperty = property != null && !property.trim().isEmpty();

			String file = cell.getFile();
			boolean hasFile = file != null && !file.trim().isEmpty();

			if (!cell.isValid() || (hasProperty && hasFile) ||
				(!hasProperty && !hasFile))
			{
				return false;
			}
		}

		return true;
	}

	@Override
	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		builder.append("{reference=").append(ref == null ? "null" : ref);

		for (CellToGet cell : cells)
		{
			builder.append(", [").append(cell.toString()).append("]");
		}

		builder.append("}");

		return builder.toString();
	}
}
