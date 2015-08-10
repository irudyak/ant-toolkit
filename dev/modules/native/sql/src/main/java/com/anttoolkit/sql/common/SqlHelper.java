package com.anttoolkit.sql.common;

import java.io.*;
import java.sql.*;
import java.text.*;
import java.util.*;

import org.apache.tools.ant.*;
import org.apache.tools.ant.util.FileUtils;

import com.anttoolkit.sql.tasks.*;

public class SqlHelper
{
	public static void closeResultSet(ResultSet resultSet)
	{
		if (resultSet == null)
		{
			return;
		}

		try
		{
			if (resultSet.getStatement() != null)
			{
				closeStatement(resultSet.getStatement());
			}

			resultSet.close();
		}
		catch (SQLException e) {}
	}

	public static ResultSet executeQuery(SqlSession session, String query)
	{
		return executeQuery(session, query, (Task)null);
	}

	public static ResultSet executeQuery(SqlSession session, String query, Task logger)
	{
		return executeQuery(session, query, null, logger);
	}

	public static ResultSet executeQuery(SqlSession session, String query, String delimiter)
	{
		return executeQuery(session, query, delimiter, null);
	}

	public static ResultSet executeQuery(SqlSession session, String query, String delimiter, Task logger)
	{
		Statement statement = null;

		try
		{
			String _query = query.trim();

			if (delimiter != null && delimiter.trim().length() != 0 &&
				_query.lastIndexOf(delimiter) == _query.length() - 1)
			{
				_query = _query.substring(0, _query.length() - 1);
			}

			if (logger != null)
			{
				logger.log("Executing SQL query: " + _query);
			}

			if (isStoredProcedureCall(_query))
			{
				statement = session.getConnection().prepareCall(_query);
				return ((CallableStatement)statement).executeQuery();
			}
			else
			{
				statement = session.getConnection().createStatement();
				return statement.executeQuery(_query.trim());
			}
		}
		catch (SQLException e)
		{
			throw new BuildException("Failed to execute SQL query: " + query, e);
		}
	}

	public static void executeUpdateStatements(SqlSession session, Reader reader, Project project,
											   boolean keepformat, String delimiter,
											   String delimiterType)
	{
		executeUpdateStatements(session, reader, project, keepformat, delimiter, delimiterType, null);
	}

	public static void executeUpdateStatements(SqlSession session, Reader reader, Project project,
											   boolean keepformat, String delimiter,
											   String delimiterType, Task logger)
	{
		StringBuffer sql = new StringBuffer();
		String line;

  		BufferedReader in = new BufferedReader(reader);

		try
		{
			while ((line = in.readLine()) != null)
			{
				if (!keepformat)
				{
					line = line.trim();
				}

				line = project.replaceProperties(line);

				if (!keepformat)
				{
					if (line.startsWith("//") || line.startsWith("--"))
					{
						continue;
					}

					StringTokenizer st = new StringTokenizer(line);
					if (st.hasMoreTokens())
					{
						String token = st.nextToken();
						if ("REM".equalsIgnoreCase(token))
						{
							continue;
						}
					}
				}

				sql.append(keepformat ? "\n" : " ").append(line);

				if (!keepformat && line.indexOf("--") >= 0)
				{
					sql.append("\n");
				}

				if (delimiter != null)
				{
					int lastDelimPos = lastDelimiterPosition(sql, line, delimiter, delimiterType);
					if (lastDelimPos > -1)
					{
						executeUpdate(session, sql.substring(0, lastDelimPos), logger);
						sql.replace(0, sql.length(), "");
					}
				}
			}
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to read SQL query from resource", e);
		}
		finally
		{
			FileUtils.close(in);
		}

		// Catch any statements not followed by ;
		if (sql.length() > 0)
		{
			executeUpdate(session, sql.toString(), logger);
		}
	}

	public static boolean exist(SqlSession session, String query)
	{
		return exist(session, query, (Task)null);
	}

	public static boolean exist(SqlSession session, String query, Task logger)
	{
		return exist(session, query, null, logger);
	}

	public static boolean exist(SqlSession session, String query, String delimiter)
	{
		return exist(session, query, delimiter, null);
	}

	public static boolean exist(SqlSession session, String query, String delimiter, Task logger)
	{
		ResultSet resultSet = null;

		try
		{
			resultSet = executeQuery(session, query, delimiter, logger);
			return resultSet.next();
		}
		catch (SQLException e)
		{
			throw new BuildException("Exception occured while trying to iterate over result set");
		}
		finally
		{
			closeResultSet(resultSet);
		}
	}

	public static String getStringParamFromFirstString(SqlSession session, String query, int columnIndex, String dateFormat, String delimiter)
	{
		return getStringParamFromFirstString(session, query, columnIndex, dateFormat, delimiter, null);
	}

	public static String getStringParamFromFirstString(SqlSession session, String query, int columnIndex, String dateFormat, String delimiter, Task logger)
	{
		ResultSet resultSet = null;

		try
		{
			resultSet = executeQuery(session, query, delimiter, logger);
			if (!resultSet.next())
			{
				throw new BuildException("No results were returned by the query: " + query);
			}

			return getColumnValue(resultSet, columnIndex, dateFormat);
		}
		catch (SQLException e)
		{
			throw new BuildException("Exception occured while trying to iterate over result set", e);
		}
		finally
		{
			closeResultSet(resultSet);
		}
	}

	public static List<String> getColumnNames(ResultSet resultSet)
	{
		try
		{
			if (resultSet == null || resultSet.getMetaData() == null)
			{
				return null;
			}

			List<String> columnNames = new LinkedList<String>();

			ResultSetMetaData metadata = resultSet.getMetaData();
			int count = metadata.getColumnCount();

			for (int i = 1 ; i <= count; i++)
			{
				String name = metadata.getColumnName(i);
				columnNames.add(name);
			}

			return columnNames;
		}
		catch (SQLException e)
		{
			throw new BuildException("Failed to get result set metadata", e);
		}
	}

	public static String getColumnValue(ResultSet resultSet, String columnName)
	{
		return getColumnValue(resultSet, columnName, null);
	}

	public static String getColumnValue(ResultSet resultSet, String columnName, String dateFormat)
	{
		List<String> names = getColumnNames(resultSet);
		int index = names.indexOf(columnName);
		return getColumnValue(resultSet, index + 1, dateFormat);
	}

	public static String getColumnValue(ResultSet resultSet, int columnIndex)
	{
		return getColumnValue(resultSet, columnIndex, null);
	}

	public static String getColumnValue(ResultSet resultSet, int columnIndex, String dateFormat)
	{
		SimpleDateFormat formatter = dateFormat == null ?
				new SimpleDateFormat("MM/dd/yyyy hh:mm:ss") :
				new SimpleDateFormat(dateFormat);

		try
		{
			ResultSetMetaData metadata = resultSet.getMetaData();
			int type = metadata.getColumnType(columnIndex);

			switch (type)
			{
				case Types.BOOLEAN:
					return Boolean.toString(resultSet.getBoolean(columnIndex));
				case Types.CHAR:
					return resultSet.getString(columnIndex);
				case Types.DATE:
					java.sql.Date date = resultSet.getDate(columnIndex);
					return date == null ? " " : formatter.format(date);
				case Types.DOUBLE:
					return Double.toString(resultSet.getDouble(columnIndex));
				case Types.FLOAT:
					return Float.toString(resultSet.getFloat(columnIndex));
				case Types.INTEGER:
					return Integer.toString(resultSet.getInt(columnIndex));
				case Types.NUMERIC:
					return Long.toString(resultSet.getLong(columnIndex));
				case Types.BIGINT:
					return Long.toString(resultSet.getLong(columnIndex));
				case Types.SMALLINT:
					return Short.toString(resultSet.getShort(columnIndex));
				case Types.TIME:
					java.sql.Time time = resultSet.getTime(columnIndex);
					return time == null ? " " : formatter.format(time);
				case Types.TIMESTAMP:
					java.sql.Timestamp tstamp = resultSet.getTimestamp(columnIndex);
					return tstamp == null ? " " : formatter.format(tstamp);
				case Types.TINYINT:
					return Integer.toString(resultSet.getInt(columnIndex));
				case Types.VARCHAR:
					return resultSet.getString(columnIndex);
			}

			throw new BuildException("Unrecognized column type '" + type + "' for column number " + columnIndex);
		}
		catch (SQLException e)
		{
			throw new BuildException("Failed to get value for query column " + columnIndex, e);
		}
	}

	public static int executeUpdate(SqlSession session, String query)
	{
		return executeUpdate(session, query, null);
	}

	public static int executeUpdate(SqlSession session, String query, Task logger)
	{
		if (query == null || query.trim().length() == 0)
		{
			return 0;
		}

		Statement statement = null;

		if (logger != null)
		{
			logger.log("Executing SQL query: " + query);
		}

		try
		{
			if (isStoredProcedureCall(query))
			{
				statement = session.getConnection().prepareCall(query.trim());
				return ((CallableStatement)statement).executeUpdate();
			}
			else
			{
				statement = session.getConnection().createStatement();
				return statement.executeUpdate(query.trim());
			}
		}
		catch (SQLException e)
		{
			throw new BuildException("Failed to execute SQL query: " + query, e);
		}
		finally
		{
			closeStatement(statement);
		}
	}

	public static void closeStatement(Statement statement)
	{
		try
		{
			if (statement != null)
			{
				statement.close();
			}
		}
		catch (SQLException e) {}
	}

	private static int lastDelimiterPosition(StringBuffer buf, String currentLine, String delimiter, String delimiterType)
	{
		String _delimiter = delimiter.trim().toLowerCase(Locale.ENGLISH);

		if (!delimiterType.equals(SqlUpdateQueryTask.DelimiterType.NORMAL))
		{
			return currentLine.trim().toLowerCase(Locale.ENGLISH).equals(_delimiter) ?
					buf.length() - currentLine.length() : -1;
		}

		int endIndex = delimiter.length() - 1;
		int bufferIndex = buf.length() - 1;

		while (bufferIndex >= 0 && Character.isWhitespace(buf.charAt(bufferIndex)))
		{
			--bufferIndex;
		}

		if (bufferIndex < endIndex)
		{
			return -1;
		}

		while (endIndex >= 0)
		{
			if (buf.substring(bufferIndex, bufferIndex + 1).toLowerCase(Locale.ENGLISH).charAt(0) != _delimiter.charAt(endIndex))
			{
				return -1;
			}

			bufferIndex--;
			endIndex--;
		}

		return bufferIndex + 1;
	}

	private static boolean isStoredProcedureCall(String query)
	{
		String _query = query.trim().toLowerCase();

		StringTokenizer st = new StringTokenizer(_query);

		if (!st.hasMoreTokens())
		{
			return false;
		}

		String firstToken = st.nextToken();

		return "exec".equals(firstToken) || "execute".equals(firstToken) || "call".equals(firstToken);
	}
}
