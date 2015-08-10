package com.anttoolkit.sql.common;

import org.apache.tools.ant.*;

import java.sql.*;
import java.util.Enumeration;

public class SqlSession
{
	private LoginInfo loginInfo;
	private Connection connection;
	private boolean hasActiveTransaction = false;

	SqlSession(LoginInfo loginInfo)
	{
		this.loginInfo = loginInfo;
	}

	public Connection getConnection()
	{
		if (connection != null)
		{
			return connection;
		}

		try
		{
			if (!isDriverAlreadyRegistered(loginInfo.driver))
			{
				DriverManager.registerDriver(loginInfo.driver);
			}

			connection = DriverManager.getConnection(loginInfo.url, loginInfo.connectionProperties);
			connection.setAutoCommit(true);
			return connection;
		}
		catch (SQLException e)
		{
			throw new BuildException("Failed to connect to URL: " + loginInfo.url, e);
		}
	}

	public boolean hasActiveTransaction()
	{
		return hasActiveTransaction;
	}

	public void beginTransaction(int isolationLevel)
	{
		if (hasActiveTransaction)
		{
			throw new BuildException("Nested transactions are not supported");
		}

		try
		{
			getConnection().setAutoCommit(false);

			if (getConnection().getTransactionIsolation() != isolationLevel)
			{
				getConnection().setTransactionIsolation(isolationLevel);
			}

			hasActiveTransaction = true;
		}
		catch (SQLException e)
		{
			throw new BuildException("Failed to set transaction isolation level to " + isolationLevel, e);
		}
	}

	public void commitTransaction()
	{
		if (connection == null || !hasActiveTransaction)
		{
			return;
		}

		try
		{
			connection.commit();
			connection.setAutoCommit(true);

			if (connection.getTransactionIsolation() != Connection.TRANSACTION_READ_COMMITTED)
			{
				connection.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
			}

			hasActiveTransaction = false;
		}
		catch (SQLException e)
		{
			throw new BuildException("Failed to commit transaction", e);
		}
	}

	public void rollbackTransaction()
	{
		if (connection == null || !hasActiveTransaction)
		{
			return;
		}

		try
		{
			connection.rollback();
			connection.setAutoCommit(true);
			hasActiveTransaction = false;
		}
		catch (SQLException e)
		{
			throw new BuildException("Failed to rollback transaction", e);
		}
	}

	public void closeConnection()
	{
		try
		{
			if (connection != null && !connection.isClosed())
			{
				connection.close();
			}

			connection = null;
		}
		catch (Throwable e) {}
	}

	public LoginInfo getLoginInfo()
	{
		return loginInfo;
	}

	private boolean isDriverAlreadyRegistered(Driver driver)
	{
		Enumeration<Driver> drivers = DriverManager.getDrivers();
		if (drivers == null || !drivers.hasMoreElements())
		{
			return false;
		}

		while (drivers.hasMoreElements())
		{
			Driver _driver = drivers.nextElement();
			if (_driver.equals(driver) || _driver.getClass().equals(driver.getClass()))
			{
				return true;
			}
		}

		return false;
	}
}
