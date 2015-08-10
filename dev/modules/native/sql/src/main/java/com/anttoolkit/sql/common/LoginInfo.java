package com.anttoolkit.sql.common;

import java.sql.*;
import java.util.*;

public class LoginInfo
{
	public final String user;
	public final String password;
	public final String url;
	public final Driver driver;
	public final Properties connectionProperties;

	public LoginInfo(String url, String user, String password, Driver driver, Properties connectionProperties)
	{
		this.url = url;
		this.user = user;
		this.password = password;
		this.driver = driver;

		Properties props = connectionProperties;
		if (props == null)
		{
			props = new Properties();
		}

		props.put("user", user);
		props.put("password", password);

		if (!props.containsKey("autocommit"))
		{
			props.put("autocommit", "false");
		}

		this.connectionProperties = props;
	}

	public String toString()
	{
		return url + "^" + user + "^" + password + "^" + driver;
	}
}
