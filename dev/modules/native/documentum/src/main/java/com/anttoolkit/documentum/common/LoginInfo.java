package com.anttoolkit.documentum.common;

public class LoginInfo
{
	private String login = null;
	private String domain = null;
	private String password = null;
	private String docbase = null;

	private String stringPresentation = null;

	public LoginInfo(String login,
					 String domain,
					 String password,
					 String docbase)
	{
		this.login = login == null || login.trim().length() == 0 ? null : login.trim();
		this.domain = domain == null || domain.trim().length() == 0 ? null : domain.trim();
		this.password = password == null || password.trim().length() == 0 ? null : password.trim();
		this.docbase = docbase == null || docbase.trim().length() == 0 ? null : docbase.trim();

		StringBuffer buffer = new StringBuffer();
		buffer.append(this.login == null ? "" : this.login).append("\r\n");
		buffer.append(this.domain == null ? "" : this.domain).append("\r\n");
		buffer.append(this.password == null ? "" : this.password).append("\r\n");
		buffer.append(this.docbase == null ? "" : this.docbase);

		stringPresentation = buffer.toString();
	}

	public LoginInfo(String stringPresentation)
	{
		String[] parts = stringPresentation.split("\r\n", -1);
		if (parts.length != 4)
		{
			throw new IllegalArgumentException("Incorrect login info string specified");
		}

		login = parts[0];
		domain = parts[1];
		password = parts[2];
		docbase = parts[3];

		this.stringPresentation = stringPresentation;
	}

	public boolean isInvalid()
	{
		return login == null || password == null || docbase == null;
	}

	public String getLogin()
	{
		return login;
	}

	public String getDomain()
	{
		return domain;
	}

	public String getPassword()
	{
		return password;
	}

	public String getDocbase()
	{
		return docbase;
	}

	public boolean equals(Object obj)
	{
		return !(obj == null || !(obj instanceof LoginInfo)) &&
				obj.toString().equals(this.toString());
	}

	public String toString()
	{
		return stringPresentation;
	}
}
