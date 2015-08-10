package com.anttoolkit.general.tasks.build.csv;

import com.anttoolkit.general.tasks.build.util.*;

public class VersionRecord implements Comparable
{
	public final BuildVersion version;
	public final String qualifier;
	public final String timestamp;
	public final String comment;
	public final String user;
	public final String host;
	public final String ip;

	public static boolean isVersionRecord(String text)
	{
		return text.split("\\|", -1).length == 7;
	}

	public VersionRecord(String text)
	{
		String[] chunks = text.split("\\|", -1);

		this.qualifier = chunks[0].trim();
		this.version = BuildVersion.parse(chunks[1]);
		this.timestamp = chunks[2];
		this.user = chunks[3].trim();
		this.host = chunks[4].trim();
		this.ip = chunks[5].trim();
		this.comment = chunks[6].replace("\r\n", " ").replace("\n", " ").trim();
	}

	public VersionRecord(BuildVersion version, String qualifier, String timestamp, String comment, String user, String host, String ip)
	{
		this.qualifier = qualifier == null ? "" : qualifier.trim();
		this.version = version;
		this.timestamp = timestamp == null ? "" : timestamp.trim();
		this.user = user == null ? "" : user.trim();
		this.host = host == null ? "" : host.trim();
		this.ip = ip == null ? "" : ip.trim();
		this.comment = comment == null ? "" : comment.replace("\r\n", " ").replace("\n", " ").trim();
	}

	@Override
	public int compareTo(Object obj)
	{
		if (obj == null)
		{
			throw new IllegalArgumentException("Can't compare csv build version record to null object");
		}

		if (!(obj instanceof VersionRecord))
		{
			throw new IllegalArgumentException("Can't compare csv build version record to not csv build version record object");
		}

		return qualifier.compareTo(((VersionRecord)obj).qualifier);
	}

	@Override
	public String toString()
	{
		return qualifier + "|" + version.toString() + "|" + timestamp + "|" + user + "|" + host + "|" + ip + "|" + comment;
	}
}
