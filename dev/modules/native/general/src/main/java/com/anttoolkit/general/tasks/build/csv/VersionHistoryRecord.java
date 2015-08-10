package com.anttoolkit.general.tasks.build.csv;

import com.anttoolkit.general.tasks.build.util.*;

public class VersionHistoryRecord
{
	public final BuildVersion curVersion;
	public final BuildVersion newVersion;
	public final String qualifier;
	public final String timestamp;
	public final String comment;
	public final String user;
	public final String host;
	public final String ip;

	public static boolean isVersionHistoryRecord(String text)
	{
		return text.split("\\|", -1).length == 8;
	}

	public VersionHistoryRecord(String text)
	{
		String[] chunks = text.split("\\|", -1);

		this.qualifier = chunks[0].trim();
		this.curVersion = BuildVersion.parse(chunks[1]);
		this.newVersion = BuildVersion.parse(chunks[2]);
		this.timestamp = chunks[3];
		this.user = chunks[4].trim();
		this.host = chunks[5].trim();
		this.ip = chunks[6].trim();
		this.comment = chunks[7].replace("\r\n", " ").replace("\n", " ").trim();
	}

	public VersionHistoryRecord(BuildVersion curVersion, BuildVersion newVersion, String qualifier, String timestamp, String comment, String user, String host, String ip)
	{
		this.qualifier = qualifier == null ? "" : qualifier.trim();
		this.curVersion = curVersion;
		this.newVersion = newVersion;
		this.timestamp = timestamp == null ? "" : timestamp.trim();
		this.user = user == null ? "" : user.trim();
		this.host = host == null ? "" : host.trim();
		this.ip = ip == null ? "" : ip.trim();
		this.comment = comment == null ? "" : comment.replace("\r\n", " ").replace("\n", " ").trim();
	}

	@Override
	public String toString()
	{
		return qualifier + "|" + curVersion.toString() + "|" + newVersion.toString() + "|" + timestamp + "|" + user + "|" + host + "|" + ip + "|" + comment;
	}

}
