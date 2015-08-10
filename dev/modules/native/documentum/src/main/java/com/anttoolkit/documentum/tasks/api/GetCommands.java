package com.anttoolkit.documentum.tasks.api;

import java.util.*;

public class GetCommands
{
	public static final Set<String> COMMANDS;

	static
	{
		Set<String> commands = new HashSet<String>();

		commands.add("adddigsignature");
		commands.add("addesignature");
		commands.add("addpackage");
		commands.add("appendpart");
		commands.add("appendstate");
		commands.add("apply");
		commands.add("archive");
		commands.add("branch");
		commands.add("cachequery");
		commands.add("checkin");
		commands.add("checkinapp");
		commands.add("checkout");
		commands.add("connect");
		commands.add("count");
		commands.add("create");
		commands.add("createaudit");
		commands.add("datatype");
		commands.add("dereference");
		commands.add("describe");
		commands.add("dump");
		commands.add("dumpconnection");
		commands.add("dumploginticket");
		commands.add("encryptpass");
		commands.add("get");
		commands.add("getconnection");
		commands.add("getcontent");
		commands.add("getdocbasemap");
		commands.add("getdocbrokermap");
		commands.add("getevents");
		commands.add("getfile");
		commands.add("getlastcoll");
		commands.add("getlogin");
		commands.add("getmessage");
		commands.add("getpath");
		commands.add("getservermap");
		commands.add("id");
		commands.add("insertpart");
		commands.add("insertstate");
		commands.add("iscached");
		commands.add("listconnection");
		commands.add("listmessage");
		commands.add("locate");
		commands.add("lpq");
		commands.add("offset");
		commands.add("print");
		commands.add("query_cmd");
		commands.add("query");
		commands.add("queue");
		commands.add("readquery");
		commands.add("repeat");
		commands.add("repeating");
		commands.add("resolvealias");
		commands.add("restore");
		commands.add("retrieve");
		commands.add("saveasnew");
		commands.add("type");
		commands.add("values");
		commands.add("vdmpath");
		commands.add("vdmpathdql");

		COMMANDS = commands;
	}
}
