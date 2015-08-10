package com.anttoolkit.documentum.tasks.api;

import java.util.*;

public class SetCommands
{
	public static final Set<String> COMMANDS;

	static
	{
		Set<String> commands = new HashSet<String>();

		commands.add("append");
		commands.add("appendcontent");
		commands.add("insert");
		commands.add("insertcontent");
		commands.add("set");
		commands.add("setcontent");

		COMMANDS = commands;
	}
}
