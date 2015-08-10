package com.anttoolkit.zookeeper.tasks.util;

import java.util.*;

import org.apache.zookeeper.data.*;

public class CyclicBarrier
{
	public final String rootNode;
	public final String name;
	public final int parties;
	public final List<ACL> acls;
	public final String barrierNode;
	public final String readyNode;

	public CyclicBarrier(String rootNode, String name, int parties, List<ACL> acls)
	{
		this.rootNode = rootNode;
		this.name = name;
		this.parties = parties;
		this.acls = acls;
		this.barrierNode = rootNode.endsWith("/") ? rootNode + name : rootNode + "/" + name;
		this.readyNode = barrierNode + "/ready";
	}
}
