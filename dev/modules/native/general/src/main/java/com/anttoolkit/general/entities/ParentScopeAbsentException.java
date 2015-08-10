package com.anttoolkit.general.entities;

public class ParentScopeAbsentException extends RuntimeException
{
	public final int scopeLevel;
	public final int parentLevel;

	public ParentScopeAbsentException(int scopeLevel, int parentLevel, String msg)
	{
		super(msg);
		this.scopeLevel = scopeLevel;
		this.parentLevel = parentLevel;
	}
}
