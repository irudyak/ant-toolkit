package com.anttoolkit.general.tasks.profile;

public class ProfileResource
{
	private String name;
	private boolean required = true;

	public void setName(String name)
	{
		this.name = name;
	}

	public String name()
	{
		return name;
	}

	public void setRequired(boolean required)
	{
		this.required = required;
	}

	public boolean required()
	{
		return required;
	}
}
