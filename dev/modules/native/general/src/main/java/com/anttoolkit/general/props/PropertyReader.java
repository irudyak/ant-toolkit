package com.anttoolkit.general.props;

import com.anttoolkit.general.entities.*;

public class PropertyReader implements IEntityProcessor<String, Void, String>
{
	@Override
	public String processEntity(String entity, Void param)
	{
		return entity;
	}

	@Override
	public boolean readOnly()
	{
		return true;
	}
}
