package com.anttoolkit.general.entities;

public interface IEntityProcessor<T, P, R>
{
	public R processEntity(T entity, P param);

	public boolean readOnly();
}
