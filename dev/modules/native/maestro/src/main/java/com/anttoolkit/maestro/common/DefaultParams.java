package com.anttoolkit.maestro.common;

import java.lang.annotation.*;

@Inherited
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface DefaultParams
{
	public String[] params();
	public String[] values();
}
