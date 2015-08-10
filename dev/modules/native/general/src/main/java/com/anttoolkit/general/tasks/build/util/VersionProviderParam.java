package com.anttoolkit.general.tasks.build.util;

import java.lang.annotation.*;

@Inherited
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface VersionProviderParam
{
	public String value();
}
