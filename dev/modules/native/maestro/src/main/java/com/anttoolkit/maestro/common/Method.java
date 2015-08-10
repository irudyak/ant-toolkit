package com.anttoolkit.maestro.common;

import java.lang.annotation.*;

@Inherited
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Method
{
	public String value() default "POST";
}
