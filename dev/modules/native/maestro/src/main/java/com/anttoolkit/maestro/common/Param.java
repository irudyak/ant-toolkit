package com.anttoolkit.maestro.common;

import java.lang.annotation.*;

@Inherited
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Param
{
	public String value() default "";
	public boolean keyValueList() default false;
	public boolean sorted() default false;
}
