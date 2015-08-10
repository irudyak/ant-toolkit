package com.anttoolkit.jasperreports.report;

import java.util.*;

public interface IDataSourceProcessor <R>
{
	public R processDataSource(Collection<?> coll);
}
