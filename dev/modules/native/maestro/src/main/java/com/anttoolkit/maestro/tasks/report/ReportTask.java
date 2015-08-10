package com.anttoolkit.maestro.tasks.report;

import com.anttoolkit.maestro.common.*;
import com.anttoolkit.maestro.tasks.*;

@Action("report")
public abstract class ReportTask extends CliCommandTask
{
	@Param
	private Integer day;

	@Param
	private Integer month;

	@Param
	private Integer year;

	public void setDay(int day)
	{
		this.day = day;
	}

	public void setMonth(int month)
	{
		this.month = month;
	}

	public void setYear(int year)
	{
		this.year = year;
	}
}
