package com.anttoolkit.general.tasks.build;

import com.anttoolkit.general.tasks.build.csv.*;

public class CsvVersionProviderTask extends BuildVersionProviderTask
{
	@Override
	protected String getVersionProviderClass()
	{
		return CsvVersionProvider.class.getName();
	}
}
