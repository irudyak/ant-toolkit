package com.anttoolkit.sql.tasks.build;

import com.anttoolkit.general.tasks.build.*;
import com.anttoolkit.sql.tasks.build.util.*;

public class SqlBuildVersionProviderTask extends BuildVersionProviderTask
{
	@Override
	protected String getVersionProviderClass()
	{
		return VersionProviderProxy.class.getName();
	}
}
