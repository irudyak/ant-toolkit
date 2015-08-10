package com.anttoolkit.hbase.tasks.build;

import com.anttoolkit.general.tasks.build.*;
import com.anttoolkit.hbase.tasks.build.util.*;

public class HbaseBuildVersionProviderTask extends BuildVersionProviderTask
{
	@Override
	protected String getVersionProviderClass()
	{
		return HBaseVersionProvider.class.getName();
	}
}
