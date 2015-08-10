package com.anttoolkit.documentum.tasks.build;

import com.anttoolkit.documentum.tasks.build.util.*;
import com.anttoolkit.general.tasks.build.*;

public class DocbaseVersionProviderTask
		extends BuildVersionProviderTask
{
	@Override
	protected String getVersionProviderClass()
	{
		return DocbaseVersionProvider.class.getName();
	}
}
