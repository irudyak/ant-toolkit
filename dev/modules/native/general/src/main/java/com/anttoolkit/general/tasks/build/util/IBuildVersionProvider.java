package com.anttoolkit.general.tasks.build.util;

import java.util.*;

import com.anttoolkit.general.tasks.*;

public interface IBuildVersionProvider
{
	public void init(GenericTask task, Map<String, String> properties);
	public void validate(GenericTask task);
	public BuildVersion getCurrentVersion(String qualifier);
	public boolean canUpdateToVersion(BuildVersion version, String qualifier);
	public void updateToVersion(BuildVersion currentVersion, BuildVersion newVersion, String comment, String qualifier);
}
