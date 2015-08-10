package com.anttoolkit.general.tasks.build;

import java.util.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.*;
import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.tasks.build.util.*;

public class BuildTask
		extends GenericTask
		implements TaskContainer
{
	//Specifies list of builds that should not be installed
	public static final String IGNORE_BUILDS_CONTENT = "anttoolkit.ignore.builds.content";

	//Limits list of build that should be installed. Syntax: [[qualifier #1],]1.2,12.6,14.36,[[qualifier #2],]6.7,7.23,11.58
	public static final String BUILDS_TO_INSTALL = "anttoolkit.builds.install";

	private static Map<Project, Map<String, Set<BuildVersion>>> buildsToInstall = new HashMap<Project, Map<String, Set<BuildVersion>>>();

	private BuildVersion version;
	private String comment;
	private String qualifier;
	private List<Task> tasks = new LinkedList<Task>();

	public void setNumber(String version)
	{
		this.version = BuildVersion.parse(version);
	}

	public void setComment(String comment)
	{
		this.comment = comment;
	}

	public void setQualifier(String qualifier)
	{
		this.qualifier = qualifier;
	}

	@Override
	public void addTask(Task task)
	{
		if (task == null)
		{
			return;
		}

		tasks.add(task);
	}

	@Override
	protected void preProcessing()
	{
		if (qualifier != null)
		{
			BuildContextManager.setContextQualifier(qualifier.trim());
		}
	}

	@Override
	protected void postProcessing()
	{
		if (qualifier != null)
		{
			BuildContextManager.releaseContextQualifier();
		}
	}

	@Override
	public void doWork() throws BuildException
	{
		IBuildVersionProvider provider = BuildContextManager.getVersionProvider(this);
		if (provider == null)
		{
			throw new BuildException("No version provider registered");
		}

		if (version == null)
		{
			throw new BuildException("Build version number should be specified");
		}

		if (isBuildInstallationForbidden(version))
		{
			return;
		}

		if (!provider.canUpdateToVersion(version, BuildContextManager.getContextQualifier()))
		{
			return;
		}

		BuildVersion currentVersion = provider.getCurrentVersion(BuildContextManager.getContextQualifier());

		boolean outOfTurnUpdate = version.compareTo(currentVersion) < 0;

		String versionTitle = BuildContextManager.getContextQualifier() == null ||
				BuildContextManager.getContextQualifier().trim().length() == 0 ?
				version.toString() :
				version.toString() + " [" + BuildContextManager.getContextQualifier() + "]";

		if (outOfTurnUpdate)
		{
			versionTitle = versionTitle + " <<OUT OF TURN UPDATE>>";
		}

		String ignoreContent = getProject().getProperty(IGNORE_BUILDS_CONTENT);
		boolean ignore = ignoreContent != null && BooleanHelper.getBoolean(ignoreContent);

		this.log("Updating system from version " + currentVersion + " to " + versionTitle + (ignore ? " <<BUILD IGNORED>>" : ""));

		long startTime = System.currentTimeMillis();

		if (!ignore)
		{
			for (Task task : tasks)
			{
				task.perform();
			}
		}
		else
		{
			comment = "IGNORED";
		}

		provider.updateToVersion(currentVersion, version, comment, BuildContextManager.getContextQualifier());

		long duration = (System.currentTimeMillis() - startTime) / 1000;

		this.log("System was updated from version " + currentVersion + " to " + versionTitle + " [" + duration + " secs]");
	}

	private Map<String, Set<BuildVersion>> getBuildsToInstall()
	{
		Map<String, Set<BuildVersion>> builds;

		synchronized (buildsToInstall)
		{
			builds = buildsToInstall.get(getProject());
			if (builds != null)
			{
				return builds;
			}

			builds = parseBuildsToInstall(getProject().getProperty(BUILDS_TO_INSTALL));
			buildsToInstall.put(getProject(), builds);

			return builds;
		}
	}

	private Map<String, Set<BuildVersion>> parseBuildsToInstall(String builds)
	{
		if (builds == null || builds.trim().length() == 0)
		{
			return null;
		}

		Map<String, Set<BuildVersion>> buildsMap = new HashMap<String, Set<BuildVersion>>();

		String[] buildParts = builds.split(",", -1);

		String qualifier = "";

		for (String buildVersion : buildParts)
		{
			buildVersion = buildVersion.trim();

			//qualifier specified
			if (buildVersion.startsWith("[") && buildVersion.endsWith("]"))
			{
				qualifier = buildVersion.substring(1, buildVersion.length() - 1).trim();
				continue;
			}

			Set<BuildVersion> buildsSet = buildsMap.get(qualifier);
			if (buildsSet == null)
			{
				buildsSet = new HashSet<BuildVersion>();
				buildsMap.put(qualifier, buildsSet);
			}

			buildsSet.add(BuildVersion.parse(buildVersion));
		}

		return buildsMap;
	}

	private boolean isBuildInstallationForbidden(BuildVersion buildVersion)
	{
		Map<String, Set<BuildVersion>> builds = getBuildsToInstall();
		if (builds == null)
		{
			return false;
		}

		String qualifier = BuildContextManager.getContextQualifier();

		Set<BuildVersion> buildVersions = builds.get(qualifier);

		return buildVersions == null || !buildVersions.contains(buildVersion);
	}
}
