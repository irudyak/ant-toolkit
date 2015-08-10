package com.anttoolkit.system.tools.packaging;

public class ModuleInfo
{
	public final String name;
	public final String description;
	public final String version;
	public final String url;
	public final String taskdef;
	public final String taskdefJar;
	public final String[] libs;
	public final String[] dependencies;
	public final String descriptor;
	public final String baseDir;

	public ModuleInfo(String name, String description, String version, String url,
					  String taskdef, String taskdefJar, String[] libs, String[] dependencies, String descriptor)
	{
		this.name = name;
		this.description = description == null ? "" : description;
		this.version = version == null ? "" : version;
		this.url = url == null ? "" : url;
		this.taskdef = taskdef;
		this.taskdefJar = taskdefJar;
		this.libs = libs;
		this.dependencies = dependencies == null || dependencies.length == 0 ? null : dependencies;
		this.descriptor = descriptor.replace("\\", "/");

		if (this.descriptor.endsWith("/target/package/module.properties"))
		{
			baseDir = this.descriptor.replace("/target/package/module.properties", "");
		}
		else
		{
			baseDir = this.descriptor.replace("/module.properties", "");
		}
	}
}
