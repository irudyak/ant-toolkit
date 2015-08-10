package com.anttoolkit.system.tools.info;

import java.io.*;
import java.util.*;

public class FrameworkInfo
{
	private static final String VERSION_INFO_RESOURCE = "com/anttoolkit/system/version.properties";
	private static final String MODULES_INFO_RESOURCE = "com/anttoolkit/system/modules.xml";

	private static final String VERSION_PROPERTY = "version";

	public static void main(String[] args)
	{
		printHeader();
		printUsage();
	}

	private static String getVersion()
	{
		Properties props = new Properties();
		InputStream in = null;

		try
		{
			ClassLoader cL = FrameworkInfo.class.getClassLoader();

			in = cL != null ?
					cL.getResourceAsStream(VERSION_INFO_RESOURCE) :
					ClassLoader.getSystemResourceAsStream(VERSION_INFO_RESOURCE);


			if (in == null)
			{
				throw new RuntimeException("Unable to find version info resource: " + VERSION_INFO_RESOURCE);
			}

			props.load(in);

			return props.getProperty(VERSION_PROPERTY);
		}
		catch (IOException e)
		{
			throw new RuntimeException("Failed to load version info resource: " + VERSION_INFO_RESOURCE, e);
		}
		finally
		{
			if (in != null)
			{
				try
				{
					in.close();
				}
				catch (IOException e) { }
			}
		}
	}

	private static void printHeader()
	{
		System.out.println("");
		System.out.println("AntToolkit " + getVersion() + " - command line utility wrapper and custom tasks for Apache Ant.");
	}

	private static void printUsage()
	{
		System.out.println("");
		System.out.println("Usage: anttoolkit [options] [Apache Ant command line arguments]");
		System.out.println("");
		System.out.println("Options:");
		System.out.println("  -help, -h               Prints this message.");
		System.out.println("");
		System.out.println("  -profile, -pr           Profile to use. All the script setting will be loaded from");
		System.out.println("                          specified profile, by adding [profilesDir]/[profile] to");
		System.out.println("                          script execution CLASSPATH.");
		System.out.println("");
		System.out.println("  -profilesDir <dir>      Specifies directory storing profiles.");
		System.out.println("    -pd        <dir>");
		System.out.println("");
		System.out.println("  -logFile <file>         Log file to use. In case of [smartLogging] feature enabled, only");
		System.out.println("    -l     <file>         file name (without absolute/relative path) should be specified.");
		System.out.println("");
		System.out.println("  -smartLogging, -sl      Boolean flag to enable smart logging feature. For this feature to");
		System.out.println("                          work you should also specify [logsRoot] parameter. New directory");
		System.out.println("                          [logsRoot]/YYYY/MM/DD/hh-mm-ss-SSS to store script execution log file");
		System.out.println("                          will be created for each script execution attempt. Thus log files for");
		System.out.println("                          each script execution attempt will be stored in a separate");
		System.out.println("                          directories like: [logsRoot]/YYYY/MM/DD/hh-mm-ss-SSS/[logFile]");
		System.out.println("");
		System.out.println("  -logsRoot <dir>         Specifies ROOT directory where to store log files for the script.");
		System.out.println("    -lr     <dir>         Only makes sence if [logFile] parameter is also provided in a form");
		System.out.println("                          that doesn't contain absolute/relative path specification.");
		System.out.println("");
		System.out.println("  -threadsLogging, -tl    Boolean flag to enable threads logging feature. The feature is only");
		System.out.println("                          makes sence if you use multithreading. If this feature is turned on,");
		System.out.println("                          it will prefix all the log messages with the thread name. In addition");
		System.out.println("                          to this, it will be created log file for each thread, to store");
		System.out.println("                          messages produced only by particular thread.");
		System.out.println("");
		System.out.println("  -update                 Updates classpath for AntToolkit jars from lib folder");
		System.out.println("                          in \"anttoolkit-all.jar\" manifest.");
		System.out.println("");
	}
}
