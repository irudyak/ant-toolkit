--------------------------------------------------------
Introduction:
--------------------------------------------------------

ant-toolkit is Apache Ant based scripting framework, which from the high level perspective could be divided to three logical peaces:

1) Plenty of custom Ant tasks/types/conditions/loggers which allow you to script lots of routine tasks and provide integration with lots of third party systems like: SQL databases, version control systems, EMC Documentum system, Apache Hadoop, Pig, Hive, HBase, ZooKeeper and so on...

2) Third party Java libraries (jars). Such libraries are needed cause lots of custom tasks depends on them (for example Hadoop related Ant tasks depends on Hadoop Core, MapReduce, Hdfs and Yarn jars and so on).

3) Scripts infrastructure, which allows you to simply reuse (by using one Ant import statement) custom tasks/types/conditions and system scripts/macroses which provides lots of features. Here are some of them:

	a) Profiles support - you can have several profiles, storing custom settings for different environments. So you can use such settings in your Ant scripts code. While executing Ant script you can simply specify profile name and all custom settings will be handled from the particular profile.
	
	b) Smart logging - if you executing the same script several times or if you want to execute in parallel two instances of the same script by providing them different profiles (for example to deploy you application to several environments) this option can be very useful for you. For each script execution session it creates directory logs-[profile-name]/[current date-time] where all Ant logs will be stored. So it eliminates the problem of logs rewriting while executing the same script several times. In a such manner all your logs will be structured by the profile and the time when particular script was executed.
	
	c) Support for different kind of loops, if-else, try-catch and other constructs.
	
	d) Incremental updates and changes tracking of particular environment.
	
	e) Re-execution of script from the point of its previous failure.
	
	f) Multithreading support - you can execute several tasks in a separate thread and further in a some point of your Ant script you can have a synchronization point for the threads.
	
	g) Mutable and scope specific Ant properties - you can change values of Ant properties (by using them like variables in programming languages). There is also supported a concept of properties scope - for example like global and local function variables in programming languages.
	
	h) Complex orchestrations - allows you to build some kind of workflow (you can also add steps dynamically in the runtime, depending on some custom condition) and execute it tracking current status. It also supports re-execution of orchestration from the point of its previous failure.
	
	i) Custom Ant tags/types/conditions to work with third party systems like: EMC Documentum, Hadoop, Pig, Hive, HBase, ZooKeeper, Jasper Reports and etc...
	
	j) and etc... (see the wiki pages) 
	
--------------------------------------------------------
Prerequisites:
--------------------------------------------------------

1) Java 6.0 or newer

2) Apache Ant 1.9.3 or newer. ANT_HOME variable should be set if you plan to execute scripts from the OS shell.

--------------------------------------------------------
Building framework distribution:
--------------------------------------------------------

To build and package the framework distribution just simply:
1) Change current directory to "dev"
2) Execute maven script "mvn clean package"

Before packaging your framework distribution, you can also specify what modules to include in it by editing dev/framework/modules file. 

If you want to package distribution which includes all available modules you can just simply remove dev/framework/modules file.