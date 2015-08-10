echo off

REM Checks that Java accessible from command line
call :checkJavaVersion
if %errorLevel% NEQ 0 exit /b %errorLevel%

REM Checks that Apache Ant accessible from command line
call :checkApacheAnt
if %errorLevel% NEQ 0 exit /b %errorLevel%

REM Setting AntToolkit jars classpath
call :setClasspath %~dp0..\anttoolkit-all.jar
if %errorLevel% NEQ 0 exit /b %errorLevel%

REM Parse command line arguments
call :processArguments %*
if %errorLevel% NEQ 0 exit /b %errorLevel%

REM Perform validation
call :validateProcessedArgs
if %errorLevel% NEQ 0 exit /b %errorLevel%

REM Enhance AntToolkit classpath by adding profile dir
call :enhanceClasspath

REM Setting logger to use
call :setLogger

REM Creating logs directory if '-logsRoot' and '-logFile' specified
call :resolveLogFile
if %errorLevel% NEQ 0 exit /b %errorLevel%

REM Set additional environment options
if exist %~dp0setenv.bat (
	call "%~dp0setenv.bat"

	if %errorLevel% NEQ 0 (
		echo.
		echo --------------------------------------------------------------------------------
		echo [ERROR] Failed to set additional environment options
		echo --------------------------------------------------------------------------------
		echo.
		exit /b 1
	)
)

REM If JDK 1.7 is used we need to specify "-UseSplitVerifier" flag to overcome problem with StackMapTable
if "%ANTTOOLKIT_JAVA_VERSION%"=="1.7" (
	set "ANT_OPTS=-noverify -XX:-UseSplitVerifier %ANT_OPTS%"
)

REM If JDK 1.8 is used we need to specify "-noverify" flag to overcome problem with StackMapTable
if "%ANTTOOLKIT_JAVA_VERSION%"=="1.8" (
	set "ANT_OPTS=-noverify %ANT_OPTS%"
)

exit /b 0

REM --------------------------------------------------------------------------------------
REM                 FUNCTION DEFINITIONS
REM --------------------------------------------------------------------------------------

REM Checks that Apache Ant properly configured
:checkApacheAnt
	setlocal
		call ant -h > NUL 2>&1

		if %errorLevel% NEQ 0 (
			echo.
			echo --------------------------------------------------------------------------------
			echo [ERROR] Apache Ant is not configured to be launched from command line
			echo --------------------------------------------------------------------------------
			echo.
			exit /b 1
		)

		if "%ANT_HOME%" == "" (
			echo.
			echo --------------------------------------------------------------------------------
			echo [ERROR] ANT_HOME environment variable is not specified
			echo --------------------------------------------------------------------------------
			echo.
			exit /b 1
		)

		if not exist "%ANT_HOME%\lib\ant.jar" (
			echo.
			echo --------------------------------------------------------------------------------
			echo [ERROR] Apache Ant isn't properly configured, cause %ANT_HOME%\lib\ant.jar doesn't exist
			echo --------------------------------------------------------------------------------
			echo.
			exit /b 1
		)

	endlocal
	goto :eof

REM Checks version of Java
:checkJavaVersion
	setlocal
		call java -version > NUL 2>&1

		if %errorLevel% NEQ 0 (
			echo.
			echo --------------------------------------------------------------------------------
			echo [ERROR] Java is not configured to be launched from command line
			echo --------------------------------------------------------------------------------
			echo.
			exit /b 1
		)

		for /f "tokens=3" %%g in ('java -version 2^>^&1 ^| findstr /i "version"') do (
			set ANTTOOLKIT_JAVA_VERSION=%%g
		)
		set ANTTOOLKIT_JAVA_VERSION=%ANTTOOLKIT_JAVA_VERSION:"=%

		for /f "delims=. tokens=1-3" %%v in ("%ANTTOOLKIT_JAVA_VERSION%") do (
			set "ANTTOOLKIT_JAVA_VERSION=%%v.%%w"
		)

	endlocal && set ANTTOOLKIT_JAVA_VERSION=%ANTTOOLKIT_JAVA_VERSION%
	goto :eof

REM Setting AntToolkit jars classpath
:setClasspath
	setlocal
		set ANTTOOLKIT_CLASSPATH=%~f1
		if not exist %ANTTOOLKIT_CLASSPATH% (
			set "ANTTOOLKIT_CLASSPATH="
			echo.
			echo --------------------------------------------------------------------------------
			echo [WARNING] There are no AntToolkit main jar found: %ANTTOOLKIT_CLASSPATH%
			echo --------------------------------------------------------------------------------
			echo.
		)
	endlocal && set ANTTOOLKIT_CLASSPATH=%ANTTOOLKIT_CLASSPATH%
	goto :eof

REM Resolves relative path to absolute path
:getAbsolutePath
	setlocal
		if "%1"=="" exit /b 0
		set __absPath=%~f2
	endlocal && set %1=%__absPath%
	goto :eof

REM Trims spaces
:trimSpaces
	setlocal
		if "%1"=="" exit /b 0
		if "%2"=="" exit /b 0
		set __val=%1
		for /l %%a in (1,1,31) do if "!__val:~-1!"==" " set __val=!__val:~0,-1!
		for /f "tokens=* delims= " %%a in ("!__val!") do set __val=%%a
	endlocal && set %2=%__val%
	goto :eof

REM Prints error message
:printError
	setlocal
		echo.
		echo --------------------------------------------------------------------------------
		echo [ERROR] %*
		echo --------------------------------------------------------------------------------
		echo.
	endlocal
	goto :eof

REM Prints usage information
:printUsage
	if "%ANTTOOLKIT_CLASSPATH%"=="" (
		echo.
		echo --------------------------------------------------------------------------------
		echo [ERROR] 'anttoolkit-all.jar' isn't available
		echo --------------------------------------------------------------------------------
		echo.
    	exit /b %errorLevel%
	)

    call java -cp "%ANTTOOLKIT_CLASSPATH%" "com.anttoolkit.system.tools.info.FrameworkInfo"

    if %errorLevel% NEQ 0 (
		echo.
		echo --------------------------------------------------------------------------------
		echo [ERROR] Failed to print help for AntToolkit
		echo --------------------------------------------------------------------------------
		echo.
    	exit /b %errorLevel%
    )

	goto :eof

REM Updates jars classpath in anttoolkit-all.jar
:updateClasspath
	if "%ANTTOOLKIT_CLASSPATH%"=="" (
		echo.
		echo --------------------------------------------------------------------------------
		echo [ERROR] Can't update classpath in 'anttoolkit-all.jar' manifest cause such file isn't available
		echo --------------------------------------------------------------------------------
		echo.
    	exit /b %errorLevel%
	)

	call :getAbsolutePath CLASSPATH_JAR_PATH %~dp0..
	call :getAbsolutePath LIB_DIR %~dp0..\lib

	set "CLASSPATH_JAR_FILE=%CLASSPATH_JAR_PATH%\anttoolkit-all-new.jar"
	set "CLASSPATH_ANT_SCRIPT_FILE=%CLASSPATH_JAR_PATH%\create-classpath-jar.xml"

	if exist %CLASSPATH_JAR_FILE% (
		del /Q /F %CLASSPATH_JAR_FILE% > NUL 2>&1
	)

	if exist %CLASSPATH_ANT_SCRIPT_FILE% (
		del /Q /F %CLASSPATH_ANT_SCRIPT_FILE% > NUL 2>&1
	)

    call java -cp "%ANTTOOLKIT_CLASSPATH%" "com.anttoolkit.system.tools.packaging.ResourceHelper" "com/anttoolkit/system/tools/packaging/create-classpath-jar.xml" "%CLASSPATH_ANT_SCRIPT_FILE%" > NUL 2>&1

    if %errorLevel% NEQ 0 (
		echo.
		echo --------------------------------------------------------------------------------
		echo [ERROR] Failed to create classpath updating Ant script file %CLASSPATH_ANT_SCRIPT_FILE%
		echo --------------------------------------------------------------------------------
		echo.
    	exit /b %errorLevel%
    )

    call ant -lib "%ANTTOOLKIT_CLASSPATH%" -f "%CLASSPATH_ANT_SCRIPT_FILE%" "-DlibDir=%LIB_DIR%" "-DclasspathJarFile=%CLASSPATH_JAR_FILE%" > NUL 2>&1

	if %errorLevel% NEQ 0 (
		echo.
		echo --------------------------------------------------------------------------------
		echo [ERROR] Failed to create new 'anttoolkit-all.jar' file
		echo --------------------------------------------------------------------------------
		echo.
		exit /b 1
	)

	del /Q /F "%CLASSPATH_ANT_SCRIPT_FILE%" > NUL 2>&1
	del /Q /F "%CLASSPATH_JAR_PATH%\anttoolkit-all.jar" > NUL 2>&1

	move /Y "%CLASSPATH_JAR_FILE%" "%CLASSPATH_JAR_PATH%\anttoolkit-all.jar" > NUL 2>&1

	echo.
	echo --------------------------------------------------------------------------------
	echo [INFO] AntToolkit classpath inside 'anttoolkit-all.jar' manifest was successfully updated
	echo --------------------------------------------------------------------------------
	echo.

	goto :eof

REM Process command line arguments
:processArguments
	setlocal EnableDelayedExpansion
		set ANTTOOLKIT_PROFILE=
		set ANTTOOLKIT_PROFILES_DIR=
		set ANTTOOLKIT_LOG_FILE=
		set ANTTOOLKIT_LOGS_ROOT=
		set ANTTOOLKIT_SMART_LOGGING_FLAG=
		set ANTTOOLKIT_THREADS_LOGGING_FLAG=
		set ANTTOOLKIT_NORMALIZED_ARGS=

		:argsLoop

		if not "%1"=="" (

			if /I "%1"=="-help" (
				call :printUsage
				exit /b 100
			) else if /I "%1"=="-h" (
				call :printUsage
				exit /b 100
			) else if /I "%1"=="-profile" (
				if "%2"=="" (
					call :printError Incorrect '-profile' specified
					exit /b 1
				)
				set "arg=%2"
				if "!arg:~0,1!"=="-" (
					call :printError Incorrect '-profile' specified
					exit /b 1
				)
				set "ANTTOOLKIT_PROFILE=%2"
				shift
			) else if /I "%1"=="-pr" (
				if "%2"=="" (
					call :printError Incorrect '-profile' specified
					exit /b 1
				)
				set "arg=%2"
				if "!arg:~0,1!"=="-" (
					call :printError Incorrect '-profile' specified
					exit /b 1
				)
				set "ANTTOOLKIT_PROFILE=%2"
				shift
			) else if /I "%1"=="-profilesDir" (
				if "%2"=="" (
					call :printError Incorrect '-profilesDir' specified
					exit /b 1
				)
				set "arg=%2"
				if "!arg:~0,1!"=="-" (
					call :printError Incorrect '-profilesDir' specified
					exit /b 1
				)

				call :getAbsolutePath ANTTOOLKIT_PROFILES_DIR %2

                if not exist !ANTTOOLKIT_PROFILES_DIR! (
					call :printError Specified '-profilesDir' directory doesn't exist !ANTTOOLKIT_PROFILES_DIR!
					exit /b 1
                )

                shift
			) else if /I "%1"=="-pd" (
				if "%2"=="" (
					call :printError Incorrect '-profilesDir' specified
					exit /b 1
				)
				set "arg=%2"
				if "!arg:~0,1!"=="-" (
					call :printError Incorrect '-profilesDir' specified
					exit /b 1
				)

				call :getAbsolutePath ANTTOOLKIT_PROFILES_DIR %2

                if not exist !ANTTOOLKIT_PROFILES_DIR! (
					call :printError Specified '-profilesDir' directory doesn't exist !ANTTOOLKIT_PROFILES_DIR!
					exit /b 1
                )

                shift
			) else if /I "%1"=="-logFile" (
				if "%2"=="" (
					call :printError Incorrect '-logFile' specified
					exit /b 1
				)
				set "arg=%2"
				if "!arg:~0,1!"=="-" (
					call :printError Incorrect '-logFile' specified
					exit /b 1
				)
				set "ANTTOOLKIT_LOG_FILE=%2"
				shift
			) else if /I "%1"=="-l" (
				if "%2"=="" (
					call :printError Incorrect '-logFile' specified
					exit /b 1
				)
				set "arg=%2"
				if "!arg:~0,1!"=="-" (
					call :printError Incorrect '-logFile' specified
					exit /b 1
				)
				set "ANTTOOLKIT_LOG_FILE=%2"
				shift
			) else if /I "%1"=="-smartLogging" (
				set "ANTTOOLKIT_SMART_LOGGING_FLAG=true"
			) else if /I "%1"=="-sl" (
				set "ANTTOOLKIT_SMART_LOGGING_FLAG=true"
			) else if /I "%1"=="-threadsLogging" (
				set "ANTTOOLKIT_THREADS_LOGGING_FLAG=true"
			) else if /I "%1"=="-tl" (
				set "ANTTOOLKIT_THREADS_LOGGING_FLAG=true"
			) else if /I "%1"=="-lib" (
				if "%2"=="" (
					call :printError Incorrect '-lib' specified
					exit /b 1
				)
				set "arg=%2"
				if "!arg:~0,1!"=="-" (
					call :printError Incorrect '-lib' specified
					exit /b 1
				)

				set "ANTTOOLKIT_CLASSPATH=%ANTTOOLKIT_CLASSPATH%; %2"

                shift
			) else if /I "%1"=="-logsRoot" (
				if "%2"=="" (
					call :printError Incorrect '-logsRoot' specified
					exit /b 1
				)
				set "arg=%2"
				if "!arg:~0,1!"=="-" (
					call :printError Incorrect '-logsRoot' specified
					exit /b 1
				)

				call :getAbsolutePath ANTTOOLKIT_LOGS_ROOT %2

                if not exist !ANTTOOLKIT_LOGS_ROOT! (
					call :printError Specified '-logsRoot' directory "!ANTTOOLKIT_LOGS_ROOT!" doesn't exist
					exit /b 1
                )

                shift
			) else if /I "%1"=="-lr" (
				if "%2"=="" (
					call :printError Incorrect '-logsRoot' specified
					exit /b 1
				)
				set "arg=%2"
				if "!arg:~0,1!"=="-" (
					call :printError Incorrect '-logsRoot' specified
					exit /b 1
				)

				call :getAbsolutePath ANTTOOLKIT_LOGS_ROOT %2

                shift
			) else if /I "%1"=="-update" (
				call :updateClasspath
				exit /b 100
			) else (
				set "ANTTOOLKIT_NORMALIZED_ARGS=%ANTTOOLKIT_NORMALIZED_ARGS% %1"
			)

			shift
			goto :argsLoop
		)

		call :trimSpaces %ANTTOOLKIT_NORMALIZED_ARGS% ANTTOOLKIT_NORMALIZED_ARGS

	endlocal && set "ANTTOOLKIT_NORMALIZED_ARGS=%ANTTOOLKIT_NORMALIZED_ARGS%" && set "ANTTOOLKIT_PROFILE=%ANTTOOLKIT_PROFILE%" && set "ANTTOOLKIT_PROFILES_DIR=%ANTTOOLKIT_PROFILES_DIR%" && set "ANTTOOLKIT_LOG_FILE=%ANTTOOLKIT_LOG_FILE%" && set "ANTTOOLKIT_SMART_LOGGING_FLAG=%ANTTOOLKIT_SMART_LOGGING_FLAG%" && set "ANTTOOLKIT_THREADS_LOGGING_FLAG=%ANTTOOLKIT_THREADS_LOGGING_FLAG%" && set "ANTTOOLKIT_LOGS_ROOT=%ANTTOOLKIT_LOGS_ROOT%" && set "ANTTOOLKIT_CLASSPATH=%ANTTOOLKIT_CLASSPATH%"
	goto :eof

REM Validate processed arguments
:validateProcessedArgs
	setlocal
		if not "%ANTTOOLKIT_PROFILE%"=="" (
			call :validateProfile
			if %errorLevel% NEQ 0 exit /b %errorLevel%
		)

		if not "%ANTTOOLKIT_PROFILES_DIR%"=="" (
			call :validateProfilesDir
			if %errorLevel% NEQ 0 exit /b %errorLevel%
		)

		if not "%ANTTOOLKIT_LOG_FILE%"=="" (
			call :validateLogFile
			if %errorLevel% NEQ 0 exit /b %errorLevel%
		)

		if not "%ANTTOOLKIT_LOGS_ROOT%"=="" (
			call :validateLogsRoot
			if %errorLevel% NEQ 0 exit /b %errorLevel%
		)

		if "%ANTTOOLKIT_SMART_LOGGING_FLAG%"=="true" (
			call :validateSmartLogging
			if %errorLevel% NEQ 0 exit /b %errorLevel%
		)

	endlocal
	goto :eof

REM Validates profile
:validateProfile
	setlocal
		if "%ANTTOOLKIT_PROFILES_DIR%"=="" (
			call :printError You need to specify '-profilesDir' directory where to look up for profiles
			exit /b 1
		)

		if not exist %ANTTOOLKIT_PROFILES_DIR% (
			call :printError Specified '-profilesDir' directory "%ANTTOOLKIT_PROFILES_DIR%" doesn't exist
			exit /b 1
		)

		if not exist %ANTTOOLKIT_PROFILES_DIR%\%ANTTOOLKIT_PROFILE% (
			call :printError Specified profile "%ANTTOOLKIT_PROFILE%" doesn't exist inside "%ANTTOOLKIT_PROFILES_DIR%"
			exit /b 1
		)
	endlocal
	goto :eof


REM Validates profiles dir
:validateProfilesDir
	setlocal
		if not exist %ANTTOOLKIT_PROFILES_DIR% (
			call :printError Specified '-profilesDir' directory "%ANTTOOLKIT_PROFILES_DIR%" doesn't exist
			exit /b 1
		)

		if "%ANTTOOLKIT_PROFILE%"=="" (
			call :printError You specified profile but forgot to specify '-profilesDir'
			exit /b 1
		)
	endlocal
	goto :eof

REM Validates log file
:validateLogFile
	setlocal
		if "%ANTTOOLKIT_LOGS_ROOT%"=="" (
			exit /b 0
		)

		echo %ANTTOOLKIT_LOG_FILE% | findstr /C:"\\" >nul 2>&1

		if %errorLevel% EQU 0 (
			call :printError With specified option '-logsRoot' you should only provide log file name. In this case it's not allowed to provide log file name using absolute/relative path specification.
			exit /b 1
		)

		echo %ANTTOOLKIT_LOG_FILE% | findstr /C:"/" >nul 2>&1

		if %errorLevel% EQU 0 (
			call :printError With specified option '-logsRoot' you should only provide log file name. In this case it's not allowed to provide log file name using absolute/relative path specification.
			exit /b 1
		)

		exit /b 0
	endlocal
	goto :eof

REM Validates logs root directory
:validateLogsRoot
	setlocal
		if "%ANTTOOLKIT_LOG_FILE%"=="" (
			call :printError It doesn't make sense to specify '-logsRoot' without specifying '-logFile'
			exit /b 1
		)
	endlocal
	goto :eof

REM Validates smart logging option
:validateSmartLogging
	setlocal
		if "%ANTTOOLKIT_LOG_FILE%"=="" (
			call :printError It doesn't make sense to specify '-smartLogging' without specifying '-logFile'
			exit /b 1
		)

		if "%ANTTOOLKIT_LOGS_ROOT%"=="" (
			call :printError It doesn't make sense to specify '-smartLogging' without specifying '-logsRoot'
			exit /b 1
		)
	endlocal
	goto :eof

REM Enhances AntToolkit classpath by adding profile dir
:enhanceClasspath
	setlocal
		if "%ANTTOOLKIT_PROFILE%"=="" (
			exit /b 0
		)

		if "%ANTTOOLKIT_PROFILES_DIR%"=="" (
			exit /b 0
		)

		if "%ANTTOOLKIT_CLASSPATH%"=="" (
			set "ANTTOOLKIT_CLASSPATH=%ANTTOOLKIT_PROFILES_DIR%\%ANTTOOLKIT_PROFILE%"
		) else (
			set "ANTTOOLKIT_CLASSPATH=%ANTTOOLKIT_CLASSPATH%;%ANTTOOLKIT_PROFILES_DIR%\%ANTTOOLKIT_PROFILE%"
		)
	endlocal  && set ANTTOOLKIT_CLASSPATH=%ANTTOOLKIT_CLASSPATH%
	goto :eof

REM Creates logs directory if '-logsRoot' and '-logFile' specified
:resolveLogFile
	setlocal
		if "%ANTTOOLKIT_LOGS_ROOT%"=="" (
			if not "%ANTTOOLKIT_LOG_FILE%"=="" (
				call :getAbsolutePath ANTTOOLKIT_LOG_FILE %ANTTOOLKIT_LOG_FILE%
				if exist "%ANTTOOLKIT_LOG_FILE%" del /Q /F %ANTTOOLKIT_LOG_FILE%
			)
			exit /b 0
		)

		if not exist %ANTTOOLKIT_LOGS_ROOT% (
			call mkdir %ANTTOOLKIT_LOGS_ROOT% > NUL 2>&1

			if %errorLevel% NEQ 0 (
				echo.
				echo --------------------------------------------------------------------------------
				echo [ERROR] Failed to create '-logsRoot' directory: %ANTTOOLKIT_LOGS_ROOT%
				echo --------------------------------------------------------------------------------
				echo.
				exit /b 1
			)
		)

		if not "%ANTTOOLKIT_SMART_LOGGING_FLAG%"=="true" (
			set "ANTTOOLKIT_LOG_FILE=%ANTTOOLKIT_LOGS_ROOT%\%ANTTOOLKIT_LOG_FILE%"
			set "ANTTOOLKIT_LOGS_DIR=%ANTTOOLKIT_LOGS_ROOT%"
			if exist "%ANTTOOLKIT_LOG_FILE%" del /Q /F %ANTTOOLKIT_LOG_FILE%
			exit /b 0
		)

		for /F "usebackq tokens=1,2 delims==" %%i in (`wmic os get LocalDateTime /VALUE 2^>NUL`) do if '.%%i.'=='.LocalDateTime.' set ldt=%%j

		set YEAR=%ldt:~0,4%
		set MONTH=%ldt:~4,2%
		set DAY=%ldt:~6,2%
		set HOUR=%ldt:~8,2%
		set MINUTE=%ldt:~10,2%
		set SECOND=%ldt:~12,2%
		set NANOSECONDS=%ldt:~15,6%

		if "%ANTTOOLKIT_PROFILE%"=="" (
			set "ANTTOOLKIT_LOGS_DIR=%ANTTOOLKIT_LOGS_ROOT%\%YEAR%\%MONTH%\%DAY%\%HOUR%-%MINUTE%-%SECOND%-%NANOSECONDS%"
		) else (
			set "ANTTOOLKIT_LOGS_DIR=%ANTTOOLKIT_LOGS_ROOT%\%ANTTOOLKIT_PROFILE%\%YEAR%\%MONTH%\%DAY%\%HOUR%-%MINUTE%-%SECOND%-%NANOSECONDS%"
		)

		set "ANTTOOLKIT_LOG_FILE=%ANTTOOLKIT_LOGS_DIR%\%ANTTOOLKIT_LOG_FILE%"

		mkdir %ANTTOOLKIT_LOGS_DIR%

		if %errorLevel% NEQ 0 (
			echo.
			echo --------------------------------------------------------------------------------
			echo [ERROR] Failed to create directory to store logs: %ANTTOOLKIT_LOGS_DIR%
			echo --------------------------------------------------------------------------------
			echo.
			exit /b 1
		)

	endlocal && set "ANTTOOLKIT_LOG_FILE=%ANTTOOLKIT_LOG_FILE%" && set "ANTTOOLKIT_LOGS_DIR=%ANTTOOLKIT_LOGS_DIR%"
	goto :eof

REM Sets AntToolkit logger to use
:setLogger
	setlocal
		if "%ANTTOOLKIT_THREADS_LOGGING_FLAG%"=="true" (
			set "ANTTOOLKIT_LOGGER=com.anttoolkit.general.loggers.ThreadAwareLogger"
		) else (
			set "ANTTOOLKIT_LOGGER=com.anttoolkit.general.loggers.GenericLogger"
		)
	endlocal && set ANTTOOLKIT_LOGGER=%ANTTOOLKIT_LOGGER%
	goto :eof
