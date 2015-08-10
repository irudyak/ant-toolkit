echo off
echo.

setlocal

REM Initializing environment
call %~dp0init.bat %*

REM Service command (either -help or -update) was executed
if %errorLevel% EQU 100 exit /b 0

REM Failed to initialized environment
if %errorLevel% NEQ 0 exit /b %errorLevel%

REM Print header
echo ---------------------------------------------------------
echo -----------    AntToolkit framework    ------------------
echo ---------------------------------------------------------

REM Print profile info
if not "%ANTTOOLKIT_PROFILE%"=="" (
	echo Profile:     ^| %ANTTOOLKIT_PROFILE%
	echo ---------------------------------------------------------
	echo Profile dir: ^| %ANTTOOLKIT_PROFILES_DIR%\%ANTTOOLKIT_PROFILE%
	echo ---------------------------------------------------------
)

REM Print logging info
if not "%ANTTOOLKIT_LOG_FILE%"=="" (
	echo Log file:    ^| %ANTTOOLKIT_LOG_FILE%
	echo ---------------------------------------------------------
)

REM Initializing classpath
if "%ANTTOOLKIT_CLASSPATH%"=="" (
	set "CLASSPATH=%ANT_HOME%\lib\ant.jar;%CLASSPATH%"
) else (
	set "CLASSPATH=%ANT_HOME%\lib\ant.jar;%ANTTOOLKIT_CLASSPATH%;%CLASSPATH%"
)

REM Executing Apache Ant without specifying log file
if "%ANTTOOLKIT_LOG_FILE%"=="" (
	echo ant -logger %ANTTOOLKIT_LOGGER% %ANTTOOLKIT_NORMALIZED_ARGS%
	echo ---------------------------------------------------------
	echo.
	call ant -logger %ANTTOOLKIT_LOGGER% %ANTTOOLKIT_NORMALIZED_ARGS%
) else (
	REM Executing Apache Ant specifying log file
	if "%ANTTOOLKIT_LOGS_DIR%"=="" (
		echo ant -logger %ANTTOOLKIT_LOGGER% -logfile "%ANTTOOLKIT_LOG_FILE%" %ANTTOOLKIT_NORMALIZED_ARGS%
		echo ---------------------------------------------------------
		echo.
		call ant -logger %ANTTOOLKIT_LOGGER% -logfile "%ANTTOOLKIT_LOG_FILE%" %ANTTOOLKIT_NORMALIZED_ARGS%
	REM Executing Apache Ant specifying log file and LOGS_DIR parameter
	) else (
		echo ant -logger %ANTTOOLKIT_LOGGER% -logfile "%ANTTOOLKIT_LOG_FILE%" "-DLOGS_DIR=%ANTTOOLKIT_LOGS_DIR%" %ANTTOOLKIT_NORMALIZED_ARGS%
		echo ---------------------------------------------------------
		echo.
		call ant -logger %ANTTOOLKIT_LOGGER% -logfile "%ANTTOOLKIT_LOG_FILE%" "-DLOGS_DIR=%ANTTOOLKIT_LOGS_DIR%" %ANTTOOLKIT_NORMALIZED_ARGS%
	)
)

echo.

set errLev=%errorLevel%

REM Report error
if %errLev% NEQ 0 (
	echo -----------------------------------------------------
	echo ---------  !!! EXECUTION FAILED !!!  ----------------
	echo -----------------------------------------------------
	echo.
	exit /b %errLev%
)

REM Report success
echo ----------------------------------------------
echo ---------  EXECUTION SUCCEED  ----------------
echo ----------------------------------------------
echo.

endlocal
goto :eof
