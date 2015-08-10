echo off

set "SMART_LOGGING=true"
set "LOGGER=com.anttoolkit.general.loggers.ThreadAwareLogger"

set "LIBS_DIR=..\..\..\..\..\..\libs"
set "PROFILES_DIR=..\..\..\..\..\..\profiles"
set "LOG_FILE=load-tests.log"
set "BUILD_FILE=load-tests.xml"

call "%~dp0%PROFILES_DIR%\..\system\run.bat" "%~dp0" %*%