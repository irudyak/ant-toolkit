echo off

set "SMART_LOGGING=true"

set "LIBS_DIR=..\..\..\..\..\..\libs"
set "PROFILES_DIR=..\..\..\..\..\..\profiles"
set "LOG_FILE=smoke-tests.log"
set "BUILD_FILE=smoke-tests.xml"

call "%~dp0%PROFILES_DIR%\..\system\run.bat" "%~dp0" %*%