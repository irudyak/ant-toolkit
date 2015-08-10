echo off

set "SMART_LOGGING=true"

set "PROFILES_DIR=..\..\profiles"

set "LOG_FILE=documentum-test-script.log"
set "BUILD_FILE=documentum-test-script.xml"

call "%~dp0%PROFILES_DIR%\..\system\run.bat" "%~dp0" %*%