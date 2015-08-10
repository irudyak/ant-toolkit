echo off

set "SMART_LOGGING=true"

set "LIBS_DIR=..\..\..\..\libs"
set "PROFILES_DIR=..\..\..\..\profiles"
set "LOG_FILE=docbase-update.log"
set "BUILD_FILE=docbase-update.xml"

call "%~dp0%PROFILES_DIR%\..\system\run.bat" "%~dp0" %*%