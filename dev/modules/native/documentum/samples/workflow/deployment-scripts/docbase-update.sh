#!/bin/sh

export SMART_LOGGING=true

export LIBS_DIR=../../../../libs
export PROFILES_DIR=../../../../profiles
export BUILD_FILE=docbase-update.xml
export LOG_FILE=docbase-update.log

$(dirname $0)/$PROFILES_DIR/../system/run.sh "$(dirname $0)" $@
