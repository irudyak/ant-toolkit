#!/bin/sh

export SMART_LOGGING=true

export LIBS_DIR=../../../../../libs
export PROFILES_DIR=../../../../../profiles
export BUILD_FILE=test-users-creation.xml
export LOG_FILE=test-users-creation.log

$(dirname $0)/$PROFILES_DIR/../system/run.sh "$(dirname $0)" $@
