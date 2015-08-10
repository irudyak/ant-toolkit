#!/bin/sh

export SMART_LOGGING=true

export LIBS_DIR=../../../../../../libs
export PROFILES_DIR=../../../../../../profiles
export BUILD_FILE=smoke-tests.xml
export LOG_FILE=smoke-tests.log

$(dirname $0)/$PROFILES_DIR/../system/run.sh "$(dirname $0)" $@
