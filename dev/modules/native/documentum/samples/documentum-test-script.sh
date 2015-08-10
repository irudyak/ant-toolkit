#!/bin/sh

export SMART_LOGGING=true

export PROFILES_DIR=../../profiles

export BUILD_FILE=documentum-test-script.xml
export LOG_FILE=documentum-test-script.log

$(dirname $0)/$PROFILES_DIR/../system/run.sh "$(dirname $0)" $@
