#!/bin/sh

export SMART_LOGGING=true
export LOGGER=com.anttoolkit.general.loggers.ThreadAwareLogger

export LIBS_DIR=../../../../../../libs
export PROFILES_DIR=../../../../../../profiles
export BUILD_FILE=load-tests.xml
export LOG_FILE=load-tests.log

$(dirname $0)/$PROFILES_DIR/../system/run.sh "$(dirname $0)" $@
