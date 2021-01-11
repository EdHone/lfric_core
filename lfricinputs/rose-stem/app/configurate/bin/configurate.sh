#!/usr/bin/env bash
# *****************************COPYRIGHT******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENCE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT******************************

set -e

# Directory with LFRic source code
LFRIC_DIR=$CYLC_SUITE_SHARE_DIR/fcm_make-$NAME/extract/lfric

# Sub-directory containing the "Configurator" tool
TOOLS_DIR=$LFRIC_DIR/infrastructure/build/tools

# Run rose_picker to generate rose-meta.json and config_namelists.txt
rose_picker $LFRIC_DIR/gungho/rose-meta/lfric-gungho/HEAD/rose-meta.conf -include_dirs $LFRIC_DIR

# Run "GenerateNamelist". This takes a rose-meta.json. This produces the *_config_mod.F90 files
echo $TOOLS_DIR/GenerateNamelist rose-meta.json -directory $LFRIC_DIR
$TOOLS_DIR/GenerateNamelist rose-meta.json -directory $LFRIC_DIR

echo Namelists:
cat config_namelists.txt

# Run "GenerateLoader". This automatically generates the top-level "read_configuration"
# subroutine which processes an input file and reads all namelists LFRic recognises.
# It (rather unhelpfully) aborts if it find an alien namelist.
echo $TOOLS_DIR/GenerateLoader $LFRIC_DIR/configuration_mod.f90 $(cat config_namelists.txt | xargs)
$TOOLS_DIR/GenerateLoader $LFRIC_DIR/configuration_mod.f90 $(cat config_namelists.txt | xargs)
