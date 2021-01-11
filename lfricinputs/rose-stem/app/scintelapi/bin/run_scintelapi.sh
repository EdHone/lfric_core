#!/bin/bash
#
echo Executable is $(which scintelapi.exe)
rose mpi-launch scintelapi.exe lfric.nl scintelapi.nl
if [ $? -gt 0 ]; then 
  echo '*** Failure detected, check log files for details ***' >&2
  exit 1
else 
  mv source_dump_000001.nc target_dump.nc
fi
