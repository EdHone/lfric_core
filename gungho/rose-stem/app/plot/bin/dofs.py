#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################
'''
Python script to plot xz slices along the y=0 and xy slices on a specified level 

This version takes nodal format output files and
interpolates onto a regular grid.

Filename hardcoded.

Levels are determined from the data

This version stitches together a directory of files
and extracts all levels so it can work in the serial
case where there is one file or the parallel case where
there is a file for each processor.

'''

import numpy as np
# Need to set a non-interactive backend for suites
import matplotlib
matplotlib.use('Agg')

import matplotlib.pyplot as plt
import matplotlib.cm as cm

from scipy.interpolate import griddata
import math
import glob
import sys

# Make an empty list to hold the levels we find in the data


x = []
y = []
z = []
levels = []

def process_file_list(filestem):

  # Get the list of files to stitch together
  dirlist = glob.glob(filestem)

  # If no files are found then don't try to process them
  if len(dirlist) < 1:
    print("No files found to plot")
  else:

    for f in dirlist:
      print "processing file ", f
      fo = open(f, "r")

      # Step through all lines in the file, split the lines
      # and where the level matches the specifed one, append 
      # data to appropriate list 
      for strline in fo:
         strsplit = strline.split()
         # Check we got a valid data line
         if (len(strsplit) == 5 or len(strsplit) == 7):
            # Get the level
            level = float(strsplit[3])
            # Is the level already in the levels list?
            if (level in levels):
               # If it is then append the data into the correct list
               x[levels.index(level)].append(float(strsplit[0]))
               y[levels.index(level)].append(float(strsplit[1]))
               z[levels.index(level)].append(float(strsplit[4]))
            else:
               # Add the level to the levels list and append
               # corresponding empty lists to x, y and z lists
               levels.append(level)
               x.append([])
               y.append([])
               z.append([])
               # ...and then append the data
               x[levels.index(level)].append(float(strsplit[0]))
               y[levels.index(level)].append(float(strsplit[1]))
               z[levels.index(level)].append(float(strsplit[4]))

      fo.close()
       
def make_figure(plotpath, field, timestep):
  fig = plt.figure(figsize=(10,5))
  nz = len(levels)
  for p in range(len(levels)):
    g = p/(2.0*nz) + 0.5
    plt.plot(np.asarray(z[p]),color=(g,g,g),linewidth=2)

  plt.title('max: %e, min: %e'%(np.amax(z),np.amin(z)))
  out_file_name = plotpath + "/" "dofs_" + field + "_" + timestep +  ".png"
  plt.savefig(out_file_name , bbox_inches='tight')

 
if __name__ == "__main__":

  try:
    config, datapath, fields, timesteps, plotpath = sys.argv[1:9]
  except ValueError:
    print("Usage: {0} <datapath> <field_names> <timestep_list> <plotpath>".format(sys.argv[0]))
    exit(1)

  # Split out the list of fields
  field_list = fields.split(':')

  # Split out the list of timesteps
  ts_list = timesteps.split(':')

  for field in field_list:

    for ts in ts_list:
      # Clear the lists in between plots
      del levels[:]
      del x[:]
      del y[:]
      del z[:]
      filestem =  datapath + "/" + config + "_nodal_" + field + "_" + ts + "*"      
      process_file_list(filestem)
      # Only try to plot if we found some files for this timestep
      if len(levels) > 0:
        make_figure(plotpath,field, ts)

