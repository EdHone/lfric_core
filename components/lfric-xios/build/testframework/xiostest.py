#!/usr/bin/env python3
##############################################################################
# (C) Crown copyright 2024 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
import os
import subprocess
from pathlib import Path
import sys
from typing import List

from testframework import MpiTest
import xarray as xr
import matplotlib.pyplot as plt


##############################################################################
class LFRicXiosTest(MpiTest):
    """
    Base for LFRic-XIOS integration tests.
    """

    def __init__(self, command=sys.argv[1], processes=1):
        super().__init__(command, processes)
        self.xios_out: List[XiosOutput] = []
        self.xios_err: List[XiosOutput] = []

    def gen_data(self, source: Path, dest: Path):
        """
        Create input data files from CDL formatted text.
        """
        proc = subprocess.Popen(
            ['ncgen', '-k', 'nc4', '-o', f'{dest}', f'{source}'],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            )
        _, err = proc.communicate()
        if proc.returncode != 0:
            raise Exception("Test data generation failed:\n" + f"{err}")
        
    def gen_config(self, config_source: Path, config_out: Path, new_config: dict):
        """
        Create an LFRic configuration namelist.
        """
        config_in = open(config_source, 'r')
        config = config_in.readlines()
        for key in new_config.keys():
            for i in range(len(config)):
                if key in config[i]:
                    config[i] = f"  {key}={new_config[key]}\n"
        config_in.close()

        f = open(config_out, "w")
        for line in config:
            f.write(line)
        f.close()            

    def nc_kgo_check(self, output: Path, kgo: Path):
        """
        Compare output files with nccmp.
        """
        proc = subprocess.Popen(
            ['nccmp', '-Fdm', '--exclude=Mesh2d', '--tolerance=0.000001', f'{output}', f'{kgo}'],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            )
        _, err = proc.communicate()

        return proc.returncode, err
    
    def nc_data_match(self, in_file: Path, out_file: Path, varname: str):
        """
        Contextually compare output data.
        """
        ds_in = xr.open_dataset(in_file, engine='netcdf4', decode_timedelta=False)
        ds_out = xr.open_dataset(out_file, engine='netcdf4', decode_timedelta=False)

        comparison_window = [max(min(ds_out['time'].values), min(ds_in['time'].values)),
                            min(max(ds_out['time'].values), max(ds_in['time'].values))]

        ds_in_comp = ds_in.sel(time=slice(comparison_window[0], comparison_window[1]))
        ds_out_comp = ds_out.sel(time=slice(comparison_window[0], comparison_window[1]))

        if ds_in_comp['time'].size == 0:
            return False
        else:
            result = [(ds_in_comp['time'] == ds_out_comp['time']).values.all(),
                    (ds_in_comp[varname] == ds_out_comp[varname]).values.all()]
            return all(result)

    def plot_output(self, in_file: Path, out_file: Path, varname: str):
        """
        Visually compare input and output data.
        """

        def get_ts_data(file_path, field_id):

            ds = xr.open_dataset(file_path, engine='netcdf4', decode_timedelta=False)
            ts = ds[field_id].mean(ds[field_id].dims[1::])
            time = ds[field_id].coords['time']

            return ts, time

        input_ts, input_time = get_ts_data(in_file, varname)
        output_ts, output_time = get_ts_data(out_file, varname)

        plt.rcParams["font.family"] = "serif"
        _, ax = plt.subplots(figsize=([10.8, 4.8]))
        ax.scatter(output_time, output_ts, c='C0', s=50)
        ax.plot(output_time, output_ts, linestyle='--', lw=2, label="Model output data")
        ax.scatter(input_time, input_ts, c='C3', marker='s', s=100, label="Input data")

        ax.set_xlabel("Date/Time")
        ax.set_ylabel("Mean model data")

        plt.legend(frameon=False)
        plt.savefig(f"{type(self).__name__}.png", bbox_inches="tight")
        plt.close()

    def post_execution(self, return_code):
        """
        Cache XIOS logging output for analysis.
        """

        for proc in range(self._processes):
            self.xios_out.append(XiosOutput(f"xios_client_{proc}.out"))
            self.xios_err.append(XiosOutput(f"xios_client_{proc}.err"))


class XiosOutput:
    """
    Simple class to hold XIOS output log information
    """

    def __init__(self, filename):
        self.path: Path = Path(os.getcwd()) / Path(filename)

        with open(self.path, "rt") as handle:
            self.contents = handle.read()

    def exists(self):
        """
        Checks if log output file exists
        """
        return os.path.exists(self.path)
