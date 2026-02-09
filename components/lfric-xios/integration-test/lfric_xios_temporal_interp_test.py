#!/usr/bin/env python3
##############################################################################
# (C) Crown copyright 2024 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
"""
A set of tests which exercise the temporal reading functionality provided by
the LFRic-XIOS component.
The tests cover the reading of a piece of non-cyclic temporal data with data
points ranging from 15:01 to 15:10 in 10 1-minute intervals. The model start
time is changed to change how the model interacts with the data.
"""
from testframework import TestEngine, TestFailed
from xiostest import LFRicXiosTest
from pathlib import Path
import sys

###############################################################################
class LfricXiosFullInterpTest(LFRicXiosTest):  # pylint: disable=too-few-public-methods
    """
    Tests the LFRic-XIOS temporal reading functionality for a full set of non-cyclic data
    """

    def __init__(self):
        super().__init__(command=[sys.argv[1], "resources/configs/cyclic_high_freq.nml"], processes=1)
        test_data_dir = Path(Path.cwd(), 'resources/data')
        Path('lfric_xios_interp_input.nc').unlink(missing_ok=True)
        self.gen_data(Path(test_data_dir, 'temporal_data.cdl'), Path('lfric_xios_interp_input.nc'))
        self.gen_data(Path(test_data_dir, 'cyclic_high_freq_kgo.cdl'), Path('cyclic_high_freq_kgo.nc'))
        self.gen_config( Path("resources/configs/cyclic_base.nml"),
                         Path("resources/configs/cyclic_high_freq.nml"),
                         {"dt":"10.0",
                          "timestep_end":"'150'"} )

    def test(self, returncode: int, out: str, err: str):
        """
        Test the output of the context test
        """

        if returncode != 0:
            print(out)
            raise TestFailed(f"Unexpected failure of test executable: {returncode}\n" +
                             f"stderr:\n" +
                             f"{err}")

        self.plot_output(Path('lfric_xios_interp_input.nc'),
                         Path('lfric_xios_interp_output.nc'),
                         'temporal_field')

        if not self.nc_data_match(Path('lfric_xios_interp_input.nc'),
                                  Path('lfric_xios_interp_output.nc'),
                                  'temporal_field'):
            raise TestFailed("Output data does not match input data for same time values")

        return "Reading and interpolating data okay..."



##############################################################################
if __name__ == "__main__":
    TestEngine.run(LfricXiosFullInterpTest())
