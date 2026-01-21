#!/usr/bin/env python3
##############################################################################
# (C) Crown copyright 2024 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
"""
A set of tests which exercise the temporal reading functionality provided by
the LFRic-XIOS component.
"""
from testframework import TestEngine, TestFailed
from xiostest import LFRicXiosTest
from pathlib import Path
import sys

###############################################################################
class LfricXiosFullCyclicTest(LFRicXiosTest):  # pylint: disable=too-few-public-methods
    """
    Tests the LFRic-XIOS temporal reading functionality for a full set of cyclic data
    """

    def __init__(self):
        super().__init__(command=[sys.argv[1], "resources/configs/cyclic_full.nml"], processes=1)
        test_data_dir = Path(Path.cwd(), 'resources/data')
        Path('lfric_xios_cyclic_input.nc').unlink(missing_ok=True)
        self.gen_data(Path(test_data_dir, 'temporal_data.cdl'), Path('lfric_xios_cyclic_input.nc'))
        self.gen_config( Path("resources/configs/cyclic_base.nml"),
                         Path("resources/configs/cyclic_full.nml"), {} )

    def test(self, returncode: int, out: str, err: str):
        """
        Test the output of the context test
        """

        if returncode != 0:
            print(out)
            raise TestFailed(f"Unexpected failure of test executable: {returncode}\n" + 
                             f"stderr:\n" +
                             f"{err}")

        self.plot_output(Path('lfric_xios_cyclic_input.nc'),
                         Path('lfric_xios_cyclic_output.nc'),
                         'temporal_field')

        if not self.nc_data_match(Path('lfric_xios_cyclic_input.nc'),
                                  Path('lfric_xios_cyclic_output.nc'),
                                  'temporal_field'):
            raise TestFailed("Output data does not match input data for same time values")

        return "Reading full set of cylic data okay..."


class LfricXiosFutureCyclicTest(LFRicXiosTest):  # pylint: disable=too-few-public-methods
    """
    Tests the LFRic-XIOS temporal reading functionality when data is in the future
    """

    def __init__(self):
        super().__init__(command=[sys.argv[1], "resources/configs/cyclic_future.nml"], processes=1)
        test_data_dir = Path(Path.cwd(), 'resources/data')
        Path('lfric_xios_cyclic_input.nc').unlink(missing_ok=True)
        self.gen_data(Path(test_data_dir, 'temporal_data.cdl'), Path('lfric_xios_cyclic_input.nc'))
        self.gen_config( Path("resources/configs/cyclic_base.nml"),
                         Path("resources/configs/cyclic_future.nml"),
                         {"calendar_start":"'2024-01-01 14:55:00'"} )

    def test(self, returncode: int, out: str, err: str):
        """
        Test the output of the context test
        """

        expected_error_code = "ERROR: I/O context must start after data time " \
                              "window when reading cyclic temporal data"

        self.plot_output(Path('lfric_xios_cyclic_input.nc'),
                         Path('lfric_xios_cyclic_output.nc'),
                         'temporal_field')

        if returncode == 1:
            errorcode = err.split("\n")[0].split("0:")[1]
            if not errorcode == expected_error_code:
                raise TestFailed("Incorrect error handling of cyclic future data")
        else:
            raise TestFailed("Unexpected non-failure of test executable")

        return "Expected error for future cyclic data reading..."


class LfricXiosPastCyclicTest(LFRicXiosTest):  # pylint: disable=too-few-public-methods
    """
    Tests the LFRic-XIOS temporal reading functionality when data is in the future
    """

    def __init__(self):
        super().__init__(command=[sys.argv[1], "resources/configs/cyclic_future.nml"], processes=1)
        test_data_dir = Path(Path.cwd(), 'resources/data')
        Path('lfric_xios_cyclic_input.nc').unlink(missing_ok=True)
        self.gen_data(Path(test_data_dir, 'temporal_data.cdl'), Path('lfric_xios_cyclic_input.nc'))
        self.gen_data(Path(test_data_dir, 'cyclic_past_kgo.cdl'), Path('cyclic_past_kgo.nc'))
        self.gen_config( Path("resources/configs/cyclic_base.nml"),
                         Path("resources/configs/cyclic_future.nml"),
                         {"calendar_start":"'2025-01-01 14:55:00'"} )

    def test(self, returncode: int, out: str, err: str):
        """
        Test the output of the context test
        """

        if returncode != 0:
            print(out)
            raise TestFailed(f"Unexpected failure of test executable: {returncode}\n" +
                             f"stderr:\n" +
                             f"{err}")
        if not self.nc_data_match(Path('cyclic_past_kgo.nc'),
                                  Path('lfric_xios_cyclic_output.nc'),
                                  'temporal_field'):
            raise TestFailed("Output data does not match expected values")

        return "Reading full set of cylic data from the past okay..."


class LfricXiosCyclicHighFreqTest(LFRicXiosTest):  # pylint: disable=too-few-public-methods
    """
    Tests the LFRic-XIOS temporal reading functionality when data is in the future
    """

    def __init__(self):
        super().__init__(command=[sys.argv[1], "resources/configs/cyclic_future.nml"], processes=1)
        test_data_dir = Path(Path.cwd(), 'resources/data')
        Path('lfric_xios_cyclic_input.nc').unlink(missing_ok=True)
        self.gen_data(Path(test_data_dir, 'temporal_data.cdl'), Path('lfric_xios_cyclic_input.nc'))
        self.gen_data(Path(test_data_dir, 'cyclic_past_kgo.cdl'), Path('cyclic_past_kgo.nc'))
        self.gen_config( Path("resources/configs/cyclic_base.nml"),
                         Path("resources/configs/cyclic_future.nml"),
                         {"dt":"10.0",
                          "timestep_end":"'150'"} )

    def test(self, returncode: int, out: str, err: str):
        """
        Test the output of the context test
        """

        self.plot_output(Path('lfric_xios_cyclic_input.nc'),
                         Path('lfric_xios_cyclic_output.nc'),
                         'temporal_field')

        if returncode != 0:
            print(out)
            raise TestFailed(f"Unexpected failure of test executable: {returncode}\n" +
                             f"stderr:\n" +
                             f"{err}")
        if not self.nc_data_match(Path('cyclic_past_kgo.nc'),
                                  Path('lfric_xios_cyclic_output.nc'),
                                  'temporal_field'):
            raise TestFailed("Output data does not match expected values")

        return "Reading full set of cylic data from the past okay..."


class LfricXiosCyclicHighFreqTest(LFRicXiosTest):  # pylint: disable=too-few-public-methods
    """
    Tests the LFRic-XIOS temporal reading functionality when data is in the future
    """

    def __init__(self):
        super().__init__(command=[sys.argv[1], "resources/configs/cyclic_high_freq.nml"], processes=1)
        test_data_dir = Path(Path.cwd(), 'resources/data')
        Path('lfric_xios_cyclic_input.nc').unlink(missing_ok=True)
        self.gen_data(Path(test_data_dir, 'temporal_data.cdl'), Path('lfric_xios_cyclic_input.nc'))
        self.gen_data(Path(test_data_dir, 'cyclic_high_freq_kgo.cdl'), Path('cyclic_high_freq_kgo.nc'))
        self.gen_config( Path("resources/configs/cyclic_base.nml"),
                         Path("resources/configs/cyclic_high_freq.nml"),
                         {"dt":"10.0",
                          "timestep_end":"'150'"} )

    def test(self, returncode: int, out: str, err: str):
        """
        Test the output of the context test
        """

        self.plot_output(Path('lfric_xios_cyclic_input.nc'),
                         Path('lfric_xios_cyclic_output.nc'),
                         'temporal_field')

        if returncode != 0:
            print(out)
            raise TestFailed(f"Unexpected failure of test executable: {returncode}\n" +
                             f"stderr:\n" +
                             f"{err}")
        if not self.nc_data_match(Path('cyclic_high_freq_kgo.nc'),
                                  Path('lfric_xios_cyclic_output.nc'),
                                  'temporal_field'):
            raise TestFailed("Output data does not match expected values")

        return "Reading full set of cylic data from the past okay..."


class LfricXiosCyclicNonSyncTest(LFRicXiosTest):  # pylint: disable=too-few-public-methods
    """
    Tests the LFRic-XIOS temporal reading functionality when model timesteps do not match data timesteps
    """

    def __init__(self):
        super().__init__(command=[sys.argv[1], "resources/configs/cyclic_non_sync.nml"], processes=1)
        test_data_dir = Path(Path.cwd(), 'resources/data')
        Path('lfric_xios_cyclic_input.nc').unlink(missing_ok=True)
        self.gen_data(Path(test_data_dir, 'temporal_data.cdl'), Path('lfric_xios_cyclic_input.nc'))
        self.gen_data(Path(test_data_dir, 'non_sync_kgo.cdl'), Path('non_sync_kgo.nc'))
        self.gen_config( Path("resources/configs/cyclic_base.nml"),
                         Path("resources/configs/cyclic_non_sync.nml"),
                         {"dt":"10.0",
                          "calendar_start":"'2024-01-01 15:03:20'",
                          "timestep_end":"'30'"} )

    def test(self, returncode: int, out: str, err: str):
        """
        Test the output of the context test
        """

        if returncode != 0:
            print(out)
            raise TestFailed(f"Unexpected failure of test executable: {returncode}\n" +
                             f"stderr:\n" +
                             f"{err}")
        if not self.nc_data_match(Path('non_sync_kgo.nc'),
                                  Path('lfric_xios_cyclic_output.nc'),
                                  'temporal_field'):
            raise TestFailed("Output data does not match expected values")

        self.plot_output(Path('lfric_xios_cyclic_input.nc'),
                         Path('lfric_xios_cyclic_output.nc'),
                         'temporal_field')

        return "Reading non-synchronised cyclic data okay..."



##############################################################################
if __name__ == "__main__":
    TestEngine.run(LfricXiosFullCyclicTest())
    TestEngine.run(LfricXiosFutureCyclicTest())
    TestEngine.run(LfricXiosPastCyclicTest())
    TestEngine.run(LfricXiosCyclicHighFreqTest())
    TestEngine.run(LfricXiosCyclicNonSyncTest())