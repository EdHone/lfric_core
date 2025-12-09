#!/usr/bin/env python3
##############################################################################
# (C) Crown copyright 2025 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
"""
A set of tests which exercise the temporal reading functionality provided by
the LFRic-XIOS component. For these tests the file is configured mainly via
the iodef.xml file, rather than the fortran API.
The tests cover the reading of a piece of non-cyclic temporal data with data
points ranging from 15:01 to 15:10 in 10 1-minute intervals. The model start
time is changed to change how the model interacts with the data.
"""
from testframework import TestEngine, TestFailed
from xiostest import LFRicXiosTest
from pathlib import Path
import sys

###############################################################################
class LfricXiosFullNonCyclicIodefTest(LFRicXiosTest):  # pylint: disable=too-few-public-methods
    """
    Tests the LFRic-XIOS temporal reading functionality for a full set of non-cyclic data
    """

    def __init__(self):
        super().__init__(command=[sys.argv[1], "resources/configs/non_cyclic_full.nml"], processes=1)
        test_data_dir = Path(Path.cwd(), 'resources/data')
        Path('lfric_xios_temporal_input.nc').unlink(missing_ok=True)
        self.gen_data(Path(test_data_dir, 'temporal_data.cdl'), Path('lfric_xios_temporal_input.nc'))
        self.gen_config( Path("resources/configs/non_cyclic_base.nml"),
                         Path("resources/configs/non_cyclic_full.nml"), {} )
        self.use_iodef(Path("resources/iodef_temporal.xml"))

    def test(self, returncode: int, out: str, err: str):
        """
        Test the output of the context test
        """

        if returncode != 0:
            print(out)
            raise TestFailed(f"Unexpected failure of test executable: {returncode}\n" + 
                             f"stderr:\n" +
                             f"{err}")
        if not self.nc_data_match(Path('lfric_xios_temporal_input.nc'),
                                  Path('lfric_xios_temporal_output.nc'),
                                  'temporal_field'):
            raise TestFailed("Output data does not match input data for same time values")

        return "Reading full set of non-cylic data okay..."


class LfricXiosFullNonCyclicIodefHighFreqTest(LFRicXiosTest):  # pylint: disable=too-few-public-methods
    """
    Tests the LFRic-XIOS temporal reading functionality for a full set of
    non-cyclic data at hieher model frequency
    """

    def __init__(self):
        super().__init__(command=[sys.argv[1], "resources/configs/non_cyclic_full.nml"], processes=1)
        test_data_dir = Path(Path.cwd(), 'resources/data')
        Path('lfric_xios_temporal_input.nc').unlink(missing_ok=True)
        self.gen_data(Path(test_data_dir, 'temporal_data.cdl'), Path('lfric_xios_temporal_input.nc'))
        self.gen_config( Path("resources/configs/non_cyclic_base.nml"),
                         Path("resources/configs/non_cyclic_full.nml"),
                         {"dt": 10.0,
                          "timestep_end": 60} )
        self.use_iodef(Path("resources/iodef_temporal.xml"))

    def test(self, returncode: int, out: str, err: str):
        """
        Test the output of the context test
        """

        if returncode != 0:
            print(out)
            raise TestFailed(f"Unexpected failure of test executable: {returncode}\n" +
                             f"stderr:\n" +
                             f"{err}")
        if not self.nc_data_match(Path('lfric_xios_temporal_input.nc'),
                                  Path('lfric_xios_temporal_output.nc'),
                                  'temporal_field'):
            raise TestFailed("Output data does not match input data for same time values")

        return "Reading full set of non-cylic data okay..."


class LfricXiosFullNonCyclicIodefNoFreqTest(LFRicXiosTest):  # pylint: disable=too-few-public-methods
    """
    Tests the error handling for the case where there is no frequency set in either
    the iodef or the fortran configuration.
    """

    def __init__(self):
        super().__init__(command=[sys.argv[1], "resources/configs/non_cyclic_full.nml"], processes=1)
        test_data_dir = Path(Path.cwd(), 'resources/data')
        Path('lfric_xios_temporal_input.nc').unlink(missing_ok=True)
        self.gen_data(Path(test_data_dir, 'temporal_data.cdl'), Path('lfric_xios_temporal_input.nc'))
        self.gen_config( Path("resources/configs/non_cyclic_base.nml"),
                         Path("resources/configs/non_cyclic_full.nml"), {} )
        self.use_iodef(Path("resources/iodef.xml"))

    def test(self, returncode: int, out: str, err: str):
        """
        Test the output of the context test
        """

        expected_xios_err = 'In file "type_impl.hpp", function "void xios::CType<T>::_checkEmpty() const [with T = xios::CDuration]",  line 210 -> Data is not initialized'

        if returncode == 134:
            if self.xios_err[0].contents.strip() == expected_xios_err:
                return "Expected failure of test executable due to missing frequency setting."
            else:
                raise TestFailed("Test executable failed, but with unexpected error message.")
        elif returncode == 0:
            raise TestFailed("Test executable succeeded unexpectedly despite missing frequency setting.")
        else:
            raise TestFailed("Test executable failed with unexpected return code.")




##############################################################################
if __name__ == "__main__":
    TestEngine.run(LfricXiosFullNonCyclicIodefTest())
    TestEngine.run(LfricXiosFullNonCyclicIodefHighFreqTest())
    TestEngine.run(LfricXiosFullNonCyclicIodefNoFreqTest())