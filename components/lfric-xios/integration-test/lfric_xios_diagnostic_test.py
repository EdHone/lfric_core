#!/usr/bin/env python3
##############################################################################
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
"""
Tests the lfric_xios_diagnostic_type by adding various diagnostics to the I/O
context in different scenarios.
"""

from testframework import TestEngine, TestFailed
from xiostest import LFRicXiosTest
import sys

###############################################################################
class LfricXiosDiagnosticTest(LFRicXiosTest):
    """
    Tests the lfric_xios_diagnostic_type by adding a diagnostic to the I/O context.
    """

    def __init__(self):
        super().__init__(command=[sys.argv[1], "context.nml"], processes=1, iodef_file="iodef_diagnostic.xml")
        self.gen_config( "context.nml", "context.nml", {} )

    def test(self, returncode: int, out: str, err: str):
        """
        Test the output of the context test
        """

        if returncode != 0:
            raise TestFailed(
                f"Unexpected failure of test executable: {returncode}\n"
                "stderr:\n"
                f"{err}"
            )

        return "Simple diagnostic test passed"

class LfricXiosDiagnosticNotInIodefTest(LFRicXiosTest):
    """
    Tests the lfric_xios_diagnostic_type behaviour when the diagnostic is not in the iodef.xml.
    """

    def __init__(self):
        super().__init__(command=[sys.argv[1], "context.nml"], processes=1)
        self.gen_config( "context.nml", "context.nml", {} )

    def test(self, returncode: int, out: str, err: str):
        """
        Test the output of the context test
        """

        expected_error_code = "ERROR: Diagnostic field 'diagnostic_field' must have a definition in iodef.xml" # pylint: disable=line-too-long

        if returncode == 1:
            errorcode = err.split("\n")[0].split("0:")[1]
            if not errorcode == expected_error_code:
                raise TestFailed("Incorrect error handling of non-existent diagnostic.")
        else:
            raise TestFailed("Unexpected non-failure of test executable")

        return "Correctly handled missing diagnostic entry in iodef.xml..."


##############################################################################
if __name__ == "__main__":
    TestEngine.run(LfricXiosDiagnosticTest())
    TestEngine.run(LfricXiosDiagnosticNotInIodefTest())
