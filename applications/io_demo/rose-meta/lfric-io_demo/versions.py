import sys

from metomi.rose.upgrade import MacroUpgrade

from .version22_30 import *


class UpgradeError(Exception):
    """Exception created when an upgrade fails."""

    def __init__(self, msg):
        self.msg = msg

    def __repr__(self):
        sys.tracebacklimit = 0
        return self.msg

    __str__ = __repr__


"""
Copy this template and complete to add your macro
class vnXX_txxx(MacroUpgrade):
    # Upgrade macro for <TICKET> by <Author>
    BEFORE_TAG = "vnX.X"
    AFTER_TAG = "vnX.X_txxx"
    def upgrade(self, config, meta_config=None):
        # Add settings
        return config, self.reports
"""


class vn30_t216(MacroUpgrade):
    """Upgrade macro for ticket #216 by Ed Hone."""

    BEFORE_TAG = "vn3.0"
    AFTER_TAG = "vn3.0_t216"

    def upgrade(self, config, meta_config=None):
        # Commands From: rose-meta/lfric-io_demo
        """Add new io_demo namelist"""
        source = self.get_setting_value(
            config, ["file:configuration.nml", "source"]
        )
        source = re.sub(
            r"namelist:io",
            r"namelist:io" + "\n" + " namelist:io_demo",
            source,
        )
        self.change_setting_value(
            config, ["file:configuration.nml", "source"], source
        )
        self.add_setting(config, ["namelist:io_demo"])

        """Move multifile_io setting from io namelist to io_demo"""
        self.remove_setting(
            config, ["namelist:io", "multifile_io"])
        self.add_setting(
            config, ["namelist:io_demo", "multifile_io"], ".false."
        )

        """Add new default settings"""
        self.add_setting(
            config, ["namelist:io_demo", "io_benchmark"], ".false."
        )
        self.add_setting(
            config, ["namelist:io_demo", "n_benchmark_fields"], "0"
        )

        return config, self.reports
