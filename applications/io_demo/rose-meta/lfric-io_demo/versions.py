import sys

from metomi.rose.upgrade import MacroUpgrade

from .version21_22 import *


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


class vn22_t4661(MacroUpgrade):
    """Upgrade macro for ticket #4661 by Denis Sergeev."""

    BEFORE_TAG = "vn2.2"
    AFTER_TAG = "vn2.2_t4661"

    def upgrade(self, config, meta_config=None):
        # Commands From: rose-meta/lfric-driver
        self.add_setting(config, ["namelist:extrusion", "eta_values"], "''")
        return config, self.reports


class vn22_t4684(MacroUpgrade):
    """Upgrade macro for ticket #4684 by Ed Hone."""

    BEFORE_TAG = "vn2.2_t4661"
    AFTER_TAG = "vn2.2_t4684"

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
