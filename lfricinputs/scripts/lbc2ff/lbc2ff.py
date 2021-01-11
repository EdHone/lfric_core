#!/usr/bin/env python3
#
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENCE.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
"""
Converts an LBC file into standard fieldsfile.

Takes a single positional argument - input filename. Script will output a
fieldsfile in the working directory with the same name as the input file with
'.ff' appended.
"""
import argparse
import mule
from mule.lbc import LBCToMaskedArrayOperator
from lbc_stash_map import LBC_STASH_MAP


def main():
    parser = argparse.ArgumentParser(usage='Convert an LBC file into a fieldsfile')
    parser.add_argument("input_filename", help=argparse.SUPPRESS)
    args = parser.parse_args()
    input_filename = args.input_filename

    # Open file
    lbc = mule.lbc.LBCFile.from_file(input_filename)
    ff = create_ff_from_lbc(lbc)

    # Use the mule provided LBC operator. This converts the 1d LBC array
    # (which contains all levels) into a standard 3d array of levels, rows
    # and columns
    lbc_to_masked = LBCToMaskedArrayOperator()
    for field in lbc.fields:
        field = lbc_to_masked(field)
        ncols = field.lbnpt
        nrows = field.lbrow
        # Rim and halo widths
        halo_code = field.lbuser3
        rimwidth = int(halo_code // 10000)
        halo_ns = int(halo_code - rimwidth * 10000) // 100
        halo_ew = int(halo_code - rimwidth * 10000 - halo_ns * 100)
        for level_num, single_level in enumerate(field.get_data(), 1):
            # Copy whole field to get metadata
            field_2d = field.copy()
            # Data provider is mule method for telling new field
            # where to get data from. Slice the array to remove halo regions
            # as not needed in LFRic LBC file
            array_provider = mule.ArrayDataProvider(
                single_level.filled(mule._REAL_MDI)[halo_ns:nrows + halo_ns,
                halo_ew:ncols + halo_ew])
            field_2d.set_data_provider(array_provider)
            # Update the stash code to the standard prognostic version
            field_2d.lbuser4 = LBC_STASH_MAP[field_2d.lbuser4]
            # Update level number
            field_2d.lblev = level_num
            # Update lbhem variable to be what is expected by fieldsfile
            field_2d.lbhem = ff.fixed_length_header.horiz_grid_type % 100
            ff.fields.append(field_2d)
            # import pdb;pdb.set_trace()
    # Set dataset version
    ff.fixed_length_header.data_set_format_version = 20
    # Update dataset type to be fieldsfile
    ff.fixed_length_header.dataset_type = 3
    ff.to_file(input_filename + ".ff")


def create_ff_from_lbc(lbc):
    # Create new file to copy into
    ff = mule.FieldsFile()
    # Copy across all headers
    ff.fixed_length_header = mule.FixedLengthHeader(
        lbc.fixed_length_header.raw[1:])
    ff.integer_constants = mule.ff.FF_IntegerConstants(
        lbc.integer_constants.raw[1:])
    ff.real_constants = mule.ff.FF_RealConstants(
        lbc.real_constants.raw[1:])
    ff.level_dependent_constants = mule.ff.FF_LevelDependentConstants.empty(
        lbc.level_dependent_constants.raw.shape[0])
    ff.level_dependent_constants.raw[:, 1:5] = (
        lbc.level_dependent_constants.raw[:, 1:])
    return ff


if __name__ == "__main__":
    main()
