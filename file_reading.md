# Reading time-varying data in LFRic

[TOC]

## Reading data from files

Model I/O configuration is primarily handled via the `lfric_xios_file_type`.
Each instance of this type corresponds to a file to be read from or written to, and contains information
about the file and it's I/O behaviour.
Each `lfric_xios_context_type`, which corresponds to a single XIOS context, contains a `filelist`: a linked
list of `lfric_xios_file_type` objects.
File definitions are typically added to a context in the following way:
```fortran
file_list = io_context%get_filelist()
call file_list%insert_item(lfric_xios_file_type( my_input_file_path,          &
                                                 xios_id="my_input_file_id",  &
                                                 io_mode=FILE_MODE_READ ) )
```
This enables a user to specify certain dynamic properties of a file, such as the path to the file on disk,
at runtime, whilst keeping certain properties of the file fixed.
Additional parameters can be passed to the `lfric_xios_file_type` to control additionas aspects of the I/O
behaviour associated with that file:
- the `freq` argument takes an integer number of timesteps after which the file will be operated on. This
value will default to the frequency provided in the iodef.xml definition for that file, if one is provided.
- the `operation` argument takes an enumeration that specifies whether a file is to be operated on once, or
continuously

The `lfric_xios_file_type` also allows deeper control of a file's I/O behaviour during a model run, by
allowing the user to pass a field_collection to the `lfric_xios_file_type` constructor:
```fortran
call file_list%insert_item(lfric_xios_file_type( my_input_file_path,          &
                                                 xios_id="my_input_file_id",  &
                                                 io_mode=FILE_MODE_READ,      &
                                                 freq=10,                     &
                                                 fields_in_file=input_field_collection ) )
```
This file configuration will set up the `lfric_xios_context_type` to read data from the file, directly
into the fields in the `input_field_collection`, at a frequency specified by the user (in the case above,
every 10 timesteps).
The configuration is then passed to XIOS and 'locked-in' for the models duration with the call to
`xios_close_context_definition` (called from `io_context%close_context_definition()).

## Implementation within LFRic_apps

Primarily, across the applications in lfric_apps, files defined using the `lfric_xios_file_type` are
done so in a bare-bones way, i.e:
```fortran
call files_list%insert_item( lfric_xios_file_type( ancil_fname,               &
                                                   xios_id="land_area_ancil", &
                                                   io_mode=FILE_MODE_READ ) )
```
This allows the user to specify the file's path and whether the file is to be read-from or written-to,
but that's it.
This means that the actual read and write operations (calls to `field%read_field()` and
`field%write_field()`) are spread across the various applications driver and algorithm layers. This results
in a much messier and less robust I/O implementation, and we have the opportunity to improve upon this.

## Reading time-varying data

The current impementation for reading time-varying data is poorly constructed.
The time dimension is defined as a spatial dimension in the XIOS configuration and therefore the entire
contents of each field is read in for each time entry, rather than just what is required.
This leads to massive speed and memory performance degradation.
In addition to this issue, the interface is difficult to use and adds risk.
A `time_axis_type` object for each time-varying file needs to be created as well as the
`lfric_xios_file_type`.
This object is then used at runtime to read and update the field data, which propagates the clunky
interface throughout the driver layer at all stages of the model.

### The new time-varying-read mechanism

A new method for reading time-varying data has been developed, which aims to solve the problems outlined
above.
The majority of the functionality is contained within the new `temporal_type`.
This object performs similar function to the previous `time_axis_type`, in holding the fields, which are read
in from disk, in a cache before using them to populate the model data.
However, the `temporal_type` is never exposed to the user as part of the designed interface and is held
within the `lfric_xios_file_type` - this avoids the issue of the clunky interface propagating throughout the
application layer.
With the new interface, simply setting up a file definition in the following way:
```fortran
call files_list%insert_item( lfric_xios_file_type( ancil_fname,                    &
                                                   xios_id="land_area_ancil",      &
                                                   io_mode=FILE_MODE_READ,         &
                                                   operation=OPERATION_TIMESERIES, &
                                                   files_in_file=land_area_fields ) )
```
is enough to ensure that the fields in the `land_area_fields` field collection (which should match with the
fields in the file) are updated correctly to match the data on disk without the need for any further calls to `field%read_field()`.
The other, more pressing issue of performance should also be addressed by the new `temporal_type`.
When activated (by passing through the `io_mode=FILE_MODE_READ` and `operation=OPERATION_TIMESERIES` flags
to the `lfric_xios_file_type` constructor) the `temporal_type` will create the XIOS field and file definitions
in such a way that only data that is required by the model is read-in.