# IDEAFilter 0.2.1

- Update Rd `\links{}` to use a package 'anchor' when referring to a package outside of `IDEAfilter`. This is used for HTML help, rendered by CRAN.

# IDEAFilter 0.2.0

## New features
* Added a new module implementation `IDEAFilter()` that is more modern and uses less resources. The old implementation `shiny_data_filter()` has been deprecated.
* Added a feature to allow users to restrict the filter to a subset of columns
* Added an erase button to filter items to allow users to clear applied filters

## Minor improvements and fixes
* Fixed a bug that was trying to assign an attribute to a NULL
* Fixed a bug that was causing inaccurate filtering for `datetime` vectors
* Fixed a bug that was not always including a selected NA filter for `logical` vectors
* Fixed a bug that was causing the column selectize input to fail
* Upgraded `pillar::new_pillar_type()` to `pillar::type_sum()`
* Added trash collector to delete inputs and observers from dynamic vector modules when removed from UI

# IDEAFilter 0.1.3
* Cited works of other contributors (`shinyDataFilter` & `SortableJS`)

# IDEAFilter 0.1.2 (CRAN Release)

* Prepped package for release on CRAN
* Built up readme file

# IDEAFilter 0.1.1

* Added check to make sure `shiny_vector_filter` exists in global environment

# IDEAFilter 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Upgraded `pillar::new_pillar_type(obj)$type` to `pillar::new_pillar_type(obj)[[1]][1]`
