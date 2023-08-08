# IDEAFilter (development version)
* Fix bug that was trying to assign an attribute to a NULL (#15)
* Fix bug that was causing inaccurate filtering for `datetime` vectors (#17)
* Add new implementation `IDEAFilter()` that uses a more modern implementation and less resources (#22)
* Allow user to restrict filter to a subset of columns (#14)
* Upgraded `pillar::new_pillar_type()` to `pillar::type_sum()` (#9)

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
