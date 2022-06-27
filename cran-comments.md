## Re-submission 2022-06-27
This is a re-submission. In this version I have:

* Referenced package names using single quotes in the description field. Also, spelled out 'UI' to 'user interface'.

* Added appropriate `@return` roxygen comments for the following exported functions to generate `\value` statement in `.Rd` files:
  - getInitializationCode.shinyDataFilter_df.Rd
  - shiny_vector_filter_numeric_few.Rd
  - shiny_vector_filter_numeric_many.Rd

#### R CMD Check
0 errors | 0 warnings | 1 note
```
checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Aaron Clark <clark.aaronchris@gmail.com>'
  
  New submission
```

## Re-submission 2022-06-24
This is a re-submission. In this version I have:

* Added more details about the package functionality in the Description field of the DESCRIPTION file.

* Removed `@examples` section for unexported functions, which coincidentally also removed an instance where `:::` was used.

* Used `if(interactive())` and not `\dontrun` in `man/shiny_data_filter.Rd`'s `@examples` section since it is insufficient by itself.


#### R CMD Check
0 errors | 0 warnings | 1 note
```
checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Aaron Clark <clark.aaronchris@gmail.com>'
  
  New submission
```


## Re-submission 2022-06-11
This is a re-submission. In this version I have:

* Clearly identified the copyright holder in the DESCRIPTION file, and omitted the extra LICENSE file, per request,  as it was not needed for AGPL-3.
  
* Reduced the size of the package to be less than 5MB.
  
#### R CMD Check
0 errors | 0 warnings | 1 note
```
checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Aaron Clark <clark.aaronchris@gmail.com>'
  
  New submission
```
## Initial submission 2022-06-10
#### R CMD Check
0 errors | 0 warnings | 1 note

```
checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Aaron Clark <clark.aaronchris@gmail.com>'
  
  New submission
  
  License components with restrictions and base license permitting such:
    AGPL-3 + file LICENSE
  File 'LICENSE':
    YEAR: 2020
    COPYRIGHT HOLDER: Biogen;
  
  Size of tarball: 5464618 bytes
```
### Downstream dependencies

There are none.
