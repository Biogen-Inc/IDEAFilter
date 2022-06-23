## Re-submission 2022-06-23
This is a re-submission. In this version I have:

* Added more details about the package functionality in the Description field of the DESCRIPTION file.

* Omitted the `\dontrun` calls since I was already using the (sufficient)
`if(interactive())` wrap on my example(s). This impacted:
```
man/nullor.Rd
man/shiny_data_filter_ui.Rd
man/shiny_data_filter.Rd
```
* Removed examples for unexported functions.
Please either omit these examples or export these functions.

* Removed use of `:::` in documentation (by removing the unnecessary example on an unexported function):
```
man/nullor.Rd
```

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
