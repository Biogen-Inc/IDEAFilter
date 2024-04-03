## R CMD check results
0 errors | 0 warnings | 1 note
```
Found the following (possibly) invalid URLs:
  URL: https://bit.ly/demo_IDEAFilter (moved to https://rinpharma.shinyapps.io/IDEAfilter/)
    From: README.md
    Status: 301
    Message: Moved Permanently
  For content that is 'Moved Permanently', please change http to https,
  add trailing slashes, or replace the old by the new URL.
```
This is an intentional redirect that allows the package maintainer to monitor traffic to the demo application originating from the package README

## Reverse dependency check

One reverse dependency exists (`{tidyCDISC}`) and was tested by running R CMD Check using the development version of `IDEAFilter`. The changes have no negative impact on it's reverse dependency.