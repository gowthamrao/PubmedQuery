PubmedQuery
================


[![Build Status](https://github.com/OHDSI/PubmedQuery/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/PubmedQuery/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/PubmedQuery/coverage.svg?branch=main)](https://codecov.io/github/OHDSI/PubmedQuery?branch=main)

Introduction
============

THIS PACKAGE IS UNDER ACTIVE DEVELOPMENT. IT IS NOT PART OF HADES.

PubmedQuery is an R package designed to understand medication compliance and persistence in cohort studies. 

Features
========
- Calculate key adherence metrics such as medication possession ratio and proportional days covered, with and without stockpiling.
- Assess drug utilization from first exposure through user-defined persistence windows.
- Visualize adherence patterns and statistics through both tabular and graphical outputss.


Technology
============
PubmedQuery is an R package.

System Requirements
============
Requires R (version 3.6.0 or higher). 

Installation
=============
1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including RTools and Java.

2. In R, use the following commands to download and install PubmedQuery:

  ```r
  install.packages("remotes")
  remotes::install_github("ohdsi/PubmedQuery")
  ```

User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/PubmedQuery).

PDF versions of the documentation are also available:
* Package manual: [PubmedQuery.pdf](https://raw.githubusercontent.com/OHDSI/PubmedQuery/main/extras/PubmedQuery.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/PubmedQuery/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

License
=======
PubmedQuery is licensed under Apache License 2.0

Development
===========
PubmedQuery is being developed in R Studio.

### Development status

PubmedQuery is under development.
