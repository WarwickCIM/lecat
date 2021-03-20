
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LE-CAT

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![codecov](https://codecov.io/gh/WarwickCIM/lecat/branch/master/graph/badge.svg?token=RtMnQjfEcZ)](https://codecov.io/gh/WarwickCIM/lecat)
[![R-CMD-check](https://github.com/WarwickCIM/lecat/workflows/R-CMD-check/badge.svg)](https://github.com/WarwickCIM/lecat/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/161813229.svg)](https://zenodo.org/badge/latestdoi/161813229)

LE-CAT is a Lexicon-based Categorization and Analysis Tool developed by
the Centre for Interdisciplinary Methodologies in collaboration with the
Media of Cooperation Group at the University of Siegen.

The tool allows you to apply a set of word queries associated with a
category (a lexicon) to a data set of textual sources (the corpus).
LE-CAT determines the frequency of occurrence for each query and
category in the corpus, as well as the relations between categories
(co-occurrence) by source.

The purpose of this technique is to automate and scale up user-led data
analysis as it allows the application of a custom-built Lexicon to large
data sets. The quick iteration of analysis allows the user to refine a
corpus and deeply analyse a given phenomenon.

LE-CAT was coded by [James Tripp](https://jamestripp.github.io). It has
been used to support the workshops Youtube as Test Society (University
of Siegen), Parking on Twitter (University of Warwick) and the Digital
Test of the News (University of Warwick) and is part of the
[CIM](https://warwick.ac.uk/cim) module [Digital Object, Digital
Methods](https://warwick.ac.uk/fac/cross_fac/cim/apply-to-study/cross-disciplinary-postgraduate-modules/im904-digital-objects-digital-methods/).

Academic correspondence should be sent to [Noortje
Marres](mailto:N.Marres@warwick.ac.uk).

## Installation

You can install the released version of lecat from
[Github](https://github.com/) by running, in R, the following line of
code:

``` r
install.packages("devtools")
devtools::install_github("warwickcim/lecat")
```

## Web based interface

LE-CAT has a web interface which can be started by running

``` r
lecat::run_app()
```

which starts a new [shiny app](https://shiny.rstudio.com).

## Bugs or feature requests

Please enter any bugs or feature requests via github.

[Dr James Tripp](https://jamestripp.github.io), Senior Academic
Technologist, [CIM](https://www.warwick.ac.uk/cim)
