# DQAstats NEWS

## Unreleased (2022-06-22)

#### Fixes

* fixed checking if plausibility checks are possible with subset of data elements
* json-object to character for storing in pl-list
#### Refactorings

* failing if info from mdr is lacking for plausibilities
#### Others

* updated gha 2
* updated gha
* updated lintr
* preparing next cran-release
* added demo sitename.json
* replaced "data system" with "database"
* updated description
* making json prettier
* updating description and news.md

Full set of changes: [`v0.3.0...9e3da23`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.3.0...9e3da23)

## v0.3.0 (2022-05-19)

#### New Features

* added logo
* 'no data available' now available for conformance checks
* added creation of temporal view to sql display
* more flexibility to use read_mdr
* now returning modified sql statements for pdf / gui-button
* added new constraints functionality for datetime variables
* adding functionality for datetime constraints (wip)
#### Fixes

* fixed rendering of report when datetime future checks present
* error with diztools::is.empty
* fixed displaying of conformance results, when no data available to perform conformance checks
* fixed recognition of empty data for datetime variables
* fixed missing of view creation / deletion in displayed sqls
* fixed issues when source_table_name included where-clause
* only the first sql showed the view-creation sql. This is now fixed.
* added all view creations to display-sql where only the last one was
* corrected hierarchy of outlist list to transform variables 2
* corrected hierarchy of outlist list to transform variables
* replaced wrong argument to future::plan: 'worker' to 'workers'
* fixed errors in vignette
* better error handling for parallel backend
* added on.exit for future plan
* parallel=FALSE by default
* sql statements to target system, if source=target
#### Refactorings

* formating value conformance results for reuse in dqagui
* improved detection of missing datetime data
* added 'no data available' category for etl checks
* enhanced formatting of displayed sqls
#### Docs

* replace missing definition with informatino
* removed dataquier as soft dependency; now loading datasets directly from url in vignette
* added wiki-page for gui to vignette
* fixed wrong paths in vignette
* update vignette
* adding vignette that analyzes dataquier ship-dataset
#### Others

* preparing new cran release v0.3.0
* updated description / news.md
* removed unnecessary paste
* updated version, news.md, etc.
* added linebreake after sql-where
* function create_markdown to extra file
* updated news.md
* updated news.md
* added diztools version to logging
* updated package metadata and news.md
* updated news.md
* updated news.md; fixed git config to work with auto-changelog again
* update debugging.info
* updated debugging info
* removed comments from display sql
* merged development
* extened SQL hints in display message
* updated news.md
* updated news.md
* updated news
* added feedback msg to error
* updated cran comments
* added unname after ncores
* outscourced dqagui deployment to dqagui
* added dqastats and dqagui docker-compse files
* added dqagui image deployment
* fixed typo in vignette
* updated news.md
* fixed localhost url in vignette
* removed unnecessary columns from example mdr
* removed contexts
* fixed unittest for new mdr

Full set of changes: [`v0.2.6...v0.3.0`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.2.6...v0.3.0)

## v0.2.6 (2022-03-02)

#### Fixes

* renaming data.table columns when modified by sql
#### Others

* updated news.md
* updated version; news.md
* added error message to agrepl command
* updated readme to comply with diztools/dizutils
* updated readme to comply with diztools
* removed gitlab badges from readme

Full set of changes: [`v0.2.5...v0.2.6`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.2.5...v0.2.6)

## v0.2.5 (2022-02-21)

#### Refactorings

* switch to diztools dependency
#### Others

* updated dizutils version

Full set of changes: [`v0.2.4...v0.2.5`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.2.4...v0.2.5)

## v0.2.4 (2022-02-11)

#### Others

* preparing cran update
* updated news.md, adaptions to new diztools/dizutils
* updated news.md and description
* some revisions to file path handling to avoid warnings
* updated news.md

Full set of changes: [`v0.2.3...v0.2.4`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.2.3...v0.2.4)

## v0.2.3 (2022-02-04)

#### New Features

* added datamap variables; updated tests accordingly; added unique plausi check irregularity
* updated docker image
* added demo `email.yml` file
* removed e1071 dependency; expect value_set to be array
#### Fixes

* fallback in csv path extraction to handed over variable
#### Docs

* updated documentation
* updated readme
* restructured files and docs
* updated `NEWS.md`
#### Others

* wrap examples into dont-test due to long runtime on cran
* updated rbuildignore, gitignore, news.md
* updated description and news.md
* updated version for cran submission
* updated news.md; .rbuildignore
* updated date range in license statements
* added env var content to logging msg
* re-added roxygennote
* updated url in readme
* removed print-statement from datetime_restrictions
* updated MDR and example data
* repair codecov badge
* fixed wrong formatting of markdown in readme.md badges section
* removed html comments
* reactivated cran-badges in readme.md
* deleting logfiles after each test
* adapt checks to MDR columns

Full set of changes: [`v0.2.2...v0.2.3`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.2.2...v0.2.3)

## v0.2.2 (2022-01-12)

#### Fixes

* merging origin/development
#### Refactorings

* updated K8s manifest and docs
#### Docs

* added @chgl / @christian.gulden to thanks
* updated k8s docs
#### Others

* updated news.md
* cran-submission to rbuildignore
* fix failing cran checks due to failing unit tests in rdevel

Full set of changes: [`v0.2.1...v0.2.2`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.2.1...v0.2.2)

## v0.2.1 (2022-01-04)

#### New Features

* added script to run testdata
* added kubernetes manifest
#### Fixes

* will not fail when tinytex is missing
* removed tinytex-installation from report.r due to cran policy violation
#### Docs

* cran-installation to readme
#### Others

* updated news.md
* fixed typo in rbuildignore
* updated `news.md`
* detailed error message for `tinytex`
* updated news.md
* allow news.md in .gitignore and add to .Rbuildignore
* added creation of news.md to devstuffs.R

Full set of changes: [`v0.2.0...v0.2.1`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.2.0...v0.2.1)

## v0.2.0 (2022-01-03)

#### New Features

* new possibility for time constraints using sql modifications
* added build script and docker images
* switched to rocker-verse with included latex
* added demo-dockerfile and dc
* added `run_once` function for container runs
* added `run_once.R` file for one-shot-container-runs
#### Fixes

* added `xcolor` to latex deps to avoid latex crashing in the image
* build script image tagging fixed
* moved `run_once` file to the right folder
#### Docs

* added hints for rocker/verse to readme
* updated readme
#### Others

* updated image and added renovate
* added printing of current versions to dqa function
* added tinitex dependency
* switched to dev version of miracumdqa
* fixed intendation in readme
* added miracumdqa and docs
* added tinytex packages to dockerfile
* switched to variables in docker compose
* catched fromjson error
* added logmessages
* removed `run_once()` function

Full set of changes: [`v0.1.7...v0.2.0`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.1.7...v0.2.0)

## v0.1.7 (2021-07-01)

#### New Features

* new release
* added uniqueness and atemporal plausibility checks
* integrated support for non-timestamp columns for time-filtering
* integrated temporal filtering
* added time filtering function for databases
* added time-filtering for csv files
#### Fixes

* report display if no datamap variable was analysed
* if analysis was performed without elements from the datamap no error is displayed now
* improved error handling with missing constraints
* removing undefined from mdr filtering
* renamed format_POSIXct to lowercase
* formatting error
* made demo files working
* added support for datetime_format in conformance checks
* changed default format for date constraint
* fixed bug where empty filter-cols crashed the script
* adapted to new version of `DIZutils::db_connection`
* fixed date format in db-import
#### Refactorings

* fixed gh action warnings
* improved style of the report
* updated restricting date calls
#### Docs

* added `key` docu in function `apply_time_restriciton()`
* fixed optionality of parameter in `apply_time_restriciton`
#### Others

* moved to central ci-config
* moving towards central ci-config
* test
* test
* moved to central gitlab-ci
* moved to central ci-config
* moved to central ci-configuration
* moved to decentral ci
* moved to decentral ci-config
* updated desc
* removed uncommented code
* version increment to 0.1.6.9013
* version increment
* added findme in multicore functions
* added support for custom format for time filtering csv files
* added `check_date_restriction_requirements` as external function
* fixed potential use of uncommented code
* moving forwards to temporal restrictions

Full set of changes: [`v0.1.6...v0.1.7`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.1.6...v0.1.7)

## v0.1.6 (2021-02-18)

#### Fixes

* updated dizutils version
* fixed error with missing date_format
* custom setting inputs (from the ui) are now used as db-connection parameters
#### Refactorings

* reverted changes in example mdr
* preparation for new release v0.1.6
#### Others

* run ci also for new tags
* xied ci
* update
* excluded building a new base_image for all dev-commits
* fixed ci pipeline
* fix
* updated ci for updateing the base_image in ci-run
* updated ci
* fixed ci
* removed apk
* updated harbor url
* docker login
* fixed docker login
* ci fixes for harbor login
* updated ci
* added dqa_base_image as ci-source
* switched from install.packages() to install2.r due to speed improvements
* switched from install.packages() to install2.r due to speed improvements
* switched from install.packages() to install2.r due to speed improvements
* switched from install.packages() to install2.r due to speed improvements
* switched from install.packages() to install2.r due to speed improvements
* switched from install.packages() to install2.r due to speed improvements

Full set of changes: [`v0.1.5...v0.1.6`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.1.5...v0.1.6)

## v0.1.5 (2020-05-07)


Full set of changes: [`v0.1.4...v0.1.5`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.1.4...v0.1.5)

## v0.1.4 (2020-04-28)


Full set of changes: [`v0.1.3...v0.1.4`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.1.3...v0.1.4)

## v0.1.3 (2020-04-20)


Full set of changes: [`v0.1.2...v0.1.3`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.1.2...v0.1.3)

## v0.1.2 (2020-03-20)


Full set of changes: [`v0.1.1...v0.1.2`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.1.1...v0.1.2)

## v0.1.1 (2020-03-18)


Full set of changes: [`v0.1.0...v0.1.1`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.1.0...v0.1.1)

## v0.1.0 (2020-03-16)


Full set of changes: [`v0.0.7...v0.1.0`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.0.7...v0.1.0)

## v0.0.7 (2020-02-25)


Full set of changes: [`v0.0.6...v0.0.7`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.0.6...v0.0.7)

## v0.0.6 (2020-01-29)


Full set of changes: [`v0.0.5...v0.0.6`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.0.5...v0.0.6)

## v0.0.5 (2019-12-02)


Full set of changes: [`v0.0.4...v0.0.5`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.0.4...v0.0.5)

## v0.0.4 (2019-11-15)


Full set of changes: [`v0.0.3...v0.0.4`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.0.3...v0.0.4)

## v0.0.3 (2019-11-12)


Full set of changes: [`v0.0.2...v0.0.3`](https://gitlab.miracum.org/miracum/dqa/dqastats/compare/v0.0.2...v0.0.3)

## v0.0.2 (2019-09-27)

