packagename <- "DQAstats"

# remove existing description object
unlink("DESCRIPTION")
# Create a new description object
my_desc <- desc::description$new("!new")
# Set your package name
my_desc$set("Package", packagename)
# Set author names
my_desc$set_authors(c(
  person(
    "Lorenz A.",
    "Kapsner",
    email = "lorenz.kapsner@uk-erlangen.de",
    role = c("cre", "aut"),
    comment = c(ORCID = "0000-0003-1866-860X")
  ),
  person("Jonathan M.", "Mang", role = "aut",
         comment = c(ORCID = "0000-0003-0518-4710")),
  person("MIRACUM - Medical Informatics in Research and Care in University Medicine", role = "fnd"),
  person("Universitätsklinikum Erlangen", role = "cph")
)) #,
#  person("Name2", "Surname2", email = "mail@2", role = 'aut')))
# Remove some author fields
my_desc$del("Maintainer")
# Set the version
my_desc$set_version("0.1.5.9006")
# The title of your package
my_desc$set(Title = "DQAstats - Core Functions for Data Quality Assessment")
# The description of your package
my_desc$set(
  Description = paste0(
    "Perform data quality assessment (DQA)",
    " of electronic health records (EHR)."
  )
)
# The description of your package
my_desc$set("Date" = as.character(Sys.Date()))
# The urls
my_desc$set("URL", "https://github.com/miracum/dqa-dqastats")
my_desc$set("BugReports",
            "https://github.com/miracum/dqa-dqastats/issues")
# License
my_desc$set("License", "GPL-3")
# Save everyting
my_desc$write(file = "DESCRIPTION")

# License
#usethis::use_gpl3_license(name = "Universitätsklinikum Erlangen")


# add Imports and Depends
# Listing a package in either Depends or Imports ensures that it’s installed when needed
# Imports just loads the package, Depends attaches it
# Loading will load code, data and any DLLs; register S3 and S4 methods; and run the .onLoad() function.
##      After loading, the package is available in memory, but because it’s not in the search path,
##      you won’t be able to access its components without using ::.
##      Confusingly, :: will also load a package automatically if it isn’t already loaded.
##      It’s rare to load a package explicitly, but you can do so with requireNamespace() or loadNamespace().
# Attaching puts the package in the search path. You can’t attach a package without first loading it,
##      so both library() or require() load then attach the package.
##      You can see the currently attached packages with search().


# Depends

# Imports
usethis::use_package("data.table", type = "Imports")
usethis::use_package("shiny", type = "Imports")
usethis::use_package("magrittr", type = "Imports")
usethis::use_package("stats", type = "Imports")
usethis::use_package("jsonlite", type = "Imports")
usethis::use_package("RPostgres", type = "Imports")
usethis::use_package("e1071", type = "Imports")
usethis::use_package("knitr", type = "Imports")
usethis::use_package("rmarkdown", type = "Imports")
usethis::use_package("tinytex", type = "Imports")
usethis::use_package("kableExtra", type = "Imports")
usethis::use_package("utils", type = "Imports")
usethis::use_package("DIZutils", type = "Imports")
usethis::use_package("future.apply", type = "Imports")
usethis::use_package("future", type = "Imports")

# Suggests
usethis::use_package("testthat", type = "Suggests")
usethis::use_package("lintr", type = "Suggests")

## .Rbuildignore: ##
usethis::use_build_ignore("docker")
usethis::use_build_ignore("_settings")
usethis::use_build_ignore("tests/testthat/testdata")
usethis::use_build_ignore("_header")
usethis::use_build_ignore("data-raw")
usethis::use_build_ignore("LICENSE.md")
usethis::use_build_ignore(".gitlab-ci.yml")
usethis::use_build_ignore("DQA_report.pdf")
usethis::use_build_ignore("DQA_report.md")
usethis::use_build_ignore("DQA_report.log")
usethis::use_build_ignore("DQA_report.aux")
usethis::use_build_ignore("DQA_report.tex")
usethis::use_build_ignore("DQA_report.toc")
usethis::use_build_ignore("DQA_report.out")
usethis::use_build_ignore("output")
usethis::use_build_ignore(".log")
usethis::use_build_ignore(".vscode")
usethis::use_build_ignore(".lintr")

## .gitignore:
usethis::use_git_ignore("/*")
usethis::use_git_ignore("/*/")
usethis::use_git_ignore("*.log")
usethis::use_git_ignore("!/.gitignore")
usethis::use_git_ignore("!/data-raw/")
usethis::use_git_ignore("!/DESCRIPTION")
usethis::use_git_ignore("!/inst/")
usethis::use_git_ignore("!/LICENSE.md")
usethis::use_git_ignore("!/man/")
usethis::use_git_ignore("!NAMESPACE")
usethis::use_git_ignore("!/R/")
usethis::use_git_ignore("!/README.md")
usethis::use_git_ignore("!/tests/")
usethis::use_git_ignore("/tests/testthat/test_settings_use.yml")
usethis::use_git_ignore("/tests/testthat/testdata/")
usethis::use_git_ignore("/.Rhistory")
usethis::use_git_ignore("!/*.Rproj")
usethis::use_git_ignore("/.Rproj*")
usethis::use_git_ignore("/.RData")
usethis::use_git_ignore("/.vscode")
usethis::use_git_ignore("!/.lintr")
#usethis::use_git_ignore("/inst/demo_data/utilities/MDR/.~lock.mdr_example_data.csv#")
usethis::use_git_ignore(".~lock.*.csv#")


usethis::use_tidy_description()
