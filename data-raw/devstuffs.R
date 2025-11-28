# nolint start
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
    email = "lorenz.kapsner@gmail.com",
    role = c("cre", "aut"),
    comment = c(ORCID = "0000-0003-1866-860X")
  ),
  person("Jonathan M.", "Mang", role = "aut",
         comment = c(ORCID = "0000-0003-0518-4710")),
  person("Helene Köster", role = "ctb"),
  person("MIRACUM - Medical Informatics in Research and Care in University Medicine", role = "fnd"),
  person("Universitätsklinikum Erlangen", role = "cph")
)) #,
#  person("Name2", "Surname2", email = "mail@2", role = 'aut')))
# Remove some author fields
my_desc$del("Maintainer")
# Set the version
my_desc$set_version("0.3.9.9002")

# The title of your package
my_desc$set(Title = "Core Functions for Data Quality Assessment")
# The description of your package
my_desc$set(
  Description = paste0(
    "Perform data quality assessment ('DQA')",
    " of electronic health records ('EHR'). Publication: ",
    "Kapsner et al. (2021) <doi:10.1055/s-0041-1733847>."
  )
)
# The description of your package
my_desc$set("Date" = as.character(Sys.Date()))
# The urls
my_desc$set("URL", "https://github.com/miracum/dqa-dqastats")
my_desc$set("BugReports",
            "https://github.com/miracum/dqa-dqastats/issues")
# Vignette Builder
my_desc$set("VignetteBuilder" = "quarto")
# Quarto
my_desc$set("SystemRequirements" = paste0(
  "Quarto command line tools ",
  "(https://github.com/quarto-dev/quarto-cli).")
)

# Testthat stuff
my_desc$set("Config/testthat/parallel" = "false")
my_desc$set("Config/testthat/edition" = "3")
# Roxygen
my_desc$set("Roxygen" = "list(markdown = TRUE)")

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
usethis::use_package("future.apply", type = "Imports")
usethis::use_package("jsonlite", type = "Imports")
usethis::use_package("kableExtra", type = "Imports")
usethis::use_package("knitr", type = "Imports")
usethis::use_package("quarto", type = "Imports", min_version = "1.4")
usethis::use_package("magrittr", type = "Imports")
usethis::use_package("parsedate", type = "Imports")
usethis::use_package("stats", type = "Imports")
usethis::use_package("tinytex", type = "Imports")
usethis::use_package("utils", type = "Imports")
usethis::use_package("DIZutils", type = "Imports", min_version = "0.1.2")
usethis::use_package("DIZtools", type = "Imports", min_version = "0.0.8")


# Suggests
usethis::use_package("testthat", type = "Suggests")
usethis::use_package("lintr", type = "Suggests")
usethis::use_package("remotes", type = "Suggests")
usethis::use_package("DT", type = "Suggests")

# define remotes
remotes_append_vector <- NULL

# Development package
tools_tag <- "cran" # e.g. "v0.1.7", "development" or "cran"
if (tools_tag == "cran") {
  install.packages("DIZtools")
} else{
  devtools::install_git(
    url = "https://gitlab.miracum.org/miracum/misc/diztools.git",
    ref = tools_tag,
    upgrade = "always",
    quiet = TRUE
  )
  # add_remotes <- paste0(
  #   "url::https://gitlab.miracum.org/miracum/misc/diztools/-/archive/", tools_tag, "/diztools-", tools_tag, ".zip"
  # )
  add_remotes <- paste0(
    "github::miracum/misc-diztools@", tools_tag
  )

  if (is.null(remotes_append_vector)) {
    remotes_append_vector <- add_remotes
  } else {
    remotes_append_vector <- c(remotes_append_vector, add_remotes)
  }
}

utils_tag <- "cran" # e.g. "v0.1.7", "development" or "cran"
if (utils_tag == "cran") {
  install.packages("DIZutils")
} else{
  devtools::install_git(
    url = "https://gitlab.miracum.org/miracum/misc/dizutils.git",
    ref = utils_tag,
    upgrade = "always",
    quiet = TRUE
  )
  # add_remotes <- paste0(
  #   "url::https://gitlab.miracum.org/miracum/misc/dizutils/-/archive/", utils_tag, "/dizutils-", utils_tag, ".zip"
  # )
  add_remotes <- paste0(
    "github::miracum/misc-dizutils@", utils_tag
  )

  if (is.null(remotes_append_vector)) {
    remotes_append_vector <- add_remotes
  } else {
    remotes_append_vector <- c(remotes_append_vector, add_remotes)
  }
}

# finally, add remotes (if required)
if (!is.null(remotes_append_vector)) {
  desc::desc_set_remotes(
    remotes_append_vector,
    file = usethis::proj_get()
  )
}


# usethis::use_dev_package("DQAstats", type = "Imports")
# https://cran.r-project.org/web/packages/devtools/vignettes/dependencies.html


## .Rbuildignore: ##
fn <- ".Rbuildignore"
if (file.exists(fn)) {
  file.remove(fn)
}
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
usethis::use_build_ignore(".env")
usethis::use_build_ignore("Dockerfile")
usethis::use_build_ignore("docker-compose.yml")
usethis::use_build_ignore("build_image.sh")
usethis::use_build_ignore("renovate.json")
usethis::use_build_ignore("NEWS.md")
usethis::use_build_ignore("README.md")
usethis::use_build_ignore("README.qmd")
usethis::use_build_ignore("docker")
usethis::use_build_ignore(".ccache")
usethis::use_build_ignore(".github")
usethis::use_build_ignore("tic.R")
usethis::use_build_ignore("ci")
usethis::use_build_ignore("CRAN-RELEASE")
usethis::use_build_ignore("CRAN-SUBMISSION")
usethis::use_build_ignore("dqastats.Rproj")
usethis::use_build_ignore("cran-comments.md")
usethis::use_build_ignore("man/figures")
usethis::use_build_ignore("Rplots.pdf")
usethis::use_build_ignore("/dqa-dqastats.wiki/")
usethis::use_build_ignore(".pre-commit-config.yaml")


## .gitignore:
fn <- ".gitignore"
if (file.exists(fn)) {
  file.remove(fn)
}
usethis::use_git_ignore("## --------------")
usethis::use_git_ignore("## This file is auto generated.")
usethis::use_git_ignore("## Please apply changes in `./data-raw/devstuffs.R`!")
usethis::use_git_ignore("## -------------")
usethis::use_git_ignore("/*")
usethis::use_git_ignore("/*/")
usethis::use_git_ignore("*.log")
usethis::use_git_ignore("inst/doc")
usethis::use_git_ignore("!/.gitignore")
usethis::use_git_ignore("!/.github")
usethis::use_git_ignore("!/data-raw/")
usethis::use_git_ignore("!/vignettes/")
usethis::use_git_ignore("!/DESCRIPTION")
usethis::use_git_ignore("!/inst/")
usethis::use_git_ignore("!/LICENSE.md")
usethis::use_git_ignore("!/man/")
usethis::use_git_ignore("!NAMESPACE")
usethis::use_git_ignore("!/R/")
usethis::use_git_ignore("!/README.md")
usethis::use_git_ignore("!/README.qmd")
usethis::use_git_ignore("!/tests/")
usethis::use_git_ignore("/tests/testthat/test_settings_use.yml")
usethis::use_git_ignore("/tests/testthat/testdata/")
usethis::use_git_ignore("/dqa-dqastats.wiki/")
usethis::use_git_ignore("/.Rhistory")
usethis::use_git_ignore("!/*.Rproj")
usethis::use_git_ignore("/.Rproj*")
usethis::use_git_ignore("/.RData")
usethis::use_git_ignore("/.vscode")
usethis::use_git_ignore("!/.lintr")
usethis::use_git_ignore("!/NEWS.md")
usethis::use_git_ignore("!/docker/")
usethis::use_git_ignore("/docker/*")
usethis::use_git_ignore("!/docker/build_image.sh")
usethis::use_git_ignore("!/docker/docker-compose.yml")
usethis::use_git_ignore("!/docker/dqastats-secret.yaml")
usethis::use_git_ignore("!/docker/dqastats-workflow.yaml")
usethis::use_git_ignore("!/docker/Dockerfile")
usethis::use_git_ignore("!/docker/README.md")
usethis::use_git_ignore("!/docker/.env")
#usethis::use_git_ignore("/inst/demo_data/utilities/MDR/.~lock.mdr_example_data.csv#")
usethis::use_git_ignore(".~lock.*.csv#")
usethis::use_git_ignore("!/manifests/")
usethis::use_git_ignore("!cran-comments.md")
usethis::use_git_ignore("!.pre-commit-config.yaml")



usethis::use_tidy_description()

quarto::quarto_render(input = "README.qmd")

# create NEWS.md using the python-package "auto-changelog" (must be installed)
# https://www.conventionalcommits.org/en/v1.0.0/
# build|ci|docs|feat|fix|perf|refactor|test

# https://github.com/gitpython-developers/GitPython/issues/1016#issuecomment-1104114129
# system(
#   command = paste0("git config --global --add safe.directory ", getwd())
# )
# system(
#   command = 'auto-changelog -u -t "DQAstats NEWS" --tag-prefix "v" -o "NEWS.md"'
# )

an <- autonewsmd::autonewsmd$new(repo_name = packagename)
an$generate()
an$write(force = TRUE)


# imgurl <- path.expand("~/development/Rpackages/bg3.jpeg")
# hexSticker::sticker(
#   subplot = imgurl,
#   package = "DQAstats",
#   s_width = 0.66,
#   s_height = 0.66,
#   s_x = 1,
#   s_y = 1,
#   p_size = 22,
#   p_x = 1,
#   p_y = 1,
#   filename = "man/figures/logo.png",
#   h_color = "#00ffd5", # "#b4f2e9",
#   p_color = "#00ffd5", # "#b4f2e9",
#   h_size = 0.8,
#   h_fill = "#baffa8",
#   spotlight = TRUE,
#   #l_width = 6,
#   #l_height = 6,
#   white_around_sticker = FALSE,
#   asp = 1
# )
# nolint end
