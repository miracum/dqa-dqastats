context("lints")

if (dir.exists("../../00_pkg_src")) {
  prefix <- "../../00_pkg_src/DQAstats/"
} else if (dir.exists("../../R")) {
  prefix <- "../../"
} else if (dir.exists("./R")) {
  prefix <- "./"
}

test_that(desc = "test lints",
          code = {
            lintlist <- list(
              "R" = list(
                "calc_stats.R" = NULL,
                "completeness.R" = NULL,
                "conformance.R" = "cyclomatic complexity",
                "database.R" = NULL,
                "dataloading.R" = list(
                  list(message = "cyclomatic complexity",
                       line_number = 179),
                  list(message = "cyclomatic complexity",
                       line_number = 281)
                ),
                "datamap.R" = NULL,
                "DQA.R" = "cyclomatic complexity",
                "etl.R" = NULL,
                "plausibilities.R" = "cyclomatic complexity",
                "plausibility_results.R" = "cyclomatic complexity",
                "report.R" = NULL,
                "statistics.R" = NULL,
                "utils.R" = list(
                  list(message = "snake_case",
                       line_number = 109),
                  list(message = "space before left parenthesis",
                       line_number = 111)
                )
              ),
              "tests/testthat" = list(
                "test-lints.R" = NULL,
                "test-MDR.R" = NULL,
                "test-DQA.R" = NULL
              )
            )

            for (directory in names(lintlist)) {
              print(directory)
              for (fname in names(lintlist[[directory]])) {
                print(fname)
                #% print(list.files(prefix))

                # skip on covr
                skip_on_covr()

                lintr::expect_lint(file = paste0(prefix,
                                                 directory,
                                                 "/",
                                                 fname),
                                   checks = lintlist[[directory]][[fname]])
              }
            }
          })
