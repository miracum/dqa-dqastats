# installs dependencies, runs R CMD check, runs covr::codecov()
get_stage("install") %>%
  add_step(step_install_deps())
do_package_checks()
