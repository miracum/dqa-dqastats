## R CMD check results

0 errors | 0 warnings | 1 note

* The latest release introduces a vignette and fixes issues that were introduced by separating some functions from the upstream package `DIZutils` into a new package `DIZtools`.
* The note occurs due to the high update frequency during the last months, which is related to the fact described above.
* Furthermore, the default value of the argument `parallel` was set to `FALSE` for the `dqa()` function due to some issues when initializing the parallel backend on windows machines.
* We have now entered a more 'stable' development state and plan less frequent updates in the future.
