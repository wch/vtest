# Visual testing for R

vtest provides a platform for testing graphical output from R packages. It is made up of three main parts:

1.  Functions that you run to construct your visual tests suite:
    `vcontext()`, `save_vtest()`, `end_vcontext()`

2.  `vtest()` to run a suite of visual tests and save the results locally

3.  Functions to compare runs of tests and display differences.

Currently vtest only works with ggplot2.
