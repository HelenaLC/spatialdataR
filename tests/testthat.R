# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(SpatialData)

test_check("SpatialData")

# TODO: would be useful to have toyShape(), toyImage() etc. functions
# that mock up some data (e.g., couple points, triangle/circles etc.)

# TODO: lots and lots of unit testing...