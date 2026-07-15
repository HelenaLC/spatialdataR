# Contributing to spatialdataR

This document gives some information on how you can contribute to
spatialdataR.

## Reporting bugs & requesting new feature

Our team has a wide variety of expertise but Spatial Biology is a vast
field and it is possible that we are not supporting your specific use
case simply because we don’t know about it.

Just hearing from you, how you use spatialdataR, and how it could be
improved, is a very valuable contribution.

If you’ve found a bug, please file an issue that illustrates the bug
with a minimal [reprex](https://www.tidyverse.org/help/#reprex) (this
will also help you write a unit test, if needed). See the tidyverse
guide on [how to create a great
issue](https://code-review.tidyverse.org/issues/) for more advice.

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the
documentation directly using the GitHub web interface, as long as the
changes are made in the *source* file. This generally means you’ll need
to edit [roxygen2
comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`,
not a `.Rd` file. You can find the `.R` file that generates the `.Rd` by
reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it’s a good idea to first file an
issue to make sure someone from the team agrees that it’s needed, and to
discuss how it interacts with other pending changes.

As a rule of thumb, please start with a brainstorming issue if you are
considering the following:

- breaking changes
- addition of a new dependency

### Pull request process

- Fork the package and clone onto your computer. If you haven’t done
  this before, we recommend using
  `usethis::create_from_github("HelenaLC/spatialdataR", fork = TRUE)`.

- Install all development dependencies with
  [`pak::local_install_dev_deps()`](https://pak.r-lib.org/reference/local_install_dev_deps.html),
  and then make sure the package passes `R CMD check` by running
  `devtools::check()`. If `R CMD check` doesn’t pass cleanly, it’s a
  good idea to ask for help before continuing.

- Create a Git branch for your pull request (PR). We recommend using
  `usethis::pr_init("brief-description-of-change")`.

- Make your changes and check that the package passes `R CMD check` by
  running `devtools::check()` again. Commit the changes to git, and then
  create a PR by running `usethis::pr_push()`, and follow the prompts in
  your browser. The title of your PR should briefly describe the change.
  The body of your PR should contain `Fixes #issue-number`.

- Summarize key changes in `NEWS.md` (i.e. just below the first header).
  See existing entries for inspiration on length, depth, and style.

### Code style

- We follow the [Bioconductor coding style
  guide](https://contributions.bioconductor.org/r-code.html#r-code-development).
  In particular, it differs from other popular style guide on the
  following points:

  - Indents are 4 spaces
  - Function and variable names use camelCase

- We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
  [Markdown
  syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html),
  for documentation.

- We use [testthat](https://cran.r-project.org/package=testthat) for
  unit tests. Contributions with test cases included are easier to
  accept.

## Code of Conduct

Please note that spatialdataR follows the [Bioconductor Code of
Conduct](https://bioconductor.github.io/bioc_coc_multilingual/). By
contributing to this project you agree to abide by its terms.
