
<!-- README.md is generated from README.Rmd. Please edit that file -->

# collatR 📦

<!-- badges: start -->

[![R-CMD-check](https://github.com/eogrady21/collatR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/eogrady21/collatR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `collatR` is to make it easy to share the entire context of
a code repository with a Large Language Model (LLM). It scans a project,
grabs all the important source files, and bundles them into a single,
formatted text file that can be copied to your clipboard.

This is incredibly useful for:

- **Debugging:** Paste your entire R package’s source code so the LLM
  can find bugs.
- **Refactoring:** Ask for suggestions on how to improve your code
  structure.
- **Adding Features:** Give the LLM the full context before asking it to
  write new functions.
- **Documentation:** Have the LLM write documentation based on all your
  functions and examples.

## Installation

You can install the development version of `collatR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eogrady21/collatR")
```

## Core Usage

The package has two main functions depending on your source.

### 1. Collate from a Local Directory

If you are working inside an RStudio project or have the code on your
machine, use `collate_from_path()`.

``` r
library(collator)

# Run this from the root of your R project
# The entire codebase will be copied to your clipboard!
collate_from_path()
```

### 2. Collate from a GitHub Repository

To analyze a public package on GitHub, use `collate_from_github()`. The
package will clone the repo to a temporary location, collate it, and
copy the result to your clipboard.

``` r
# Get the source for this package itself!
collate_from_github("eogrady21/collatR")
```

## Customization

You can customize the output by saving to a file, or changing which
files are included.

``` r
# Save the collated code to a file instead of the clipboard
collate_from_path(
  output_file = "my_project_context.txt",
  copy_to_clipboard = FALSE
)

# Use custom patterns to only include .R files from the R/ directory
collate_from_path(
  include_patterns = c("\\.R$"),
  exclude_patterns = c("tests/") # Exclude the tests directory
)
```
