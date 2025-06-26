# Helper function to create a dummy project structure
create_dummy_project <- function(path = ".") {
  # Create all directories first
  dir.create(file.path(path, "R"), showWarnings = FALSE)
  dir.create(file.path(path, "man"), showWarnings = FALSE) # <<< FIX: Move this up

  # Now write the files
  writeLines("Package: mytestpkg", file.path(path, "DESCRIPTION"))
  writeLines("a <- 1\nb <- 2", file.path(path, "R/script.R"))
  writeLines("# My Test Project", file.path(path, "README.md"))
  writeLines("*.Rproj", file.path(path, ".gitignore"))

  # A file that should be ignored by default
  writeLines("This is a man page.", file.path(path, "man/man.Rd"))

  # A binary file that should be skipped
  writeBin(as.raw(c(0x00, 0x01, 0x00, 0x02)), file.path(path, "data.bin"))
}
