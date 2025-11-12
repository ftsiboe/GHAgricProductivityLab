.onLoad <- function(libname, pkgname) {
  # set scientific notation options
  options(scipen = 999)

  # set global timeout limit
  options(timeout = 360000)

  options(future.globals.maxSize = 20 * 1024^3)  # 20 GiB

  # Register global variables used by data.table (silence R CMD check NOTES)
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      strsplit(
        " .data EaId glss stat value . TE_OLS Treat DATA",
        "\\s+"
      )[[1]]
    )
  }
}

