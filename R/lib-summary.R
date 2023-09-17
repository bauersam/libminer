#' R Library Summary
#'
#' Provide a brief summary of the package libraries on your machine.
#'
#' @return A two-column `data.frame` containing the count of packages
#' in each of the user's libraries.
#' @export
#'
#' @examples
#' lib_summary()
lib_summary <- function() {
  #Tells us how many packages are in each library
  pkgs <- utils::installed.packages()
  pkg_tbl <- table(pkgs[, "LibPath"])
  pkg_df <- as.data.frame(pkg_tbl, stringsAsFactors = FALSE)
  names(pkg_df) <- c("Library", "n_packages")
  pkg_df
}
