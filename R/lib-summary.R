#' R Library Summary
#'
#' Provide a brief summary of the package libraries on your machine.
#'
#' @param sizes logical indicating whether or not to calculate
#' library sizes. Default `FALSE`.
#'
#' @return A two-column `data.frame` containing the count of packages
#' in each of the user's libraries.
#' @export
#'
#' @examples
#' lib_summary()
lib_summary <- function(sizes = FALSE) {
  if(!is.logical(sizes)) {
    stop("'sizes' must be TRUE or FALSE.") #stop: exits function & give error
  }

  #Tells us how many packages are in each library
  pkgs <- utils::installed.packages()
  pkg_tbl <- table(pkgs[, "LibPath"])
  pkg_df <- as.data.frame(pkg_tbl, stringsAsFactors = FALSE)
  names(pkg_df) <- c("Library", "n_packages")

  if(sizes == TRUE) {
    pkg_df$lib_size <-  sapply(
      pkg_df$Library,
      function(x) { #fs is available for us to call; not for others to use
        sum(fs::file_size(fs::dir_ls(x, recurse = TRUE))) #below we use two commands from fs package
      }
    )
  }

  pkg_df
}
