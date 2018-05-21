#' Find shared object.
#'
#' \code{find_so} returns the shared objects found within the lib directory
#' of an R package.
#'
#' This is a function that searches a specified and installed R package's 
#' lib directory for shared objects, and returns any shared objects found. 
#'
#' @param package_name A single string.  The package to search for shared
#'   objects.
#' @return An array of strings containing the shared objects found, including 
#'   paths.  If no shared objects are found or the package is not installed
#'   the function records an empty array.
#' @examples
#' find_so('utils')
#' @export
find_so <-
function( package_name ) {

  if( length(package_name) > 1 ) warning("Only the first package name will be evaluated.")

  check_pkgs <- utils::installed.packages()

  check_pkg <- check_pkgs[which(check_pkgs[,1] == package_name ),]

  lib_path <- sprintf("%s/%s/libs", 
                    check_pkg['LibPath'],
                    check_pkg['Package'])

  check_shared <- list.files(lib_path) 
  
  # return nothing if there are no files in lib path
  if( length(check_shared) < 1) return( c() )
  
  check_shared <- check_shared[sapply(check_shared, function(x) grepl("[.](so|dylib)$",x))]
  
  # return nothing if there are no files in lib path that are shared objects
  if( length(check_shared) < 1) return( c() )


  check_shared <- paste( lib_path, check_shared ,sep='/')
 
 check_shared
}
