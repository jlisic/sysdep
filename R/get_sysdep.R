#' Get system dependencies from online database.
#'
#' \code{get_sysdep} returns R package system dependencies using an online
#' database.
#'
#' This is a function that searches an online database for R system 
#' dependencies.  This database includes Makevar specifications.
#'
#' @param r_pkg_name A single string.  The package to search for in the 
#'   dependency database.
#' @param r_pkg_version A single string.  The package version to search for 
#'   in the dependency database. (default is the most current version)
#' @param os_name A single string.  The operating system name to search for 
#'   in the dependency database.  (default is the current operating system)
#' @param os_detail A single string.  The operating system version name to 
#'   search for in the dependency database.  (default is the current 
#'   operating system version)
#' @param r_version A single string.  The R version to search for in the
#'   dependency database.  (default is the current R version)
#' @param sys_pkg_system A single string.  The package system to search for 
#'  in the dependency database.  (default is based on current operating 
#'  system)
#' @param arch A single string.  The processor architecture to search for 
#'   in the dependency database.  (default is the current processor 
#'   architecture)
#' @param url A single string.  The URL for the online database.
#' @return A data frame containing:
#'  
#' r_pkg_name:  R package name.
#' 
#' r_pkg_version:  R package version.
#' 
#' os_name:  Operating system.
#' 
#' os_detail:  Operating system version.
#' 
#' r_version:  R version.
#' 
#' sys_pkg_system:  Package system, e.g. homebrew, dpkg, and rpm.
#' 
#' arch:  Processor architecture of system.
#' 
#' url:  URL of online database.
#' 
#' When no dependencies are found, NULL is returned. 
#' 
#' @examples
#' \dontrun{
#' get_sysdep('rgdal')
#' }
#' @export
get_sysdep <-
function(
  r_pkg_name,
  r_pkg_version = "",
  os_name,
  os_detail,
  r_version,
  sys_pkg_system,
  arch,
  url="http://10.0.1.70/index.php"
  ) {

  if( length( r_pkg_name ) != 1 ) stop( "Sorry, get_sysdep only supports one package at a time.")

  if( missing(os_name) ) os_name   <- utils::sessionInfo()$R.version$os
  if( missing(os_detail) ) os_detail <- utils::sessionInfo()$running
  os_version <- ""
  if( missing(r_version) ) r_version <- paste0( utils::sessionInfo()$R.version$major, '.', utils::sessionInfo()$R.version$minor, collapse="")
  if( missing( arch ) ) arch <- utils::sessionInfo()$R.version$arch

  if( missing(sys_pkg_system) ) {

    if( grepl('darwin', os_name) ) {
      sys_pkg_system <- 'homebrew'
    } else if( grepl('linux', os_name) ) {
      sys_pkg_system <- 'dpkg'
    } else {
      stop(sprintf('Error: Unknown Operating System %s.',os_name))
    }
    
  }

  read_in <- sprintf("%s?r_pkg_name=%s&r_pkg_version=%s&os_name=%s&os_detail=%s&os_version=%s&r_version=%s&sys_pkg_system=%s&arch=%s",
    url,
    utils::URLencode(r_pkg_name),
    utils::URLencode(r_pkg_version),
    utils::URLencode(os_name),
    utils::URLencode(os_detail),
    utils::URLencode(os_version),
    utils::URLencode(r_version),
    utils::URLencode(sys_pkg_system),
    utils::URLencode(arch)
    )

  tmp <- tempfile()

  utils::download.file( read_in, destfile=tmp, quiet=TRUE) 

  result <- utils::read.csv(tmp,stringsAsFactors=FALSE) 
  colnames(result) <- c('r_pkg_name','r_pkg_version','os_name','os_version','os_detail','r_version', 'sys_pkg_system','arch','sys_pkg_name','sys_pkg_version','build_time','type','success','notes')
  unlink(tmp)
  return(result)

}
