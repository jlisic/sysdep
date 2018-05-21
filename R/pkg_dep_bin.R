#' Creates a list of run-time system dependencies for installed packages.
#'
#' \code{pkg_dep_bin} returns a list of run-time system dependencies based 
#' on shared objects.  System dependencies that cannot be determined through
#' shared objects are not returned.
#'
#' @param pkg_names A string array of installed packages.
#' @param verbose A logical scalar. Should additional output be provided, 
#'   (default is TRUE).
#' @return A data frame containing:
#'  
#' r_package:  R package name.
#' 
#' r_package_version:  R package version.
#' 
#' operating_system:  Current operating system.
#' 
#' shared_object:  Shared object.
#' 
#' shared_object_exists:  Logical, does the referred shared object exist.
#' 
#' package_system:  Package system, e.g. homebrew, dpkg, and rpm.
#' 
#' package_version:  Version of the package.
#' 
#' package_name:  System package name.
#' 
#' When no dependencies are found, NULL is returned. 
#' 
#' @examples
#' pkg_dep_bin(c('stats','utils'))
pkg_dep_bin <-
function( pkg_names, verbose=TRUE ) {
  
  # handle operating system specific package lookups 
  operating_system <- utils::sessionInfo()$R.version$os

  # get version number from installed packages
  installed_packages <- utils::installed.packages()
  
  # print OS
  if( verbose ) cat(sprintf("os:  %s\n", operating_system))
   
  # iterate through all package names 
  result <-c()
  for( pkg_name in pkg_names ) {

    r_pkg_version <- installed_packages[pkg_name ==installed_packages[,'Package'],'Version']

    if( verbose ) cat(sprintf("%s\n", pkg_name))
    target_sos <- sysdep::find_so(pkg_name)


    # check if there are any dependencies
    if(length(target_sos) < 1) next 
  
    # iterate over all shared objects
    for( target_so in target_sos ) { 
      
      if( grepl("darwin", operating_system)) {
        dep_list=sysdep::.otool_dep_list( target_so)
      } else if( grepl("linux", operating_system)) {
        dep_list=sysdep::.ldd_dep_list( target_so)
      } else {
        stop("Unsupported Operating System")
      }

      # handle invalid so (e.g. FastRWeb)
      if( is.null(dep_list) ) next


      result <- rbind( result, 
        data.frame(
          r_package=pkg_name,
          r_package_version=r_pkg_version,
          operating_system=utils::sessionInfo()$R.version$os,
          shared_object=dep_list,
          shared_object_exists=FALSE,
          package_system="",
          package_version="",
          package_name="",
          stringsAsFactors=FALSE
          )
        )
    }
  }

  # clear row names
  rownames(result) <- NULL

  # exit early on no shared objects 
  if( length(result$shared_object) == 0) return(result) 

  # check if the shared objects exist  
  shared_object_exists <-sapply(result$shared_object,file.exists)

  # check if any shared objects are found
  if( sum(shared_object_exists) == 0) return(result) 

  # add T/F to the output
  result$shared_object_exists <- shared_object_exists


  # darwin and homebrew
  if( grepl("darwin", operating_system)) {
      
    # check if homebrew is installed
    homebrew_out <- NULL
    tryCatch({
      homebrew_out <- system( "brew config",intern=TRUE,ignore.stderr=TRUE )
    }, error=function(x) {
    }, finally={}
    )
 
    # get homebrew dir 
    if(!is.null(homebrew_out)) {
      if( verbose ) cat("package system:  homebrew\n")
      homebrew_dir <- sprintf("%s/opt/",
       gsub("^HOMEBREW_PREFIX: ","", 
        homebrew_out[grepl("^HOMEBREW_PREFIX:",homebrew_out)]
        ))


      homebrew_files <- grepl(sprintf("^%s",homebrew_dir),result$shared_object)
      # if there is nothing to report, return 
      if( sum(homebrew_files) == 0) return(result) 

      # create a subset of homebrew packages
      homebrew_pkgs <- result[homebrew_files,c('r_package','shared_object')]

      # get likely homebrew packages
      homebrew_pkgs$package_name <- sapply(homebrew_pkgs[,"shared_object"],
        function(x) unlist(strsplit(gsub(homebrew_dir,"",x),'/'))[1])

      # append package names back to result 
      if( verbose ) cat("dependency searching...\n")

      # get pkg names
      homebrew_pkg_names <- sysdep::.homebrew_search(homebrew_pkgs)

     
      result$package_name <- NULL
      result$package_version <- NULL

      # record package version
      result <- merge( result, homebrew_pkg_names, by=c("r_package","shared_object")) 

      # record package system
      result[nchar(result$package_version) > 0,"package_system"] <- "homebrew"     

    }
  }
  
  # linux 
  if( grepl("linux", operating_system)) {
     
    # need to handle dpkg vs rpm here at some point
    # check /etc/issue and grep out Ubuntu/Debian etc... ?
    
      # append package names back to result 
      if( verbose ) cat("dependency searching...\n")
   
      # get pkg names
      dpkg_pkg_names <- sysdep::.dpkg_search(result$shared_object)

      # remove duplicate names
      dpkg_pkg_names <- dpkg_pkg_names[!duplicated(dpkg_pkg_names),]

      # merge shared object info on
      result <- merge( result, dpkg_pkg_names, by="shared_object", sort=FALSE)

      # record package version
      result[ , "package_version"] <- result$pkg_version

      # record package name 
      result[ , "package_name"] <- result$pkg_name

      result$pkg_version <- NULL
      result$pkg_name <- NULL
      # record package system
      result[nchar(result$package_version) > 0,"package_system"] <- "dpkg"     
  } 

  # make sure the order is perserved when we return
  result[,c('r_package','r_package_version', 'operating_system','shared_object',
            'shared_object_exists','package_system','package_version',
            'package_name')]
}
