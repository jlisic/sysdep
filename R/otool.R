


                           
# function to get list of shared objects
# private function do not export
# only takes a single package name
find_so <- function( package_name ) {

  check_pkgs <- installed.packages()

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


###################################################
#  ldd_dep_list
#
#  description: get ldd dep list
#
#  input:  a shared object (single path)
#
#  output: list of shared objects with path
###################################################
#  Notes:
#
#  ldd output example:
#  linux-vdso.so.1 =>  (0x00007ffddd9f1000)
#  libR.so => /usr/lib/libR.so (0x00007f2cd48fc000)
#  /lib64/ld-linux-x86-64.so.2 (0x00007f2cd512d000)
###################################################
ldd_dep_list <- function( shared_object) {

  # build otool cmd
  ldd_cmd <- sprintf(" ldd %s", shared_object) 

  # run the ldd cmd
  ldd_out <- system( ldd_cmd,intern=TRUE )

  # check if there is a kernel shared object listed
  is_kern_so <- function(x) {grepl("linux-vdso.so.1",x) }
  ldd_out <- ldd_out[ !sapply(ldd_out, is_kern_so) ]

  # get file paths from ldd
  get_paths <- function(x) gsub("^.*=>[ ]*","",gsub("\t","",gsub(" [(].*[)]$", "", x)))

  # sanitize paths from ldd 
  so_paths <- sapply( ldd_out, get_paths )
  names(so_paths) <- NULL

  # make sure we aren't chasing down symlinks
  remove_symlinks <- function(x) system(sprintf("readlink -f %s",x),intern=TRUE)

  so_paths <- sapply(so_paths, remove_symlinks)

  so_paths
}



# get otool dep list
otool_dep_list <- function( shared_object) {

  # build otool cmd
  otool_cmd <- sprintf("otool -L %s", shared_object) 

  # run the otool cmd
  otool_out <- system( otool_cmd,intern=TRUE )

  # filter a by starting with a tab
  is_so <- function(x) {grepl("^\t",x) } 

  # filter out some otool errors
  is_valid_so <- function(x) {grepl(" is not an object file$",x)}

  # get file paths from otool
  file_paths <- otool_out[sapply( otool_out, is_so)]
  file_paths <- file_paths[!sapply( file_paths, is_valid_so)]
  file_paths <- file_paths[-1]
 
  if( length(file_paths) < 1) return(NULL) 
  get_paths <- function(x) gsub("\t","",gsub(" [(]comp.*[)]", "", x)) 

  # sanitize paths from otool
  so_paths <- sapply( file_paths, get_paths )
  names(so_paths) <- NULL

  so_paths
}




# check if a package is installed, and it's version string
dpkg_search <- function( shared_objects ) {

  dpkg_versions <-c()
  dpkg_names <-c()
  dpkg_sos <- c() # handle multiple packages single file

  for( shared_object in shared_objects) {
   
    dpkg_name <- NULL 
    tryCatch({
      dpkg_name <- system( sprintf("dpkg -S %s",shared_object),intern=TRUE,ignore.stderr=TRUE)
    }, error=function(x) {
    }, finally={}
    )

    if( is.null(dpkg_name) ) next

    # get list of installed packages
    dpkg_name <- sapply(dpkg_name, function(x) {b <- strsplit(x,': '); b[[1]][1]  } )
    dpkg_version <- system( sprintf(" dpkg-query --showformat='${Version}' --show %s", dpkg_name),intern=TRUE)
    
    
    names(dpkg_name) <- NULL
    names(dpkg_version) <- NULL

    dpkg_sos    <-  c(rep(shared_object,length(dpkg_name)), dpkg_sos) 
    dpkg_names <- c(dpkg_name, dpkg_names) 
    dpkg_versions <- c(dpkg_version, dpkg_versions) 
  }
  
  result <- data.frame(shared_object=dpkg_sos, pkg_name=dpkg_names, pkg_version=dpkg_versions,stringsAsFactors=FALSE)

  return( result )
}

      

# check if a package is installed, and it's version string
homebrew_search <- function( pkg ) {

  # get list of installed packages
  a <- system( "brew list --versions",intern=TRUE )
  a2 <- t(sapply(a, function(x) {b <- strsplit(x,' '); c(b[[1]][1], b[[1]][2] ) } ))
  rownames(a2) <- NULL

  # sanitize listing 
  df2 <- data.frame(package_name=a2[,1], package_version=a2[,2],stringsAsFactors=FALSE)

  return( merge(pkg , df2, by="package_name",all.x=TRUE)  )
}


# function to create shared object data frame from a pkg
pkg_dep_bin <- function( pkg_names, verbose=TRUE ) {
  
  # handle operating system specific package lookups 
  operating_system <- sessionInfo()$R.version$os

  # get version number from installed packages
  installed_packages <- installed.packages()
  
  # print OS
  if( verbose ) cat(sprintf("os:  %s\n", operating_system))
   
  # iterate through all package names 
  result <-c()
  for( pkg_name in pkg_names ) {

    r_pkg_version <- installed_packages[pkg_name ==installed_packages[,'Package'],'Version']

    if( verbose ) cat(sprintf("%s\n", pkg_name))
    target_sos <- find_so(pkg_name)
 
    # check if there are any dependencies
    if(length(target_sos) < 1) next 
  
    # iterate over all shared objects
    for( target_so in target_sos ) { 
      
      if( grepl("darwin", operating_system)) {
        dep_list=otool_dep_list( target_so)
      } else if( grepl("linux", operating_system)) {
        dep_list=ldd_dep_list( target_so)
      } else {
        stop("Unsupported Operating System")
      }

      # handle invalid so (e.g. FastRWeb)
      if( is.null(dep_list) ) next

      result <- rbind( result, 
        data.frame(
          r_package=pkg_name,
          r_package_version=r_pkg_version,
          operating_system=sessionInfo()$R.version$os,
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
      homebrew_pkg_names <- homebrew_search(homebrew_pkgs)

     
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
      dpkg_pkg_names <- dpkg_search(result$shared_object)

      # remove duplicate names
      dpkg_pkg_names <- dpkg_pkg_names[!duplicated(dpkg_pkg_names),]

      # merge shared object info on
      result <- merge( result, dpkg_pkg_names, by="shared_object")

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


# check all installed r packages
check_all_packages <-  FALSE 
upgrade_unresolved_r_pkgs <- FALSE
check_one <- TRUE 


# test to check all packages
if( check_all_packages) {
  check_pkgs <- installed.packages()[,1]
  shared_objects <- pkg_dep_bin(check_pkgs) 
  save(shared_objects,file='/tmp/shared_objects.Rd')
}



# find dependencies for a specific package
if( check_one ) {
  #utils_dep <- pkg_dep_bin('FastRWeb')
  utils_dep <- pkg_dep_bin(c( 'rgdal','sp','meanShiftR'))
}

# check need to upgrade based on missing (upgraded) packages
if( upgrade_unresolved_r_pkgs ) {
  need_to_upgrade <- 
    aggregate( 
      shared_objects$shared_object_exist, 
      by=list(shared_objects$r_package), 
      min)

  need_to_upgrade <- need_to_upgrade[ need_to_upgrade$x == 0,'Group.1']

  if( length(need_to_upgrade) > 0 ) install.packages(need_to_upgrade)
}




