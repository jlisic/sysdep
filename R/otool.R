


                           
# function to get list of shared objects
find_so <- function( package_name ) {

  check_pkgs <- installed.packages()

  check_pkg <- check_pkgs[which(check_pkgs[,1] == package_name ),]

  lib_path <- sprintf("%s/%s/libs", 
                    check_pkg['LibPath'],
                    check_pkg['Package'])

  check_shared <- list.files(lib_path) 

  # return nothing if there are no files in lib path
  if( length(check_shared) < 1) return( c() )

  check_shared <- paste( lib_path, check_shared[sapply(check_shared, function(x) grepl("[.](so|dylib)$",x))],sep='/')
 
 check_shared
} 

# get otool dep list
otool_dep_list <- function( shared_object) {

  # build otool cmd
  otool_cmd <- sprintf("otool -L %s", shared_object) 

  # run the otool cmd
  otool_out <- system( otool_cmd,intern=TRUE )

  # filter a by starting with a tab
  is_so <- function(x) {grepl("^\t",x) } 


  # get file paths from otool
  file_paths <- otool_out[sapply( otool_out, is_so)]
  file_paths <- file_paths[-1]
  get_paths <- function(x) gsub("\t","",gsub(" [(]comp.*[)]", "", x)) 

  # sanitize paths from otool
  so_paths <- sapply( file_paths, get_paths )
  names(so_paths) <- NULL

  so_paths
}
      

# check if a package is installed, and it's version string
homebrew_search <- function( pkg ) {

  # get list of installed packages
  a <- system( "brew list --versions",intern=TRUE )
  a2 <- t(sapply(a, function(x) {b <- strsplit(x,' '); c(b[[1]][1], b[[1]][2] ) } ))
  rownames(a2) <- NULL

  # sanitize listing 
  df2 <- data.frame(pkg_name=a2[,1], pkg_version=a2[,2],stringsAsFactors=FALSE)

  return( merge(data.frame(pkg_name=pkg,stringsAsFactors=FALSE), df2, by="pkg_name",all.x=TRUE)  )
  
}


# function to create shared object data frame from a pkg
pkg_dep <- function( pkg_names, verbose=TRUE ) {
    
  result <-c()
  for( pkg_name in pkg_names ) {

    if( verbose ) cat(sprintf("%s\n", pkg_name))
    target_sos <- find_so(pkg_name)
  
    # check if there are any dependencies
    if(length(target_sos) < 1) next 
  
    # iterate over all shared objects
    for( target_so in target_sos ) { 
      otool_dep_list( target_so )
    
      result <- rbind( result, 
        data.frame(
          r_package=pkg_name,
          operating_system=sessionInfo()$R.version$os,
          shared_object=otool_dep_list( target_so),
          shared_object_exists=FALSE,
          package_system="",
          package_version="",
          package_name="",
          stringsAsFactors=FALSE
        )
      )
    }
  }
  
  # exit early on no shared objects 
  if( length(result$shared_object) == 0) return(result) 
 
  # check if the shared objects exist  
  shared_object_exists <-sapply(result$shared_object,file.exists)

  # check if any shared objects are found
  if( sum(shared_object_exists) == 0) return(result) 

  # add T/F to the output
  result$shared_object_exists <- shared_object_exists

  # handle operating system specific package lookups 
  operating_system <- sessionInfo()$R.version$os

  # darwin and homebrew
  if( grepl("darwin", operating_system)) {
    if( verbose ) cat(sprintf("os:  %s\n", operating_system))
      
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


      # get likely homebrew packages
      homebrew_pkgs <- sapply(result[homebrew_files,"shared_object"],
        function(x) unlist(strsplit(gsub(homebrew_dir,"",x),'/'))[1])
     
#debug
homebrew_pkgs <<- homebrew_pkgs

      # append package names back to result 
      if( verbose ) cat("dependency searching...\n")
   
      # get pkg names
      homebrew_pkg_names <- homebrew_search(homebrew_pkgs)

      # record package version
      result[ homebrew_files, "package_version"] <- homebrew_pkg_names$pkg_version

      # record package name 
      result[ homebrew_files, "package_name"] <- homebrew_pkg_names$pkg_name
      # record package system
      result[nchar(result$package_version) > 0,"package_system"] <- "homebrew"     

    }

  }
  
  # linux 
  if( grepl("linux", operating_system)) {
    print('linux') 
  } 

  result
}


# check all installed r packages
check_all_packages <- FALSE
upgrade_unresolved_r_pkgs <- FALSE



# test to check all packages
if( check_all_packages) {
  check_pkgs <- installed.packages()[,1]
  shared_objects <- pkg_dep(check_pkgs) 
}



# find dependencies for a specific package
utils_dep <- pkg_dep('rgdal')


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






