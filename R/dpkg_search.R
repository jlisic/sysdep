# dpkg search
.dpkg_search <-
function( shared_objects ) {

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
