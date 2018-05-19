
# function to get installed package dependencies
get_dev_dep <- function() { 


  dpkg_output <- tempfile()
  # get a list of installed packages
  system(sprintf("dpkg-query -W -f='${binary:Package}, ${Version}, ${Architecture}, ${binary:Summary}\n' > %s", dpkg_output))
  
  
  dpkg_installed_df <- read.csv( dpkg_output, stringsAsFactors=FALSE, header=FALSE, quote="",strip.white=TRUE) 
  
  unlink(dpkg_output)
  
  # get names, versions, arch, and descriptions 
  colnames(dpkg_installed_df) <- c("pkg_name", "pkg_ver", "pkg_arch", "pkg_description")
  
  # find likely pairings of dev and non-dev packages
  
  dev_pkg <- sapply(dpkg_installed_df$pkg_name, function(x) grepl( "-(dev|devel)$", x) )
  dev_pkg <- dev_pkg | sapply(dpkg_installed_df$pkg_name, function(x) grepl( "-(dev:|devel:)", x) )
  
  dev_pkg_names <- dpkg_installed_df$pkg_name[dev_pkg]
  nondev_pkg_names <- dpkg_installed_df$pkg_name[!dev_pkg]
  
  # get info from apt-cache show
  srcs <- c()
  depends <- c()
  for( dev_pkg_name in dev_pkg_names) {
    apt_cache_query <- sprintf("apt-cache show --no-all-versions %s",dev_pkg_name) 
    apt_cache <- system(apt_cache_query, intern=TRUE)
  
    # pick up the source and dep lines 
    src = gsub("^Source: ","",apt_cache[grepl( "^Source: ", apt_cache) ]) 
    depend = gsub("^Depends: ","",apt_cache[grepl( "^Depends: ", apt_cache) ] )
    
    # handle the case when src and depends don't exist 
    if( length(src) == 0 ) src = " " 
    if( length(depend) == 0 ) depend = " " 
  
    srcs <- c( srcs, src ) 
    depends <- c( depends, depend ) 
  
  }
  dev_pkg_df <- data.frame( pkg_name = dev_pkg_names, source=srcs, depends=depends, stringsAsFactors=FALSE)
  
  dev_pkg_df$depends <- sapply( dev_pkg_df$depends, function(x) gsub("[|]",",",gsub("[(].*[)]","",x)))
  
  # dev_pkg_df
  dev_pkg_rows <- c()
  pkg_names <- dev_pkg_df$pkg_name[grepl( ",", dev_pkg_df$depends) ]
  for( i in 1:NROW(dev_pkg_df) ) {
    dev_pkg_row <- dev_pkg_df[i,]   
  
    deps_names <- unlist(strsplit(dev_pkg_row$depends,','))
    deps <- length(deps_names)
  
    dev_pkg_rows <- rbind( dev_pkg_rows, data.frame( 
      pkg_name = rep(dev_pkg_row$pkg_name, deps), 
      source = rep(dev_pkg_row$source, deps), 
      depends  = deps_names,
      stringsAsFactors=FALSE 
      ))
  }

  return(dev_pkg_rows)
}
