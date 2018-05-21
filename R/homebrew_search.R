# internal function
.homebrew_search <-
function( pkg ) {

  # get list of installed packages
  a <- system( "brew list --versions",intern=TRUE )
  a2 <- t(sapply(a, function(x) {b <- strsplit(x,' '); c(b[[1]][1], b[[1]][2] ) } ))
  rownames(a2) <- NULL

  # sanitize listing 
  df2 <- data.frame(package_name=a2[,1], package_version=a2[,2],stringsAsFactors=FALSE)

  return( merge(pkg , df2, by="package_name",all.x=TRUE)  )
}
