# internal function
.ldd_dep_list <-
function( shared_object) {
  
  if( length(shared_object) > 1 ) warning("Only the first shared object will be evaluated.")
  shared_object <- shared_object[1]

  # build otool cmd
  ldd_cmd <- sprintf(" ldd %s", shared_object) 

  # run the ldd cmd
  suppressWarnings( 
  ldd_out <- system( ldd_cmd,intern=TRUE ,ignore.stderr=TRUE)
  )
  # check if it is a shared object
  if( sum(grepl("not a dynamic executable",ldd_out)) > 0 ) return(NULL)

  # check if there is a kernel shared object listed
  is_kern_so <- function(x) {grepl("(linux-vdso.so.1|statically link)",x) }
  ldd_out <- ldd_out[ !sapply(ldd_out, is_kern_so) ]

  if(length(ldd_out) < 1) return(NULL)

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
