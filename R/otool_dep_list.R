# private function
.otool_dep_list <-
function( shared_object) {
  
  if( length(shared_object) > 1 ) warning("Only the first shared object will be evaluated.")
  shared_object <- shared_object[1]

  # build otool cmd
  otool_cmd <- sprintf("otool -L %s", shared_object) 

  # run the otool cmd
  otool_out <- system( otool_cmd,intern=TRUE , ignore.stderr=TRUE)

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
