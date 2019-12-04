#' Function to cache long operations
#' 
#' @param name Name of the file to create
#' @param code Expression of the code to execute and cache
#' @param dir Name of directory for cache to be placed in the working directory
#' @param createdir Logical about whether to create the directory if it does not exist
#' @param clearcache Recalculate the cache for this object
#' @return Output of code, either freshly executed if the file exists or from the cache file
#' @export
cacheit <- function(name, code, dir = "cache", createdir = FALSE, clearcache = FALSE) {
  if(createdir & !dir.exists(dir)) dir.create(dir)
  if(!dir.exists(dir)) stop("Cache directory must exist, set createdir to TRUE to create")
  fn <- paste0(getwd(), "/", dir, "/", name,".rds")
  if(clearcache & file.exists(fn)) file.remove(fn)
  if(!file.exists(fn)) {
    assign(name, eval(code))
    saveRDS(get(name), fn)
  } else {
    assign(name, readRDS(fn))
  }
  get(name)
}
