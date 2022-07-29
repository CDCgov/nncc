#' Function to cache long operations
#'
#' Save results from code that takes a long time to execute to a .rds file
#' if that file does not exist in the cache directory. If the file exists in the
#' cache directory, that file will be loaded to memory without evaluating the code.
#'
#' For more information, please refer to the vignette using
#' \code{browseVignettes("nncc")}.
#'
#' @param name Name of the file to create without extension
#' @param code Expression of the code to execute and cache
#' @param dir Name of cache directory which should be placed in the working directory
#' @param createdir Logical about whether to create the directory if it does not
#'   exist
#' @param clearcache Logical about whether to recalculate the cached .rds file
#'   for this object
#' @return Output of code, either freshly executed if the file does not exist or
#'   or clearcache is TRUE otherwise returns result from the cache file
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
