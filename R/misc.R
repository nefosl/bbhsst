#' Row-binds SST data
#' 
#' @export
#' @param ... one or more sst class tibble passed to \code{\link[base]{rbind}} 
#' @return a tibble of class sst_group

rbind.sst <- function(...){
  ## make list of sst tibbles
  x <- list(...)
  ## harvest the attributes from individual tibbles
  a <- lapply(x, 
              function(y){
                dplyr::tibble(name = attr(y, 'name'),
                              longname = attr(y, 'longname'),
                              source = attr(y, 'source')) 
              })|>
    dplyr::bind_rows()
  ## add names
  names(x) <- a$name
  ## bind tibbles
  x <- lapply(names(x),
              function(n){
                y <- dplyr::mutate(x[[n]], name = n, .before = 1)
                attr(y, 'name') <- NULL
                attr(y, 'longname') <- NULL
                attr(y, 'source') <- NULL
                return(y)
              }) |>
    dplyr::bind_rows()
  
  attr(x, 'info') <- a
  class(x) <- c('sst_group', class(x))
  return(x)
}