#' #' Plots SST data
#' #' 
#' #' @export
#' #' @param x sst class tibble
#' #' @param y NULL, ignored, or another tibble of class sst
#' #' @param type character see \code{\link[base]{plot}}, defaults to 'l'
#' #' @param ... other arguments passed to \code{\link[base]{plot}} 
#' #' @return NULL
#' 
#' plot.sst <- function(x, y = NULL, type = 'l', ...){
#'   plot(x$COLLECTION_DATE, x$SEA_SURFACE_TEMP_AVG_C, type = type, ...)
#' }

#' Plots SST data (multiple series)
#'
#' @export 
#' @param x sst class tibble
#' @param ... other arguments passed to \code{\link[base]{plot}} 
#' @return ggplot object

plot.sst_group <- function(x, ...){
  ggplot2::ggplot(x, ggplot2::aes(x = COLLECTION_DATE,
                  y = SEA_SURFACE_TEMP_AVG_C,
                  group = name,
                  col = name)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(aes(ymin = SEA_SURFACE_TEMP_MIN_C,
                             ymax = SEA_SURFACE_TEMP_MAX_C,
                             fill = name),
                linetype = 0,
                alpha = 0.4)
}

#' Plots SST data (single series)
#' 
#' @export
#' @param x sst class tibble
#' @param xlim date range limits, defaults to max/min
#' @param ... other arguments passed to \code{\link[base]{plot}} 
#' @return ggplot object

plot.sst <- function(x, 
                     xlim = range(x$COLLECTION_DATE, na.rm = TRUE), 
                     ...){
  if(!inherits(xlim, 'Date')){xlim <- as.Date(xlim)}
  
  ggplot2::ggplot(x |> dplyr::filter(dplyr::between(COLLECTION_DATE, xlim[1], xlim[2])), 
                  ggplot2::aes(x = COLLECTION_DATE,
                  y = SEA_SURFACE_TEMP_AVG_C)) +
    ggplot2::xlim(xlim) +
    ggplot2::geom_line(...) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = SEA_SURFACE_TEMP_MIN_C,
                                      ymax = SEA_SURFACE_TEMP_MAX_C),
                         linetype = 0,
                         alpha = 0.4,
                         ...) +
    ggplot2::labs(title = paste(attr(x, 'longname')),
                  caption = paste('Source:', attr(x, 'source')))
}