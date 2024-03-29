#' Check spatialvalid object
#' 
#' Test if its argument is a spatialvalid object
#' 
#' @param x the object to be tested
#' @return returns \code{TRUE} if its argument is a spatialvalid
#' @keywords Check
#' @export


is.spatialvalid <- function(x) {
  inherits(x, "spatialvalid")
}


#' Plot Method for Class Spatialvalid
#' 
#' A set of plots to explore objects of the class \code{spatialvalid}. A plot
#' to visualize the flags from clean_coordinates
#' 
#' 
#' @param x an object of the class \code{spatialvalid} as from
#' \code{\link{clean_coordinates}}.
#' @param lon character string. The column with the longitude coordinates.
#' Default = \dQuote{decimalLongitude}.
#' @param lat character string. The column with the latitude coordinates.
#' Default = \dQuote{decimalLatitude}.
#' @param bgmap an object of the class \code{SpatVector} or \code{sf} used as
#' background map. Default = ggplot::borders()
#' @param clean logical.  If TRUE, non-flagged coordinates are included in the
#' map.
#' @param details logical. If TRUE, occurrences are color-coded by the type of
#' flag.
#' @param pts_size numeric. The point size for the plot.
#' @param font_size numeric. The font size for the legend and axes
#' @param zoom_f numeric. the fraction by which to expand the plotting area 
#' from the occurrence records. Increase, if countries do not show 
#' up on the background map.
#' @param \dots arguments to be passed to methods.
#' @return A plot of the records flagged as potentially erroneous by
#' \code{\link{clean_coordinates}}.
#' @seealso \code{\link{clean_coordinates}}
#' @keywords Visualisation
#' @examples
#' 
#' 
#' exmpl <- data.frame(species = sample(letters, size = 250, replace = TRUE),
#'                     decimalLongitude = runif(250, min = 42, max = 51),
#'                     decimalLatitude = runif(250, min = -26, max = -11))
#' 
#' test <- clean_coordinates(exmpl, species = "species", 
#'                           tests = c("sea", "gbif", "zeros"),
#'                           verbose = FALSE)
#' 
#' summary(test)
#' plot(test)
#' @export
#' @method plot spatialvalid
#' @importFrom grDevices extendrange
#' @importFrom ggplot2 borders fortify aes_string geom_polygon coord_fixed map_data theme_bw theme element_text geom_point scale_colour_manual scale_shape_manual element_blank
plot.spatialvalid <- function(x, 
                              lon = "decimalLongitude",
                              lat = "decimalLatitude",
                              bgmap = NULL, 
                              clean = TRUE, 
                              details = FALSE,
                              pts_size = 1, 
                              font_size = 10,
                              zoom_f = 0.1,
                              ...) {
  x <- data.frame(x)

  # prepare background
  if (is.null(bgmap)) {
    plo <- ggplot2::ggplot() + 
      ggplot2::borders(fill = "grey60", 
                       xlim = extendrange(r = range(x[lon]), f = 0.1), 
                       ylim = extendrange(r = range(x[lat]), f = 0.1)) +
      ggplot2::coord_fixed() +
      ggplot2::theme_bw()
    
  } else {
    bgmap <- sf::st_as_sf(bgmap)
    # bgmap <- suppressWarnings(ggplot2::fortify(bgmap))
    
    plo <- ggplot2::ggplot() + 
      ggplot2::geom_sf(data = bgmap, 
                       fill = "grey60") + 
      ggplot2::theme_bw()
    
  }

  # identify failed tests and create flags column, 
  # if multiple failed, first in order
  inv <- x[, grep("^\\.", names(x))]
  
  # Sometimes names from gbif have multiple dots
  inv <- inv[, sapply(inv, "is.logical"), drop = FALSE]

  flgs <- names(inv)[unlist(lapply(apply(inv != 1, 1, "which"), "[", 1), 
                            use.names = FALSE)]
  flgs <- gsub("\\.", "", flgs)

  if (sum(!is.na(flgs)) == 0) {
    flgs <- rep("AAAclean", nrow(x))
  } else {
    flgs[is.na(flgs)] <- "AAAclean"
  }
  
  x$flag <- flgs

  if (!"AAAclean" %in% x$flag) {
    clean <- FALSE
    warnings("All records were flagged, setting clean to FALSE")
  }

  # add points to background
  if (!clean & !details) {
    pts <- x[!x$.summary, ]
    plo <- plo + 
      ggplot2::geom_point(data = pts, ggplot2::aes_string(
        x = lon,
        y = lat), 
        colour = "#FDE725FF", 
        size = pts_size) + 
      ggplot2::theme(axis.title = ggplot2::element_text(size = font_size))
  }

  if (clean & !details) {
    pts <- x
    if (all(pts$.summary)) {
      test_all <- 2
    } else {
      test_all <- 1:2
    }
    plo <- plo + 
      ggplot2::geom_point(data = pts, 
                          ggplot2::aes_string(
                            x = lon,
                            y = lat, 
                            colour = ".summary"), 
                          size = pts_size) + 
      ggplot2::scale_colour_manual(values = c("#440154FF", "#FDE725FF")[test_all], 
                                   labels = c("Flagged", "Clean")[test_all]) + 
      ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = font_size), 
      legend.text = ggplot2::element_text(size = font_size)
    )
  }

  if (!clean & details) {
    pts <- x[!x$.summary, ]
    plo <- plo + 
      ggplot2::geom_point(data = pts, 
                          ggplot2::aes_string(
                            x = lon,
                            y = lat, 
                            colour = "flag"), 
                          size = pts_size) +
      #viridis::scale_colour_viridis(discrete = T) + 
      ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = font_size), 
      legend.text = ggplot2::element_text(size = font_size)
    )
  }

  if (clean & details) {
    pts <- x
    plo <- plo + ggplot2::geom_point(data = pts, 
                                     ggplot2::aes_string(
                                       x = lon,
                                       y = lat, 
                                       shape = "flag", 
                                       colour = "flag"), 
                                     size = pts_size) +
      ggplot2::scale_colour_manual(
        values = c("#00BFC4", 
                   rep("#F8766D", length(unique(pts$flag)))),
        breaks = sort(as.character(unique(pts$flag))),
        labels = c("clean", sort(as.character(unique(pts$flag)))[-1])) +
      ggplot2::scale_shape_manual(
        values = c(16, seq(15, 15 + (length(unique(pts$flag)) - 1))), 
        breaks = sort(as.character(unique(pts$flag))), 
        labels = c("clean", sort(as.character(unique(pts$flag)))[-1])) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        axis.title = ggplot2::element_text(size = font_size), 
        legend.text = ggplot2::element_text(size = font_size)
      )
  }
  plo
}

#' @export
#' @method summary spatialvalid
#' @importFrom tidyselect starts_with
#' @importFrom dplyr select
summary.spatialvalid <- function(object, ...) {
  out <- dplyr::select(object, tidyselect::starts_with("."))
  out <- apply(out, 2, "!")
  out <- apply(out, 2, "sum")
  return(out)
}
