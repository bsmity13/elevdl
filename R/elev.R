
#' Construct Data URL
#'
#' Constructs the URL for the elevation data requested
#'
#' @param coords `[numeric(2) or data.frame]` Numeric vector of length 2 giving
#' the coordinates of a single point that falls in the desired tile or a
#' two-column data.frame giving the longitude and latitude (in that order) of
#' multiple points to be considered.
#'
#' @examples
#'
#' # Example location (near Bear Lake, UT)
#' coords1 <- c(-111.419, 41.94)
#'
#' url1 <- elev_url(coords1)
#'
#' # Example data.frame
#' coord_df <- data.frame(x = c(-111.5, -112.5),
#'                        y = c(41.5, 40.5))
#'
#' url2 <- elev_url(coord_df)
#'
#' @export
elev_url <- function(coords) {
  # Check `coords`
  if (inherits(coords, "numeric")) {
    if (!(length(coords) == 2)) {
      stop("If you pass a numeric vector to 'coords', it must be exactly ",
           "length 2.")
    }
  } else {
    if (inherits(coords, "data.frame")) {
      if (!(ncol(coords) == 2)){
        stop("If you pass a data.frame to 'coords', it must have exactly ",
             "2 columns.")
      }
    }
  }

  # USGS stores tiles in 1 x 1 degree increments
  # Name of the tile is the max N and max W (= min(long))

  # Get tile numbers
  x_num <- unique(floor(coords[[1]]))
  y_num <- unique(ceiling(coords[[2]]))

  # Get tile directions
  # Directions should typically be N/W, but check
  x_dir <- ifelse(x_num < 0, "w", "e")
  y_dir <- ifelse(y_num > 0, "n", "s")

  # Make tile names
  x_tile <- paste0(x_dir, abs(x_num))
  y_tile <- paste0(y_dir, abs(y_num))

  # Full tile string
  tile <- paste0(y_tile, x_tile)

  # Now create URL
  baseurl <- "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/"
  tile_url <- paste0(baseurl, tile, "/USGS_13_", tile, ".tif" )

  # Return
  return(tile_url)
}
