#' Get identifiers for census regions interesecting a geometry
#'
#' @description
#' This function returns a list of regions that intersect a give geometry. It is useful for example when
#' one is interested in census data for a particular geographic region that does not coincide with
#' well-known census geometries. The returned value can be used as the \code{regions} parameter in \code{get_census}
#' to get corresponding census geographies and variables that cover the give geometry.
#'
#' @param dataset A CensusMapper dataset identifier.
#' @param level The census aggregation level to retrieve. One of \code{"Regions"}, \code{"PR"}, \code{"CMA"}, \code{"CD"}, \code{"CSD"}, \code{"CT"} or \code{"DA"}.
#' @param geometry An \code{sf} or \code{sfc} object
#' @param api_key An API key for the CensusMapper API. Defaults to \code{options()} and then the \code{CM_API_KEY} environment variable.
#'
#' @source Census data and boundary geographies are reproduced and distributed on
#' an "as is" basis with the permission of Statistics Canada (Statistics Canada
#' 2001; 2006; 2011; 2016).
#'
#' @export
#'
#' @examples
#' # Query the API for the census tract contain the coordinates [-123.25149, 49.27026]
#' \dontrun{
#' point_geo <- sf::st_sfc(sf::st_point(c(-123.25149, 49.27026)),crs=4326)
#' regions <- get_intersecting_geometries(dataset = 'CA16', level = 'CT', geometry = point_geo)
#' census_data <- get_census(dataset='CA16', regions=regions,
#'                           vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
#'                           level='CT')
#'
#'}
get_intersecting_geometries <- function(dataset, level, geometry, api_key=getOption("cancensus.api_key")) {
  api_key <- robust_api_key(api_key)
  if ("sf" %in% class(geometry)) {
    geometry=sf::st_geometry(geometry)
  }
  if (!("sfc" %in% class(geometry))) {
    stop("The `geometry` parameter needs to be of class sf or sfc")
  }
  if (length(geometry)>1) {
    geometry <- sf::st_union(geometry)
  }

  url <- paste0(cancensus_base_url(),"/api/v1/intersecting_geographies")
  response<-httr::POST(url,
                       body = list(dataset=dataset,
                                   level=level,
                                   geometry=geojsonsf::sfc_geojson(geometry),
                                   api_key=api_key),
                       config = httr::accept_json())

  if (response$status_code!=200) {
    message=httr::content(response,as="text")
    stop(paste("Download of Census Data failed.",
               message, sep=' '))
  }

  result <- httr::content(response, type = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
}
