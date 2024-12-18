#' Create a hexagonal grid representation of spatial polygons
#'
#' This function transforms irregular spatial polygons into a hexagonal grid representation,
#' useful for visualizing geographical patterns when dealing with polygons of vastly different sizes.
#' Small polygons are aggregated into single hexagons while large polygons can span multiple hexagons.
#' The function handles edge cases and ensures spatial relationships are maintained.
#'
#' @param sf_object An sf object containing the polygons to be transformed. Must be a valid sf object
#'        with a geometry column of type POLYGON or MULTIPOLYGON.
#' @param hex_size Size of hexagonal cells in the same units as sf_object (e.g., meters for UTM).
#'        Controls the resolution of the output grid.
#' @param min_coverage Minimum proportion (0-1) of a hex that must be covered by input geometries
#'        for it to be included. Higher values remove partial edge hexes. Default 0.2.
#' @param id_col Character string naming the column in sf_object containing unique identifiers
#'        for each polygon.
#' @param name_col Optional character string naming a column containing names or labels for
#'        the polygons. These will be preserved in the output.
#'
#' @return A list with two elements:
#'   \item{hex_sp}{An sf object containing the hexagonal grid. Each hex has attributes:
#'     \itemize{
#'       \item geom_ids: String of original polygon IDs contained in the hex
#'       \item area_tot: Total area of intersection with original polygons
#'       \item prop: Proportion of hex covered by original polygons
#'       \item names: If name_col provided, concatenated names of contained polygons
#'     }
#'   }
#'   \item{hex_links}{A data frame linking original polygon IDs to their hex assignments}
#'
#' @importFrom sf st_union st_as_sf st_make_grid st_area st_intersection st_distance st_drop_geometry
#' @importFrom dplyr filter mutate group_by ungroup summarise arrange slice select left_join pull bind_rows
#' @importFrom tibble rownames_to_column
#' @importFrom stats complete.cases
#' @importFrom rlang sym .data
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
#'
#' # Load example Swedish parish data (replace with your data)
#' parishes <- read_sf("parishes.shp") %>%
#'   mutate(parish_id = row_number(),
#'          name = parish_name)
#'
#' # Create hexagonal representation with 25km hexes
#' result <- create_hex_map(
#'   sf_object = parishes,
#'   hex_size = 25000,  # 25km in projection units
#'   min_coverage = 0.2,
#'   id_col = "parish_id",
#'   name_col = "name"
#' )
#'
#' # Plot the results
#' library(ggplot2)
#' ggplot(result$hex_sp) +
#'   geom_sf(aes(fill = geom_ids)) +
#'   theme_minimal() +
#'   theme(legend.position = "none") +
#'   ggtitle("Parish Hexagonal Representation")
#'
#' # Check which parishes are in a specific hex
#' hex_assignments <- result$hex_links %>%
#'   group_by(id) %>%
#'   summarise(parishes = paste(parish_id, collapse = ", "))
#' }
#'
#' @seealso
#' \code{\link[sf]{st_make_grid}} for the underlying hex grid creation
#'
#' @export
#'
create_hex_map <- function(sf_object,
                           hex_size = 25000,
                           min_coverage = 0.2,
                           id_col = "geom_id",
                           name_col = NULL) {

  # Validate inputs
  if (!inherits(sf_object, "sf")) {
    stop("sf_object must be an sf object")
  }

  if (min_coverage < 0 || min_coverage > 1) {
    stop("min_coverage must be between 0 and 1")
  }

  if (!id_col %in% names(sf_object)) {
    stop(paste("id_col", id_col, "not found in sf_object"))
  }

  if (!is.null(name_col) && !name_col %in% names(sf_object)) {
    stop(paste("name_col", name_col, "not found in sf_object"))
  }

  # Create base area for hex grid
  base_area <- sf_object %>%
    st_union() %>%
    st_as_sf()

  # Create hex grid
  hex_grid <- st_make_grid(base_area,
                           square = FALSE,
                           cellsize = hex_size,
                           what = "polygons")

  # Convert to sf and add ids
  hex_sf <- hex_grid[base_area] %>%
    st_as_sf() %>%
    rownames_to_column(var = "id")

  # Get base hex area for coverage calculations
  base_hex_area <- st_area(hex_sf$x[1]) %>% as.numeric()

  # Get intersections and calculate coverage
  res0 <- st_intersection(hex_sf, base_area)
  hex_coverage <- res0 %>%
    group_by(id) %>%
    summarise(
      n = n(),
      area = sum(as.numeric(st_area(.)))
    )

  # Filter hexes by minimum coverage
  valid_hexes <- hex_coverage %>%
    filter(area > (base_hex_area * min_coverage))

  hex_clean <- hex_sf %>%
    filter(id %in% valid_hexes$id)

  # Calculate intersections with input geometries
  intersections <- st_intersection(sf_object, hex_clean) %>%
    mutate(
      area = as.numeric(st_area(.)),
      full_coverage = area >= base_hex_area
    ) %>%
    rownames_to_column() %>%
    group_by(!!sym(id_col)) %>%
    mutate(
      n_hexes = n(),
      n_full = sum(full_coverage)
    ) %>%
    ungroup()

  # Process different cases
  # 1. One hex geometries
  one_hex <- intersections %>%
    filter(n_hexes == 1)

  # 2. Multi-hex with no full coverage
  other_hex <- intersections %>%
    filter(n_hexes > 1)

  no_full_hex <- other_hex %>%
    arrange(-area) %>%
    group_by(!!sym(id_col)) %>%
    filter(max(full_coverage) != 1) %>%
    slice(1) %>%
    ungroup()

  # 3. Multi-hex with full coverage
  full_hex <- other_hex %>%
    group_by(id) %>%
    filter(n() == 1) %>%
    ungroup()

  # 4. Process remaining hexes
  other_hex2 <- other_hex %>%
    filter(!id %in% unique(full_hex$id)) %>%
    filter(!id %in% unique(no_full_hex$id)) %>%
    filter(!id %in% unique(one_hex$id))

  leftover_hex <- other_hex2 %>%
    arrange(-area) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()

  # Combine processed hexes
  hex_data <- bind_rows(one_hex, full_hex, no_full_hex, leftover_hex)


  # Handle unassigned geometries
  unassigned <- sf_object %>%
    filter(!.data[[id_col]] %in% unique(hex_data[[id_col]]))

  if (nrow(unassigned) > 0) {
    # Find closest hex for unassigned geometries
    distances <- st_distance(hex_clean, unassigned)
    closest_hex <- apply(distances, 2, function(x) {
      which.min(x)[1]
    })

    unassigned$id <- hex_clean$id[closest_hex]
    hex_data <- bind_rows(hex_data, unassigned)
  }

  # Create linkage table
  hex_links <- hex_data %>%
    st_drop_geometry() %>%
    select(!!sym(id_col), id)

  # Create final hex spatial object
  hex_summary <- hex_data %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(
      geom_ids = paste(unique(!!sym(id_col)), collapse = "-"),
      area_tot = sum(area)
    ) %>%
    mutate(
      prop = area_tot/base_hex_area
    )

  if (!is.null(name_col)) {
    hex_summary <- hex_summary %>%
      mutate(
        names = hex_data %>%
          st_drop_geometry() %>%
          group_by(id) %>%
          summarise(
            names = paste(unique(!!sym(name_col)), collapse = "\n")
          ) %>%
          pull(names)
      )
  }

  final_hex <- hex_clean %>%
    left_join(hex_summary, by = "id")

  # Return results
  return(list(
    hex_sp = final_hex,
    hex_links = hex_links
  ))
}


