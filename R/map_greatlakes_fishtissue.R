#' @title Great Lakes Map
#'
#' @description Map of Great Lakes with values above and below a screening
#' value.
#'
#' @details Map of Great Lakes with two data layers.  Layer 1 are sites greater
#' than or equal to the screening value (exceedances).  Layer 2 are sites less
#' than the screening value (non-exceedances).
#'
#' # 20230719, moved code to package scripts.  Derived for Great Lakes studies.
#'
#' @param data Data frame of all values.  Default = NULL
#' @param sv Screening value. Default = NULL
#' @param xlong Data column with longitude. Default = Longitude
#' @param ylat Data column with latitude. Default = Latitude
#' @param amount Data column with analyte value. Default = amount
#' @param lab_title Title string. Default = NULL
#' @param lab_subtitle Caption string. Default = NULL
#' @param lab_caption Caption string. Default = NULL
#'
#' @return A ggplot object is returned.
#'
#' @examples
#'
#' data <- data_gl2010_map
#' sv <- 40
#' xlong <- "Longitude"
#' ylat <- "Latitude"
#' amount <- "Amount"
#' lab_title <- "Great Lakes, 2010"
#' lab_subtitle <- paste("PFOS", paste0("Screeing Value = ", sv), sep = "\n")
#' lab_caption <- NULL
#'
#' map_greatlakes_fishtissue(data = data
#'                           , sv = sv
#'                           , xlong = xlong
#'                           , ylat = ylat
#'                           , amount = amount
#'                           , lab_title = lab_title
#'                           , lab_subtitle = lab_subtitle
#'                           , lab_caption = lab_caption)
#'
#' @export
map_greatlakes_fishtissue <- function(data = NULL
                                      , sv = NULL
                                      , xlong = "Longitude"
                                      , ylat = "Latitude"
                                      , amount = "amount"
                                      , lab_title = NULL
                                      , lab_subtitle = NULL
                                      , lab_caption = NULL){

  # QC----
  ## QC, data----
  if (is.null(data)) {
    qc_param <- "data"
    msg <- paste0("Missing input parameter; ", qc_param)
    stop(msg)
  } ## IF ~ data

  ## QC, sv----
  if (is.null(sv)) {
    qc_param <- "sv"
    msg <- paste0("Missing input parameter; ", qc_param)
    stop(msg)
  } ## IF ~ sv

  ## QC, xlong----
  if (!(xlong %in% names(data))) {
    qc_param <- "xlong"
    msg <- paste0("Data is missing column; ", qc_param, " = ", xlong)
    stop(msg)
  } ## IF ~ xlong

  ## QC, ylat----
  if (!(ylat %in% names(data))) {
    qc_param <- "ylat"
    msg <- paste0("Data is missing column; ", qc_param, " = ", ylat)
    stop(msg)
  } ## IF ~ ylat

  ## QC, amount----
  if (!(amount %in% names(data))) {
    qc_param <- "amount"
    msg <- paste0("Data is missing column; ", qc_param, " = ", amount)
    stop(msg)
  } ## IF ~ amount

  # Map, base ----
  ## Data, Download
  mapdata_us <- rnaturalearth::ne_states("United States of America"
                                         , returnclass = "sf")
  mapdata_ca <- rnaturalearth::ne_states("Canada"
                                         , returnclass = "sf")
  mapdata_lakes <- rnaturalearth::ne_download(scale = "medium"
                                              , type = "lakes"
                                              , category = "physical"
                                              , returnclass = "sf")
  ## Data, Filter
  names_us_gl <- c("Minnesota", "Wisconsin", "Michigan", "Illinois", "Indiana"
                   , "Ohio", "Pennsylvania", "New York")
  names_ca_gl <- "Ontario"
  names_lakes_gl <- c(paste0("Lake "
                      , c("Erie", "Huron", "Michigan", "Ontario", "Superior")))

  mapdata_us_gl <- mapdata_us[mapdata_us$name %in% names_us_gl, ]
  mapdata_ca_gl <- mapdata_ca[mapdata_ca$name %in% names_ca_gl, ]
  mapdata_lakes_gl <- mapdata_lakes[mapdata_lakes$name %in% names_lakes_gl, ]

  ## PLOT
  default_crs = sf::st_crs(4326) #WGS84
  # bounding box for great lakes
  bbox_gl <- sf::st_bbox(mapdata_lakes_gl)

  m_base <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = mapdata_ca_gl, fill = NA) +
    ggplot2::geom_sf(data = mapdata_us_gl, fill = NA) +
    ggplot2::geom_sf(data = mapdata_lakes_gl, fill = "sky blue", alpha = 0.5) +
    ggplot2::xlim(bbox_gl[1], bbox_gl[3]) +
    ggplot2::ylim(bbox_gl[2], bbox_gl[4]) +
    ggplot2::theme_void()

  # Data, Munge ----
  data_sv_lt <- data[data[, amount] < sv, ]
  data_sv_gte <- data[data[, amount] >= sv, ]

  # Map, user ----
  map_results <- m_base +
    ggplot2::geom_point(data = data_sv_gte
               , ggplot2::aes(x = .data[[xlong]]
                              , y = .data[[ylat]]
                              , size = .data[[amount]]
                              , color = .data[[amount]])
               , alpha = 0.95
               , na.rm = TRUE) +
    ggplot2::geom_point(data = data_sv_lt
               , ggplot2::aes(x = .data[[xlong]]
                              , y = .data[[ylat]])
               , color = "black"
               , alpha = 0.50
               , na.rm = TRUE) +
    ggplot2::labs(title = lab_title
         , subtitle = lab_subtitle
         , color = paste0(amount, " (color)")
         , size = paste0(amount, " (size)")
         , caption = lab_caption) +
    #ggplot2::theme(plot.caption = element_text(hjust = 0)) + # left align
    ggplot2::scale_size_continuous() +
    ggplot2::scale_color_continuous(type = "viridis")

  # Return ----
  return(map_results)


} ## FUNCTION ~ END
