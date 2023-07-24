#' @title Plot Fish Study Upset
#'
#' @description Upset plot for fish study projects
#'
#' @details Upset plot using the `ComplexUpset` package.
#'
#' Upset plots show co-occurrence of different variables.  The left side sizes
#' plot is turned on by default.  This creates extra area in the upper left
#' that can be used for an alternative location for the plot title.  The
#' function is set up to use the basic title as default but has input parameters
#' to change the location (e.g., upper left).
#'
#' A ggplot object is returned that the user can further modify and save.
#' #'
#' # 2023-07-20, moved code to package scripts.
#' Derived for Great Lakes studies.
#'
#' @param data Data as data frame (variables are 1 or 0 as columns)
#' @param title_str Title string. Default = NA
#' @param title_custloc Use a custom location for the title? Default = FALSE
#' @param title_x Title x placement.  Default = 0.2
#' @param title_y Title y placement. Default = 0.9
#' @param title_fontsize Title font size.  Default = 16
#' @param cap_str Caption string. Default = NA
#' @param cap_custloc Use a custom location for the caption? Default = FALSE
#' @param cap_x Caption x placement. Default = 0.5
#' @param cap_y Caption y placement. Default = 0.025
#' @param cap_fontsize Caption font size. Default = 8
#' @param ... Arguments to be passed on to ComplexUpset::upset
#'
#' @return A ggplot object is returned.
#'
#' @examples
#' # Data
#' df_data <- data_gl2010_upset
#'
#' # Parameters ----
#' namedata <- "Great Lakes, 2010"
#' fishconsumer <- "general"
#' sv_hg <- 300
#' sv_pfas <- 0.52
#' sv_pcb <- 12
#'
#' # Plot Parts ----
#' title_str <- paste0(namedata
#'                     , "\n(n = ", nrow(df_data), ")"
#'                     , "\nExceeds Screening Value")
#' cap_str <- paste0("Screening Values for "
#'                   , fishconsumer
#'                   , " fish consumer; "
#'                   , "Mercury = "
#'                   , sv_hg
#'                   , ", "
#'                   , "PFAS = "
#'                   , sv_pfas
#'                   , ", "
#'                   , "Total PCBs = "
#'                   , sv_pcb
#'                   )
#'
#' # Plot, title and caption, base location ----
#' p1 <- plot_upset_fishtissue(df_data = df_data
#'                            , title_str = title_str
#'                            , cap_str = cap_str)
#' p1
#'
#' # Plot, title and caption, custom location ----
#' p2 <- plot_upset_fishtissue(df_data = df_data
#'                            , title_str = title_str
#'                            , title_custloc = TRUE
#'                            , title_x = 0.16
#'                            , title_y = 0.90
#'                            , title_fontsize = 16
#'                            , cap_str = cap_str
#'                            , cap_custloc = TRUE
#'                            , cap_x = 0.5
#'                            , cap_y = 0.025
#'                            , cap_fontsize = 8)
#' p2
#'
#' @export
plot_upset_fishtissue <- function(df_data
                                , title_str = NA
                                , title_custloc = FALSE
                                , title_x = 0.2
                                , title_y = 0.9
                                , title_fontsize = 16
                                , cap_str = NA
                                , cap_custloc = FALSE
                                , cap_x = 0.5
                                , cap_y = 0.025
                                , cap_fontsize = 8
                                , ...) {

  # UpSetR
  # https://upset.app/

  #https://krassowski.github.io/complex-upset/articles/Examples_R.html
  # Complex Upset adds ggplot to UpSetR

  plot_type <- "ComplexUpset"
  plot_type <- tolower(plot_type)

  if (plot_type == "upsetr") {
    # ## UpSetR ----
    # ### Plot ----
    # UpSetR::upset(df_data
    #               , text.scale = c(1.3, 1.3, 1.1, 1.1, 1.2, 1.2)
    #               , line.size = 1.1
    #               , point.size = 2.1
    #               # , mainbar.y.label = "Number of Intersecting Samples"
    #               , set_size.show = TRUE
    #               , set_size.scale_max = nrow(df_data) * 1.1
    #               , order.by = "freq"
    #               , nsets = ncol(df_data) - 1)
    #
    # # text.scale:
    # # c(intersection size title
    # #  , intersection size tick labels
    # #  , set size title
    # #  , set size tick labels
    # #  , set names
    # #  , numbers above bars)
    #
    # ### Title ----
    # grid::grid.text(title_str
    #                 , x = title_x
    #                 , y = title_y
    #                 , gp = grid::gpar(fontsize = title_fontsize))
    #
    # ### Caption ----
    # grid::grid.text(cap_str
    #                 , x = 0.5
    #                 , y = 0.025
    #                 , gp = grid::gpar(fontsize = cap_fontsize))

  } else if (plot_type == "complexupset") {
    ## ComplexUpset----
    ### Plot ----
    p <- ComplexUpset::upset(df_data
                        , intersect = names(df_data)[-1]
                        , set_sizes = (ComplexUpset::upset_set_size() +
                              ggplot2::geom_text(ggplot2::aes(label =
                                                    ggplot2::after_stat(count))
                                        , hjust = 1.1
                                        , stat = 'count')
                              + ggplot2::expand_limits(y = nrow(df_data) * 1.15)
                                      )
                        , ...)
    #### Title ----
    if (title_custloc == TRUE) {
      p <- p +
        grid::grid.text(title_str
                        , x = title_x
                        , y = title_y
                        , gp = grid::gpar(fontsize = title_fontsize))
    } else {
      p <- p +
        ggplot2::labs(title = title_str)
    }## IF ~ title_custloc

    #### Caption ----
    if (cap_custloc == TRUE) {
      p <- p +
        grid::grid.text(cap_str
                        , x = cap_x
                        , y = cap_y
                        , gp = grid::gpar(fontsize = cap_fontsize))
    } else {
      p <- p +
        ggplot2::labs(caption = cap_str)
    }## IF ~ title_custloc

    # NOTES ----
    # to get n on set sizes plot
    # https://github.com/krassowski/complex-upset/issues/24
    #
    # https://krassowski.github.io/complex-upset/articles/Examples_R.html
    # about 50% down

  }## IF ~ plot_type


  # Return ----
  print(p)

} ## FUNCTION ~ END
