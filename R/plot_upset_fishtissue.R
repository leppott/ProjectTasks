#' @title Plot Fish Study Upset
#'
#' @description Upset plot for fish study projects
#'
#' @details Upset plot with different options for fish tissue studies (NLA,
#' NRSA, and Great Lakes).
#'
#' Plots use the UpSetR package.
#'
#' Saves plots to a user defined folder.
#'
#' # 20230413, moved code to package scripts.  Derived for Great Lakes studies.
#'
#' @param data Data as data frame (variables are 1 or 0 as columns)
#' @param title_str Title string. Default = NA
#' @param title_x Title x placement.  Default = 0.2
#' @param title_y Title y placment. Default = 0.9
#' @param title_fontsize Title font size.  Default = 16
#' @param cap_str Caption string. Default = NA
#' @param cap_x Caption x placement. Default = 0.5
#' @param cap_y Caption y placement. Default = 0.025
#' @param cap_fontsize Caption font size. Default = 8
#'
#' @return A plot object is returned.
#'
#' @examples
#'
#' @export
plot_upset_fishtissue <- function(data
                                , title_str = NA
                                , title_x = 0.2
                                , title_y = 0.9
                                , title_fontsize = 16
                                , cap_str = NA
                                , cap_x = 0.5
                                , cap_y = 0.025
                                , cap_fontsize = 8
                                ) {

  # UpSetR
  # https://upset.app/

  #https://krassowski.github.io/complex-upset/articles/Examples_R.html
  # Complex Upset adds ggplot to UpSetR

  plot_type <- "ComplexUpset"
  plot_type <- tolower(plot_type)

  if (plot_type == "upsetr") {
    ## UpSetR ----
    ### Plot ----
    UpSetR::upset(data
                  , text.scale = c(1.3, 1.3, 1.1, 1.1, 1.2, 1.2)
                  , line.size = 1.1
                  , point.size = 2.1
                  # , mainbar.y.label = "Number of Intersecting Samples"
                  , set_size.show = TRUE
                  , set_size.scale_max = nrow(data) * 1.1
                  , order.by = "freq"
                  , nsets = ncol(data) - 1)

    # text.scale:
    # c(intersection size title
    #  , intersection size tick labels
    #  , set size title
    #  , set size tick labels
    #  , set names
    #  , numbers above bars)

    ### Title ----
    grid::grid.text(title_str
                    , x = title_x
                    , y = title_y
                    , gp = grid::gpar(fontsize = title_fontsize))

    ### Caption ----
    grid::grid.text(cap_str
                    , x = 0.5
                    , y = 0.025
                    , gp = grid::gpar(fontsize = cap_fontsize))

  } else if (plot_type == "complexupset") {
    ## ComplexUpset----
    ### Plot ----
    ComplexUpset::upset(data
                        , intersect = names(data)[-1]) +
      ggplot2::labs(title = title_str
                    , caption = cap_str)

    # to get n on set sizes plot
    # https://github.com/krassowski/complex-upset/issues/24


    library(ComplexUpset)
    library(ggplot2)

    movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"),
                                           header = T, sep = ";")
    genres <- names(movies)[3:19]

    upset(
      movies, genres,
      min_size = 10,
      set_sizes = upset_set_size(
        geom = function(...) {
          list(
            geom_bar(...),
            geom_text(..., aes(label = ..count..))
          )
        },
        stat = 'count'
      )
    )





  }## IF ~ plot_type


  # Return ----
  print(p)

} ## FUNCTION ~ END
