#' @title Plot Fish Study CDF
#'
#' @description CDF for fish study projects
#'
#' @details Plots CDF with different options for fish tissue studies (NLA,
#' NRSA, and Great Lakes).
#'
#' Plots are base R.
#'
#' Saves plots to a user defined folder.
#'
#' uses the Hmisc and plotrix packages
#'
#' BREAKING CHANGE
#' 2023-10-04
#' Added 5th Screening Value
#' If didn't specify parameters and assumed an order will break code
#'
#' # CDF plotting function for Fish Tissue project
#' # 20161005, Erik.Leppo@tetratech.com
#' # move function to its own script
#' # 20200401, edits
#' ## Screening Value to "Human Health Benchmark", leg_SV_str
#' ## add ng/g to parenthetical, leg_SV_units, adds if present
#' # 20210826, Add legend label update
#' # 20220322, Legend, "Data" to "Fillet Concentration Data"
#' # 20220414, move to package scripts, was fun.plot.CDF.fish
#'
#' # Derived from NRSA version (more advanced with Log scale than GL version)
#'
#' @param fun.DF Data data frame
#' @param fun.Main plot(main)
#' @param fun.xlim.x plot(xlim), value 1
#' @param fun.xlim.y plot(xlim), value 2
#' @param fun.xlab plot(xlab)
#' @param fun.ylab plot(ylab)
#' @param fun.axis.4.at axis 4 (right), at
#' @param fun.axis.4.labels axis 4 (right), labels
#' @param fun.axis.4.mtext axis 4 (right), margin text label
#' @param fun.SV.plot Screening Value 1, boolean value to plot SV
#' @param fun.SV Screening Value 1, numeric value
#' @param fun.SV.int Screening Value 1, y-intercept value
#' @param fun.SV.lab Screening Value 1, legend (source)
#' @param fun.SV2.plot Screening Value 2, boolean value to plot SV
#' @param fun.SV2 Screening Value 2, numeric value
#' @param fun.SV2.int Screening Value 2, y-intercept value
#' @param fun.SV2.lab Screening Value 2, legend (source)
#' @param fun.SV3.plot Screening Value 3, boolean value to plot SV
#' @param fun.SV3 Screening Value 3, numeric value
#' @param fun.SV3.int Screening Value 3, y-intercept value
#' @param fun.SV3.lab Screening Value 3, legend (source)
#' @param fun.SV4.plot Screening Value 4, boolean value to plot SV
#' @param fun.SV4 Screening Value 4, numeric value
#' @param fun.SV4.int Screening Value 4, y-intercept value
#' @param fun.SV4.lab Screening Value 4, legend (source)
#' @param fun.SV5.plot Screening Value 4, boolean value to plot SV
#' @param fun.SV5 Screening Value 4, numeric value
#' @param fun.SV5.int Screening Value 4, y-intercept value
#' @param fun.SV5.lab Screening Value 4, legend (source)
#' @param fun.break.boo Should x-axis include a break. Default = FALSE
#' @param fun.break.max.at . Default = NA
#' @param fun.break.pos location of break. Default = NA
#' @param fun.break.axis.at break. Default = NA
#' @param fun.break.axis.labels new x-axis labels with break
#' @param leg_SV_str Legend screening value, SV text.  Default = "Screening Value"
#' @param leg_SV_units Legend screening value, units.  Default = NA
#' @param fun.axis.2.mtext axis 2 (left) margin text label. Default = NA
#' @param fun.xlog Boolean value if x-axis should be log10 transformed.  Default = FALSE
#'
#' @return CDF plot customized for Fish Tissue project.
#'
#' @examples
#' # non at this time
#'
#' @export
plot_cdf_fishtissue <- function(fun.DF
                                , fun.Main
                                , fun.xlim.x
                                , fun.xlim.y
                                , fun.xlab
                                , fun.ylab
                                , fun.axis.4.at
                                , fun.axis.4.labels
                                , fun.axis.4.mtext
                                , fun.SV.plot
                                , fun.SV
                                , fun.SV.int
                                , fun.SV.lab
                                , fun.SV2.plot
                                , fun.SV2
                                , fun.SV2.int
                                , fun.SV2.lab
                                , fun.SV3.plot
                                , fun.SV3
                                , fun.SV3.int
                                , fun.SV3.lab
                                , fun.SV4.plot
                                , fun.SV4
                                , fun.SV4.int
                                , fun.SV4.lab
                                , fun.SV5.plot
                                , fun.SV5
                                , fun.SV5.int
                                , fun.SV5.lab
                                , fun.break.boo = FALSE
                                , fun.break.max.at = NA
                                , fun.break.max.text = NA
                                , fun.break.pos = NA
                                , fun.break.axis.at = NA
                                , fun.break.axis.labels = NA
                                , leg_SV_str = "Screening Value"
                                , leg_SV_units = NA
                                , fun.axis.2.mtext = NA
                                , fun.xlog = FALSE
                                ) {
  #browser()
  # define some values ----
  lwd.data <- 2.5
  lwd.SV <- 1.5
  lwd.CI <- lwd.SV
  col.data <- "blue"
  col.SV <- "green"
  col.CI <- "dark red"
  lty.data <- "solid"
  lty.SV <- "dashed"
  lty.CI <- "dotted"
  mtext.line <- 3 # 20210827, added for axis 2 and 4 mtext
  mtext.cex <- 1.2
  # create a graph with Value, ensure values are sorted; format graph and send to file
  plot.val.max <- max(fun.DF$Value)
  #plot.ylim.max <- 101
  #plot.xlim.min <-   # use formula based on max(fun.myDF$Value)
  par(oma = c(0, 0, 0, 3))
  par(xaxs = "i", yaxs = "i", cex.main = 1.4, cex.lab = 1.2)
  #

  #  browser()
  # log10_x, mod data ----
  if (fun.xlog == TRUE) {
    # Munge data for plot
    #fun.DF$Value <- log10(fun.DF$Value)
    # SV segments values
    # SV.val.seg <- log10(fun.SV)
    # SV2.val.seg <- log10(fun.SV2)
    # SV3.val.seg <- log10(fun.SV3)
    # SV4.val.seg <- log10(fun.SV4)
    seg.h.x0 <- 0.001
    seg.v.y0 <- -10
  } else {
    # SV.val.seg <- fun.SV
    # SV2.val.seg <- fun.SV2
    # SV3.val.seg <- fun.SV3
    # SV4.val.seg <- fun.SV4
    seg.h.x0 <- 0
    seg.v.y0 <- 0
  }## IF ~ boo_log10_x

  # Segment values
  SV.val.seg <- fun.SV
  SV2.val.seg <- fun.SV2
  SV3.val.seg <- fun.SV3
  SV4.val.seg <- fun.SV4
  SV5.val.seg <- fun.SV5

  # Legend SV value
  SV.val.leg <- fun.SV
  SV2.val.leg <- fun.SV2
  SV3.val.leg <- fun.SV3
  SV4.val.seg <- fun.SV4
  SV5.val.seg <- fun.SV5


  # Plot ----
  # without axes so can manipulate for BREAK
  if (fun.break.boo == TRUE) {##IF.fun.break.boo.START
    ## plot, break, TRUE ----
    plot(fun.DF$Estimate.P~sort(fun.DF$Value)
         , ylim = c(0,101)
         , xlim = c(fun.xlim.x, 1.01 * (fun.break.max.at))
         , type = "l"
         , col = col.data
         , lwd = lwd.data
         , lty = lty.data
         , main = fun.Main
         , xlab = fun.xlab
         , ylab = fun.ylab
         , axes = FALSE)
    #
    grid(col = "lightgray", lty = "dotted", lwd = 0.5)
    box()
    axis(1, at = fun.break.axis.at, label = fun.break.axis.labels)
    plotrix::axis.break(1, fun.break.pos, style = "zigzag")
  } else {
    ## plot, break, FALSE ----
    if (fun.xlog == TRUE) {
      ## plot, log10----
      plot(fun.DF$Estimate.P~sort(fun.DF$Value)
           , ylim = c(0,101)
           #, xlim = c(fun.xlim.x, 1.01 * (fun.xlim.y))
           , type = "l"
           , col = col.data
           , lwd = lwd.data
           , lty = lty.data
           , main = fun.Main
           , xlab = fun.xlab
           , ylab = fun.ylab
           , axes = FALSE
           , log = "x")
      ### Axis 1 (bottom) ----
      # idea
      # https://stackoverflow.com/questions/6897243/labelling-logarithmic-scale-display-in-r
      axis_log10_at <- outer(c((1:9)/100, (1:9)/10, 1:9), 10^(0:4))
      axis_log10_lab <- ifelse(log10(axis_log10_at) %% 1 == 0, axis_log10_at, NA)
      # Ticks, Minor
      axis(1, at = axis_log10_at, labels = axis_log10_lab, tck = -0.01)
      # Ticks, Major
      axis(1, at = axis_log10_lab, labels = NA, tck = -0.025)
    } else {
      # plot, base ----
      plot(fun.DF$Estimate.P~sort(fun.DF$Value)
           , ylim = c(0,101)
           , xlim = c(fun.xlim.x,1.01 * (fun.xlim.y))
           , type = "l"
           , col = col.data
           , lwd = lwd.data
           , lty = lty.data
           , main = fun.Main
           , xlab = fun.xlab
           , ylab = fun.ylab
           , axes = FALSE)
      ### Axis 1 (bottom) ----
      axis(1)
    }## IF ~ boo_log10_x ~ 2
    grid(col = "lightgray", lty = "dotted", lwd = 0.5)
    box()
  }##IF.fun.break.boo.START

  # # Log X
  # plot(fun.DF$Estimate.P~sort(fun.DF$Value)
  #      , ylim=c(0,101), log = "x"
  #      , type="l", col=col.data, lwd=lwd.data, lty=lty.data
  #      , main=fun.Main, xlab=fun.xlab, ylab=fun.ylab, axes=FALSE)
  # grid(col="lightgray",lty="dotted",lwd=0.5)
  # xlog_ticks_at <- axTicks(1)
  # # xlog_ticks_lab <- sapply(xlog_ticks_at, function(i)
  # #                         as.expression(bquote(10^ .(i)))
  # #                         )
  # xlog_ticks_lab <- format(xlog_ticks_at, scientific = FALSE, trim = TRUE)
  # axis(1, at = xlog_ticks_at, labels = xlog_ticks_lab)
  #
  ## used log="x" inside plot() so above not needed.

  box() # covers up grid lines

  # (moved grid and box inside IF/THEN)

  # Axis 2 (left) ----
  axis(2)
  # Add minor tickmarks (1 = no tick, ratio = compared to major ticks)
  Hmisc::minor.tick(nx = 1, ny = 4, tick.ratio = 0.33)

  # Axis 4 (right) ----
  axis(4, at = fun.axis.4.at, labels = fun.axis.4.labels)

  # 95% confidence intervals ----
  lines(fun.DF$UCB95Pct.P ~ sort(fun.DF$Value)
        , col = col.CI
        , lty = lty.CI
        , lwd = lwd.CI)
  lines(fun.DF$LCB95Pct.P ~ sort(fun.DF$Value)
        , col = col.CI
        , lty = lty.CI
        , lwd = lwd.CI)

  # Screening Value Lines (vertical & horizontal) ----
  mySV.num <- 0
  #seg.x0 defined under log10_x
  # SV1 - 20110713, add back in, only plot if "fun.SV.plot=TRUE".
  if (fun.SV.plot == TRUE) { ## IF.START
    mySV.num <- 1
    segments(SV.val.seg
             , seg.v.y0
             , SV.val.seg
             , fun.SV.int
             , col = col.SV
             , lwd = lwd.SV
             , lty = lty.SV)
    segments(seg.h.x0
             , fun.SV.int
             , SV.val.seg
             , fun.SV.int
             , col = col.SV
             , lwd = lwd.SV
             , lty = lty.SV)
  } ## IF.END
  # SV2 - 20161004, extra SV plot
  if (fun.SV2.plot == TRUE) { ## IF.START
    mySV.num <- 2
    segments(SV2.val.seg
             , seg.v.y0
             , SV2.val.seg
             , fun.SV2.int
             , col = col.SV
             , lwd = lwd.SV
             , lty = lty.SV)
    segments(seg.h.x0
             , fun.SV2.int
             , SV2.val.seg
             , fun.SV2.int
             , col = col.SV
             , lwd = lwd.SV
             , lty = lty.SV)
  } ## IF.END
  # SV3 - 20161006, extra SV plot
  if (fun.SV3.plot == TRUE) { ## IF.START
    mySV.num <- 3
    segments(SV3.val.seg
             , seg.v.y0
             , SV3.val.seg
             , fun.SV3.int
             , col = col.SV
             , lwd = lwd.SV
             , lty = lty.SV)
    segments(seg.h.x0
             , fun.SV3.int
             , SV3.val.seg
             , fun.SV3.int
             , col = col.SV
             , lwd = lwd.SV
             , lty = lty.SV)
  } ## IF.END
  # SV4 - 20161006, extra SV plot
  if (fun.SV4.plot == TRUE) { ## IF.START
    mySV.num <- 4
    segments(SV4.val.seg
             , seg.v.y0
             , SV4.val.seg
             , fun.SV4.int
             , col = col.SV
             , lwd = lwd.SV
             , lty = lty.SV)
    segments(seg.h.x0
             , fun.SV4.int
             , SV4.val.seg
             , fun.SV4.int
             , col = col.SV
             , lwd = lwd.SV
             , lty = lty.SV)
  } ## IF.END
  # SV5 - 20231004, extra SV plot
  if (fun.SV5.plot == TRUE) {
    mySV.num <- 5
    segments(SV5.val.seg
             , seg.v.y0
             , SV5.val.seg
             , fun.SV5.int
             , col = col.SV
             , lwd = lwd.SV
             , lty = lty.SV)
    segments(seg.h.x0
             , fun.SV5.int
             , SV5.val.seg
             , fun.SV5.int
             , col = col.SV
             , lwd = lwd.SV
             , lty = lty.SV)
  } ## IF.END
  #
  # Legend ----
  # (EWL, 20120830, added SV legend back (inside of SV IF/THEN)
  # highest number will take priority
  # 20200401, add variable for legend text and units

  if (!is.na(leg_SV_units)) {
    fun.SV  <- paste0(fun.SV,  " ", leg_SV_units)
    fun.SV2 <- paste0(fun.SV2, " ", leg_SV_units)
    fun.SV3 <- paste0(fun.SV3, " ", leg_SV_units)
    fun.SV4 <- paste0(fun.SV4, " ", leg_SV_units)
    fun.SV5 <- paste0(fun.SV5, " ", leg_SV_units)
  }## IF ~ is.na(leg_SV_units) ~ END

  # Modify legend label
  fun.SV.lab  <- ifelse(is.na(fun.SV.lab), "", paste0(fun.SV.lab, ", "))
  fun.SV2.lab <- ifelse(is.na(fun.SV2.lab), "", paste0(fun.SV2.lab, ", "))
  fun.SV3.lab <- ifelse(is.na(fun.SV3.lab), "", paste0(fun.SV3.lab, ", "))
  fun.SV4.lab <- ifelse(is.na(fun.SV4.lab), "", paste0(fun.SV4.lab, ", "))
  fun.SV5.lab <- ifelse(is.na(fun.SV5.lab), "", paste0(fun.SV5.lab, ", "))


  if (mySV.num == 1) { ##IF.START
    legend("bottomright"
           , lty = c(lty.data, lty.SV, lty.CI)
           , lwd = c(lwd.data, lwd.SV, lwd.CI)
           , col = c(col.data, col.SV, col.CI)
           , legend = c("Fillet Concentration Data"
                     , paste(leg_SV_str, " (", fun.SV.lab, fun.SV, ")", sep = "")
                     , "95% Confidence Intervals")
           , bg = "white")
  } else if (mySV.num == 2) {
    legend("bottomright"
           , lty = c(lty.data, rep(lty.SV, mySV.num), lty.CI)
           , lwd = c(lwd.data, rep(lwd.SV, mySV.num), lwd.CI)
           , col = c(col.data, rep(col.SV, mySV.num), col.CI)
           , legend = c("Fillet Concentration Data"
                     , paste(leg_SV_str, " (", fun.SV.lab, fun.SV, ")", sep = "")
                     , paste(leg_SV_str, " (", fun.SV2.lab, fun.SV2, ")", sep = "")
                     , "95% Confidence Intervals")
           , bg = "white")

  } else if (mySV.num == 3) {
    legend("bottomright"
           , lty = c(lty.data,rep(lty.SV, mySV.num), lty.CI)
           , lwd = c(lwd.data,rep(lwd.SV, mySV.num), lwd.CI)
           , col = c(col.data,rep(col.SV, mySV.num), col.CI)
           , legend = c("Fillet Concentration Data"
                     , paste(leg_SV_str, " (", fun.SV.lab, fun.SV, ")", sep = "")
                     , paste(leg_SV_str, " (", fun.SV2.lab, fun.SV2, ")", sep = "")
                     , paste(leg_SV_str, " (", fun.SV3.lab, fun.SV3, ")", sep = "")
                     , "95% Confidence Intervals")
           ,bg = "white")
  } else if (mySV.num == 4) {
    legend("bottomright"
           , lty = c(lty.data,rep(lty.SV, mySV.num), lty.CI)
           , lwd = c(lwd.data,rep(lwd.SV, mySV.num), lwd.CI)
           , col = c(col.data,rep(col.SV, mySV.num), col.CI)
           , legend = c("Fillet Concentration Data"
                     , paste(leg_SV_str, " (", fun.SV.lab, fun.SV, ")", sep = "")
                     , paste(leg_SV_str, " (", fun.SV2.lab, fun.SV2, ")", sep = "")
                     , paste(leg_SV_str, " (", fun.SV3.lab, fun.SV3, ")", sep = "")
                     , paste(leg_SV_str, " (", fun.SV4.lab, fun.SV4, ")", sep = "")
                     , "95% Confidence Intervals")
           , bg = "white")
  } else if (mySV.num == 5) {
    legend("bottomright"
           , lty = c(lty.data,rep(lty.SV, mySV.num), lty.CI)
           , lwd = c(lwd.data,rep(lwd.SV, mySV.num), lwd.CI)
           , col = c(col.data,rep(col.SV, mySV.num), col.CI)
           , legend = c("Fillet Concentration Data"
                        , paste(leg_SV_str, " (", fun.SV.lab, fun.SV, ")", sep = "")
                        , paste(leg_SV_str, " (", fun.SV2.lab, fun.SV2, ")", sep = "")
                        , paste(leg_SV_str, " (", fun.SV3.lab, fun.SV3, ")", sep = "")
                        , paste(leg_SV_str, " (", fun.SV4.lab, fun.SV4, ")", sep = "")
                        , paste(leg_SV_str, " (", fun.SV5.lab, fun.SV5, ")", sep = "")
                        , "95% Confidence Intervals")
           , bg = "white")
  } else {
    legend("bottomright"
           , lty = c(lty.data,lty.CI)
           , lwd = c(lwd.data,lwd.CI)
           , col = c(col.data,col.CI)
           , legend = c("Fillet Concentration Data"
                     , "95% Confidence Intervals")
           , bg = "white")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~
  # Break Symbol ----
  # Break (after legend so goes on top)
  if (fun.break.boo == TRUE) {##IF.fun.break.boo.START
    plotrix::axis.break(1, fun.break.pos, style = "zigzag")
  }

  # Axis 2 and 4 Titles ----
  par(new = TRUE)
  # different title based on projet (20161004)
  mtext(fun.axis.4.mtext, side = 4, line = mtext.line, cex = mtext.cex)
  # different yaxis (20210827), when use superscript have to use mtext instead
  if (fun.ylab == "") {
    mtext(fun.axis.2.mtext, side = 2, line = mtext.line - 0.5, cex = mtext.cex)
  }## IF ~ !is.na(fun.axis.2.mtext) ~ END
  #
  # Turn off new ----
  par(new = FALSE)
} ## FUNCTION ~ END
