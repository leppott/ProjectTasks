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
#' @param fun.DF x
#' @param fun.Main x
#' @param fun.xlim.x x
#' @param fun.xlim.y x
#' @param fun.xlab x
#' @param fun.ylab x
#' @param fun.axis.4.at x
#' @param fun.axis.4.labels x
#' @param fun.axis.4.mtext x
#' @param fun.SV.plot x
#' @param fun.SV x
#' @param fun.SV.int x
#' @param fun.SV.lab x
#' @param fun.SV2.plot x
#' @param fun.SV2 x
#' @param fun.SV2.int x
#' @param fun.SV2.lab x
#' @param fun.SV3.plot x
#' @param fun.SV3 x
#' @param fun.SV3.int x
#' @param fun.SV3.lab x
#' @param fun.SV4.plot x
#' @param fun.SV4 x
#' @param fun.SV4.int x
#' @param fun.SV4.lab x
#' @param fun.break.boo x
#' @param fun.break.max.at x
#' @param fun.break.max.text x
#' @param fun.break.pos x
#' @param fun.break.axis.at x
#' @param fun.break.axis.labels x
#' @param leg_SV_str x
#' @param leg_SV_units x
#' @param fun.axis.2.mtext x
#' @param fun.xlog x
#'
#' @return
#'
#' @examples
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
) { ## FUNCTION.START
  # define some values
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
  par(oma=c(0,0,0,3))
  par(xaxs="i",yaxs="i",cex.main=1.4,cex.lab=1.2)
  #
  # Plot without axes
  # (so can manipulate for BREAK)
  if(fun.break.boo==TRUE){##IF.fun.break.boo.START
    plot(fun.DF$Estimate.P~sort(fun.DF$Value)
         , ylim=c(0,101)
         , xlim=c(fun.xlim.x,1.01*(fun.break.max.at))
         , type="l"
         , col=col.data
         , lwd=lwd.data
         , lty=lty.data
         , main=fun.Main
         , xlab=fun.xlab
         , ylab=fun.ylab
         , axes=FALSE)
    #
    grid(col="lightgray",lty="dotted",lwd=0.5)
    box()
    axis(1,at=fun.break.axis.at,label=fun.break.axis.labels)
    axis.break(1,fun.break.pos,style="zigzag")
  } else {
    if(fun.xlog == TRUE) {
      plot(fun.DF$Estimate.P~sort(fun.DF$Value)
           , ylim=c(0,101)
           #, xlim=c(fun.xlim.x,1.01*(fun.xlim.y))
           , type="l"
           , col=col.data
           , lwd=lwd.data
           , lty=lty.data
           , main=fun.Main
           , xlab=fun.xlab
           , ylab=fun.ylab
           , axes=FALSE
           , log = "x")
    } else {
      plot(fun.DF$Estimate.P~sort(fun.DF$Value)
           , ylim=c(0,101)
           , xlim=c(fun.xlim.x,1.01*(fun.xlim.y))
           , type="l"
           , col=col.data
           , lwd=lwd.data
           , lty=lty.data
           , main=fun.Main
           , xlab=fun.xlab
           , ylab=fun.ylab
           , axes=FALSE)
    }## IF ~ fun.xlog

    grid(col="lightgray",lty="dotted",lwd=0.5)
    axis(1)
    box()
  }##IF.fun.break.boo.START
  #
  # add axis 2 (moved grid and box inside IF/THEN)
  axis(2)
  #
  # Screening Value Lines (vertical & horizontal)
  mySV.num <- 0
  # SV1 - 20110713, add back in, only plot if "fun.SV.plot=TRUE".
  if(fun.SV.plot==TRUE) {
    mySV.num <- 1
    segments(fun.SV,0,fun.SV,fun.SV.int,col=col.SV,lwd=lwd.SV,lty=lty.SV)
    if(fun.xlog == TRUE) {
      segments(.1,fun.SV.int,fun.SV,fun.SV.int,col=col.SV,lwd=lwd.SV,lty=lty.SV)
    } else {
      segments(0,fun.SV.int,fun.SV,fun.SV.int,col=col.SV,lwd=lwd.SV,lty=lty.SV)
    }## IF ~ fun.xlog ~ SV1
  } ## IF ~ SV1
  # SV2 - 20161004, extra SV plot
  if(SV2.plot==TRUE) {
    mySV.num <- 2
    segments(fun.SV2,0,fun.SV2,fun.SV2.int,col=col.SV,lwd=lwd.SV,lty=lty.SV)
    if(fun.xlog == TRUE) {
      segments(.1,fun.SV2.int,fun.SV2,fun.SV2.int,col=col.SV,lwd=lwd.SV,lty=lty.SV)
    } else {
      segments(0,fun.SV2.int,fun.SV2,fun.SV2.int,col=col.SV,lwd=lwd.SV,lty=lty.SV)
    }## IF ~ fun.xlog ~ SV2
  } ## IF ~ SV2
  # SV3 - 20161006, extra SV plot
  if(SV3.plot==TRUE) {
    mySV.num <- 3
    segments(fun.SV3,0,fun.SV3,fun.SV3.int,col=col.SV,lwd=lwd.SV,lty=lty.SV)
    if(fun.xlog == TRUE) {
      segments(.1,fun.SV3.int,fun.SV3,fun.SV3.int,col=col.SV,lwd=lwd.SV,lty=lty.SV)
    } else {
      segments(0,fun.SV3.int,fun.SV3,fun.SV3.int,col=col.SV,lwd=lwd.SV,lty=lty.SV)
    }## IF ~ fun.xlog ~ SV3
  } ## IF ~ SV3
  # SV4 - 20161006, extra SV plot
  if(SV4.plot==TRUE) {
    mySV.num <- 4
    segments(fun.SV4,0,fun.SV4,fun.SV4.int,col=col.SV,lwd=lwd.SV,lty=lty.SV)
    if(fun.xlog == TRUE) {
      segments(.1,fun.SV4.int,fun.SV4,fun.SV4.int,col=col.SV,lwd=lwd.SV,lty=lty.SV)
    } else {
      segments(0,fun.SV4.int,fun.SV4,fun.SV4.int,col=col.SV,lwd=lwd.SV,lty=lty.SV)
    }## IF ~ fun.xlog ~ SV4
  } ## IF ~ SV4
  #
  # Legend (EWL, 20120830, added SV legend back (inside of SV IF/THEN)
  # highest number will take priority
  # 20200401, add variable for legend text and units

  if(!is.na(leg_SV_units)){
    fun.SV  <- paste0(fun.SV,  " ", leg_SV_units)
    fun.SV2 <- paste0(fun.SV2, " ", leg_SV_units)
    fun.SV3 <- paste0(fun.SV3, " ", leg_SV_units)
    fun.SV4 <- paste0(fun.SV4, " ", leg_SV_units)
  }## IF ~ is.na(leg_SV_units) ~ END

  # Modify legend label
  fun.SV.lab <- ifelse(is.na(fun.SV.lab), "", paste0(fun.SV.lab, ", "))
  fun.SV2.lab <- ifelse(is.na(fun.SV2.lab), "", paste0(fun.SV2.lab, ", "))
  fun.SV3.lab <- ifelse(is.na(fun.SV3.lab), "", paste0(fun.SV3.lab, ", "))
  fun.SV4.lab <- ifelse(is.na(fun.SV4.lab), "", paste0(fun.SV4.lab, ", "))


  if(mySV.num==1) { ##IF.START
    legend("bottomright"
           ,lty=c(lty.data,lty.SV,lty.CI)
           ,lwd=c(lwd.data,lwd.SV,lwd.CI)
           ,col=c(col.data,col.SV,col.CI)
           ,legend=c("Fillet Concentration Data"
                     ,paste(leg_SV_str," (", fun.SV.lab, fun.SV,")",sep="")
                     ,"95% Confidence Intervals")
           ,bg="white")
  } else if (mySV.num==2) {
    legend("bottomright"
           ,lty=c(lty.data,rep(lty.SV,mySV.num),lty.CI)
           ,lwd=c(lwd.data,rep(lwd.SV,mySV.num),lwd.CI)
           ,col=c(col.data,rep(col.SV,mySV.num),col.CI)
           ,legend=c("Fillet Concentration Data"
                     ,paste(leg_SV_str," (", fun.SV.lab, fun.SV, ")",sep="")
                     ,paste(leg_SV_str," (", fun.SV2.lab, fun.SV2, ")",sep="")
                     ,"95% Confidence Intervals")
           ,bg="white")

  } else if (mySV.num==3) {
    legend("bottomright"
           ,lty=c(lty.data,rep(lty.SV,mySV.num),lty.CI)
           ,lwd=c(lwd.data,rep(lwd.SV,mySV.num),lwd.CI)
           ,col=c(col.data,rep(col.SV,mySV.num),col.CI)
           ,legend=c("Fillet Concentration Data"
                     ,paste(leg_SV_str," (", fun.SV.lab, fun.SV ,")",sep="")
                     ,paste(leg_SV_str," (", fun.SV2.lab, fun.SV2 ,")",sep="")
                     ,paste(leg_SV_str," (", fun.SV3.lab, fun.SV3 ,")",sep="")
                     ,"95% Confidence Intervals")
           ,bg="white")
  } else if (mySV.num==4) {
    legend("bottomright"
           ,lty=c(lty.data,rep(lty.SV,mySV.num),lty.CI)
           ,lwd=c(lwd.data,rep(lwd.SV,mySV.num),lwd.CI)
           ,col=c(col.data,rep(col.SV,mySV.num),col.CI)
           ,legend=c("Fillet Concentration Data"
                     ,paste(leg_SV_str," (", fun.SV.lab, fun.SV,")",sep="")
                     ,paste(leg_SV_str," (", fun.SV2.lab, fun.SV2,")",sep="")
                     ,paste(leg_SV_str," (", fun.SV3.lab, fun.SV3,")",sep="")
                     ,paste(leg_SV_str," (", fun.SV4.lab,fun.SV4,")",sep="")
                     ,"95% Confidence Intervals")
           ,bg="white")
  } else {
    legend("bottomright"
           ,lty=c(lty.data,lty.CI)
           ,lwd=c(lwd.data,lwd.CI)
           ,col=c(col.data,col.CI)
           ,legend=c("Fillet Concentration Data"
                     ,"95% Confidence Intervals")
           ,bg="white")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~
  # Break (after legend so goes on top)
  if(fun.break.boo==TRUE){##IF.fun.break.boo.START
    plotrix::axis.break(1,fun.break.pos,style="zigzag")
  }
  #
  #~~~~~~~~~~~~~~~~~~~~~~~~
  # 95% confidence intervals
  lines(fun.DF$UCB95Pct.P~sort(fun.DF$Value),col=col.CI,lty=lty.CI,lwd=lwd.CI)
  lines(fun.DF$LCB95Pct.P~sort(fun.DF$Value),col=col.CI,lty=lty.CI,lwd=lwd.CI)

  # Axis 4 (right)
  axis(4,at=fun.axis.4.at,labels=fun.axis.4.labels)
  axis(2)
  # Add minor tickmarks (1 = no tick, ratio = compared to major ticks)
  minor.tick(nx=1,ny=4,tick.ratio=0.33)
  par(new=T)
  #
  # different title based on projet (20161004)
  mtext(fun.axis.4.mtext, side = 4, line = mtext.line, cex = mtext.cex)
  # different yaxis (20210827), when use superscript have to use mtext instead
  if(fun.ylab == "") {
    mtext(fun.axis.2.mtext, side = 2, line = mtext.line - 0.5, cex = mtext.cex)
  }## IF ~ !is.na(fun.axis.2.mtext) ~ END
  #
  # Turn off new
  par(new=F)
} ## FUNCTION.END
