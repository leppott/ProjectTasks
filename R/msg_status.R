#' @title Status Message
#'
#' @description Print Status Message to Console
#'
#' @details Function for informing user of progress
#'
#' Text is composed using `print` so will have to flush console if used inside
#' of a function or loop.
#'
#' @param fun.status Status of item
#' @param fun.item.num.current Item number
#' @param fun.item.num.total Total number of items
#' @param fun.item.name Name of item
#'
#' @return Returns a text to the console
#'
#' @export
msg_status <- function(fun.status
                           , fun.item.num.current
                           , fun.item.num.total
                           , fun.item.name) {
  print(paste("Processing item "
              ,fun.item.num.current
              ," of "
              ,fun.item.num.total
              ,", "
              ,fun.status
              ,", "
              ,fun.item.name
              ,".",
              sep=""))
} ## FUNCTION.END
