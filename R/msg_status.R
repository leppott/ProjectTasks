# 2. function for informing user of progress
fun.Msg.Status <- function(fun.status
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
