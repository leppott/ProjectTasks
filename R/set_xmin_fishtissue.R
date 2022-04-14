# 1. function for use in determining X min
fun.SetXMin <- function(fun.value) {
  # determine min value for x-axis based on max value
  ifelse(fun.value > 1000000, -10000,
         ifelse(fun.value > 10000, -100,
                ifelse(fun.value > 1000, -10,
                       ifelse(fun.value > 100, -1,
                              ifelse(fun.value > 10, -0.1,-0.01)))))
} ## FUNCTION.END
