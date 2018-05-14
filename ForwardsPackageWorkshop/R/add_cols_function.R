add_cols <- function(x, y, where = 1) {
  if (where == 1) {
    cbind(y, x)
  } else if (where > ncol(x)) {
    cbind(x, y)
  } else {
    lhs <- 1:(where - 1)
    cbind(x[lhs], y, x[-lhs])
  }
}
 #' @param x data frame to insert columns into
 #' @param y the columns you want to insert
 #' @param where ordinal location for the insert, 1 means before all of the columns in x
 