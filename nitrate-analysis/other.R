################################################################################
##     R SCRIPT: dateFunctions.R
##
##      PACKAGE: SGmisc
##
##  DESCRIPTION: Miscellaneous small functions for dates
##
##   WRITTEN BY: Steve Gutreuter
##               E-mail:  sgutreuter@gmail.com
################################################################################

#' Compute the midpoint between two dates
#'
#' @param startdate A vector of class \code{Date} containing the beginning
#' date(s)
#' @param enddate A vector of class \code{Date} containing the ending date(s)
#'
#'
#' @return A vector of class \code{Date} containing the midpoint date(s) between
#' \code{startdate} and \code{enddate}
#'
#' @author Steve Gutreuter
#'
#' @seealso \code{\link[lubridate]{interval}}
#'
#' @examples
#' begin <- ymd("2020-06-21", "2021-06-21")
#' end <- ymd("2020-12-21", "2021-12-21")
#' mid_date(begin, end)
#'
#' @importFrom lubridate interval as_date int_start int_end
#' @export
mid_date <- function(startdate, enddate) {
  stopifnot(class(startdate) == "Date" & class(enddate) == "Date")
  stopifnot(length(startdate) == length(enddate))
  idx <- enddate < startdate
  if (any(idx, na.rm = TRUE)) {
    enddate[idx] <- NULL
    startdate[idx] <- NULL
    cat(paste0("\nWARNING: NAs assigned to ", sum(idx, na.rm = TRUE),
               " inconsistent date pairs\n"))
  }
  res <- NULL
  if (length(startdate > 0)) {
    intobj <- lubridate::interval(startdate, enddate)
    res <- lubridate::as_date(lubridate::int_start(intobj) +
                                ((lubridate::int_end(intobj) -
                                    lubridate::int_start(intobj)) / 2))
  }
  res
}