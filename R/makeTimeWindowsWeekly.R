
#' makeTimeWindowsWeekly
#'
#' make a weekly time window
#' start.date, end.date and period based on weekdays
#' previous.date based on tradable days
#'
#' @param start.date
#' @param end.date
#' @param use.tradedays
#' @param market \code{market = c("cn", "hk")}
#' @return \code{data.frame(start.date, end.date, period)}
#' @export
#' @importFrom dplyr %>% lead
#' @import lubridate
#' @import xts
#' @import timeDate
#' @import WindAdapter
#' @examples
#' makeTimeWindowsWeekly("2010-01-01",today())
makeTimeWindowsWeekly = function(start.date="2017-01-01", end.date=Sys.Date(),
                                  market = "cn"){
  market = tolower(market)
  if(!(market %in% c("cn","hk"))){
    stop("market must be one of c(\"cn\", \"hk\")")
  }
  # all.time.period = next.period

  start.date = as.Date(start.date)
  end.date = as.Date(end.date)

  alldays = seq(from=(start.date - 6), to=end.date , by='day')
  wd = wday(alldays)
  first.monday = first(alldays[wd==1])

  alldays = alldays[wd==1]
  alldays = alldays[alldays>=first.monday]

	ret = makeWindow(alldays, market, offset = 0)

  ret[ret$end.date>=start.date,]
}

