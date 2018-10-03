
#' makeTimeWindowsMonthly
#'
#' make a monthly time window
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
#' makeTimeWindowsMonthly("2010-01-01",today())
makeTimeWindowsMonthly = function(start.date="2017-01-01", end.date=Sys.Date(),
                                   market = "cn",offset = 0 ){
  market = tolower(market)
  if(!(market %in% c("cn","hk"))){
    stop("market must be one of c(\"cn\", \"hk\")")
  }
  # all.time.period = next.period
	start.date = as.Date(start.date)
	end.date = as.Date(end.date)
	first.days = makeFirstdays_m(start.date,end.date)

	result = makeWindow(first.days, market, offset)

	result[result$end.date >= start.date,]
}

makeFirstdays_m = function(start.date = "2017-01-01", end.date = Sys.Date()) {
	start.date = as.Date(start.date)
	end.date = as.Date(end.date)

	start.date = start.date %>%
		timeFirstDayInMonth() %>%
		as.character() %>%
		as.Date()

	first.days = seq(start.date, end.date, by = "month")
	first.days
}






