#' makeTimeWindowsHalfYearly
#'
#' make time window half-yealy
#'
#' @param start.date
#' @param end.date
#' @param offset
#' @param market \code{market = c("cn", "hk")}
#' @return \code{data.frame(start.date, end.date, period)}
#' @export
#' @importFrom dplyr %>% lead
#' @import lubridate
#' @import xts
#' @import timeDate
#' @examples
#' makeTimeWindowsHalfYearly("2000-01-01", today())
makeTimeWindowsHalfYearly = function(start.date="2000-01-01", end.date=today(),
                                    market = "cn",offset = 0){
	#同季度版，但保留4月和8月


	market = tolower(market)
	if (!(market %in% c("cn", "hk"))) {
		stop("market must be one of c(\"cn\", \"hk\")")
	}

	start.date <- as.Date(start.date)
	end.date <- as.Date(end.date)

	start.year = year(start.date) - 1
	end.year = year(end.date) + 1

	q1.date = paste0(seq(start.year, end.year), "-04-30") %>% as.Date()
	q2.date = paste0(seq(start.year, end.year), "-08-31") %>% as.Date()
	q3.date = paste0(seq(start.year, end.year), "-10-31") %>% as.Date()
	q4.date = paste0(seq(start.year, end.year), "-01-31") %>% as.Date()

	first.days = 1 + c(q1.date, q3.date)
	first.days = first.days[order(first.days)]

	result = makeWindow(first.days, market, offset)

	result[result$end.date >= start.date,]
}

