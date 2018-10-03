#' makeTimeWindowsQuarterly
#'
#' make time window Quarterly
#'
#' 截止日期
#' 年报/Q1 = 4-30, 中报/Q2 = 8-31, Q3 = 10-31, 伪Q4 = 01-31。各季报截止都是当季结束+1个月，除了中报是+2个月。
#' 那么，获得年报/Q1之后，这个数据需要使用4个月。中报/Q2数据使用2个月，其余都是使用3个月。
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
#' makeTimeWindowsQuarterly("2000-01-01", today())
makeTimeWindowsQuarterly = function(start.date="2000-01-01", end.date=today(),
                                     market = "cn",offset = 0){
	

  market = tolower(market)
  if(!(market %in% c("cn","hk"))){
    stop("market must be one of c(\"cn\", \"hk\")")
  }

  start.date <- as.Date(start.date)
  end.date <- as.Date(end.date)

  start.year = year(start.date) - 1
  end.year = year(end.date) + 1

  q1.date = paste0(seq(start.year,end.year),"-04-30") %>% as.Date()
  q2.date = paste0(seq(start.year,end.year),"-08-31") %>% as.Date()
  q3.date = paste0(seq(start.year,end.year),"-10-31") %>% as.Date()
	q4.date = paste0(seq(start.year, end.year), "-01-31") %>% as.Date()

	first.days = 1 + c(q4.date, q1.date, q2.date, q3.date)
	first.days = first.days[order(first.days)]

	result = makeWindow(first.days, market,offset)

	result[result$end.date >= start.date,]

}

