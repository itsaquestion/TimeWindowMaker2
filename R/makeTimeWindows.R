#' makeTimeWindows
#'
#' @param start.date
#' @param end.date
#' @param period
#' @param market
#' @param offset
#'
#' @return
#' @export
#'
#' @examples
#' makeTimeWindows("2016-01-01",today(),"q")
makeTimeWindows = function(start.date = "2000-01-01", end.date = today(),
													 period = "m", market = "cn",offset = 0){
  params = c(as.list(environment()))

  if(!(period %in% c("w","m","q","hy"))){
    stop("market must be one of c(\"w\", \"m\",\"q\",\"hy\")")
  }
  result = NULL

  if(period == "m"){
    result = makeTimeWindowsMonthly(start.date,end.date,market,offset = offset)
  }else if(period == "w"){
    result = makeTimeWindowsWeekly(start.date,end.date,market)
  }else if(period == "q"){
    result = makeTimeWindowsQuarterly(start.date,end.date,market,offset = offset)
  }else if(period == "hy"){
    result = makeTimeWindowsHalfYearly(start.date,end.date,market,offset = offset)
  }

  attr(result,"params") = params
  result
}
