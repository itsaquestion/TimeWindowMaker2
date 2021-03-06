% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeTimeWindowsQuarterly.R
\name{makeTimeWindowsQuarterly}
\alias{makeTimeWindowsQuarterly}
\title{makeTimeWindowsQuarterly}
\usage{
makeTimeWindowsQuarterly(start.date = "2000-01-01", end.date = today(),
  market = "cn", offset = 0)
}
\arguments{
\item{market}{\code{market = c("cn", "hk")}}
}
\value{
\code{data.frame(start.date, end.date, period)}
}
\description{
make time window Quarterly
}
\details{
截止日期
年报/Q1 = 4-30, 中报/Q2 = 8-31, Q3 = 10-31, 伪Q4 = 01-31。各季报截止都是当季结束+1个月，除了中报是+2个月。
那么，获得年报/Q1之后，这个数据需要使用4个月。中报/Q2数据使用2个月，其余都是使用3个月。
}
\examples{
makeTimeWindowsQuarterly("2000-01-01", today())
}
