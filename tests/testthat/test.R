#library(stringr)
library(lubridate)
library(testthat)
library(TimeWindowMaker2)
library(xts)
library(WindAdapter)
library(timeDate)
library(xts)
library(dplyr)
library(purrr)

context("test: make time windows")

MyUtils::rmAll()
#MyUtils::sourceDir("R/")
w.checkStart()

test_that("making ", {

  a = makeTimeWindowsMonthly("2017-01-01",Sys.Date())
  b = makeTimeWindowsHalfYearly("2017-01-01",Sys.Date())

  expect_equal(names(a),names(b))
	expect_true(as.Date(a$previous.date[1]) <= "2017-01-01")
	expect_true(as.Date(b$previous.date[1]) <= "2017-01-01")

})

test_that("CN vs HK ", {

  a = makeTimeWindowsMonthly("2017-01-01",Sys.Date(), market = "cn")
  expect_equal(a$previous.date[2] , as.Date("2017-01-26"))

  b = makeTimeWindowsMonthly("2017-01-01",Sys.Date(), market = "hk")
  expect_equal(b$previous.date[2] , as.Date("2017-01-27"))

  #all(a == b)

})

test_that("weekly", {

  a = makeTimeWindowsWeekly("2017-01-01",Sys.Date(), market = "cn")
  b = makeTimeWindowsWeekly("2017-01-01",Sys.Date(), market = "hk")

  expect_equal(!all(a$previous.date == b$previous.date),T)

  #all(a == b)

})


checkLogic = function(x) {
	all(all(x$start.date > x$previous.date),
			all(x$start.date < x$end.date),
			all(lag(x$end.date) < (x$start.date), na.rm = T)
			)
}

test_that("offset", {

	ret = makeTimeWindowsQuarterly(offset = 0)
	expect_true(checkLogic(ret))

	ret = makeTimeWindowsQuarterly(offset = 1)
	expect_true(checkLogic(ret))

	#all(a == b)

})


