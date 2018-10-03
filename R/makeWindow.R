#�������ϵ�һ�����׿�ʼ��������Խ��׵Ĵ���
#��Ϊ�漰�����ڣ���ͣ�Ƶ����⣬��˱�������������ڵġ�ǰһ�������ա���
#�������ڱ����ǿ��Բ��ܼ��ڡ�ͣ�Ƶ����صġ�
#���ǣ�CanBuyFilterҪȷ��Ŀ���Ʊ����ͣ�ƣ�Ҳû����ͣ��
#���߱�����һ���������ա�����ȷ�ϡ���ˣ���������ṩ��������
#֮ǰ�����һ��tradeable date�����������ڸ����г��Ľ����ա�
#' makeWindow
#'
#' @param first.days
#' @param market
#' @param offset
#'
#' @return
#' @export
#' @import zoo
#' @import purrr
#' @import WindAdapter
#' @examples
makeWindow = function(first.days, market = "cn", offset = 0) {

	index.price = getMarketIndex(market)
	all.trade.date = zoo::index(index.price)
	#first.days = first.days[first.days <= max(all.trade.date)]

	first.trade.days = sapply(first.days, function(x) {
		ii = (x <= all.trade.date)
		#if (all(!ii)) { stop(first.days) }
		first(all.trade.date[ii])
	})

	first.trade.days = as_date(first.trade.days, origin = lubridate::origin)

	first.trade.days = first.trade.days[!is.na(first.trade.days)]

	first.trade.days = first.trade.days + offset

	previous.trade.date = sapply(first.trade.days, function(x) {
		ii = (x > all.trade.date)
		last(all.trade.date[ii])
	})

	previous.trade.date = as_date(previous.trade.date, origin = lubridate::origin)

	#first.days = first.days[first.days <= today()]
	end.days = lead(previous.trade.date)
	if (is.na(last(end.days))) {
		end.days[length(end.days)] = today() + 1
	}

	result = data.frame(previous.date = previous.trade.date,
							 start.date = first.trade.days,
							 end.date = end.days,
							 period = paste0(first.trade.days, "::", end.days),
							 stringsAsFactors = FALSE)
	#result = data.frame(previous.date = lag(result$end.date),result)
	result
}

getMarketIndex = function(market = "cn") {

	market = tolower(market)
	if (!market %in% c("cn", "hk")) { stop(paste("Unknow market:", market)) }

	if (market == 'cn') {
		index.price = w.getPrice("000001.SH", options = "days=")
		#all.trade.date = w.tdays("1990-01-01",Sys.Date())$Data$DATETIME
		#print("cn")
	}
	if (market == "hk") {
		index.price = w.getPrice("HSI.HI", options = "TradingCalendar=HKEX;days=")
		#print("hk")
	}
	index.price
}

