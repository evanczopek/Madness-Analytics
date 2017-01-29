# Evan Czopek - 1/29/2017 6:00pm


library(XML)
url_one <- "http://www.sports-reference.com/cbb/seasons/2017-school-stats.html"
all_data_df <- readHTMLTable(url_one,
                         which = 1)

