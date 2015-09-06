library(Quandl)
Quandl.auth("your api key")

html <- GET("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
webpage <- content(html, as = "text")
webpage <- htmlParse(webpage, asText = TRUE, useInternalNodes = TRUE)
snp500 <- readHTMLTable(getNodeSet(webpage, "//table")[[1]], as.data.frame = TRUE, stringsAsFactors = FALSE)[,1]

rm(html, webpage)

quotes <- lapply(snp500, function(code){
  quote <- tryCatch({Quandl(paste("YAHOO/", code, sep =""), start_date = "2000-01-01", end_date = "2014-12-31", collapse = "daily")},
                    error = function(e){NA})
  attributes(quote)$name <- code
  if(!is.na(quote)){
    return(quote)
  } else {
    NULL
  }})

quotes <- Filter(Negate(is.null), quotes)
names(quotes) <- sapply(quotes, attr, "name")
rm(snp500)

save.image(file="snp500.RData")

sapply(quotes, function(share){
  write.table(share, paste("~/", attributes(share)$name, ".csv", sep = ""), row.names = FALSE)
})