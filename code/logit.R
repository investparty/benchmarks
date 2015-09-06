library(foreach)
library(ggplot2)

load("~/snp500.RData")

shares <- lapply(quotes, function(stock){
  stock <- stock[order(stock$Date),]
  stock$ret <- c(NA, log(stock$Close[2:nrow(stock)] / stock$Close[1:(nrow(stock)-1)]))
  stock$lag <- c(NA, stock$ret[1:(nrow(stock)-1)])
  stock$lag2 <- c(NA, NA, stock$ret[1:(nrow(stock)-2)])
  stock$dir <- stock$ret / abs(stock$ret)
  stock$dir[stock$dir == -1] <- 0
  stock <- stock[complete.cases(stock),]
  
  return(subset(stock, select = c("Date", "ret", "dir", "lag", "lag2")))
})

years <- 2000:2013

models <- lapply(shares, function(share){
  
  test <- sapply(years, function(year){
    learnData <- share[format(share$Date, "%Y") == year,]
    if(nrow(learnData) >= 100)
    {
      testData <- share[format(share$Date, "%Y") == year + 1,]
      
      model <- glm(dir ~ lag + lag2, testData, family = "binomial")
      nullmod <- glm(testData$dir ~ 1, family = "binomial")
      
      r2 <- as.numeric(1 - logLik(model) / logLik(nullmod))
      lpd <- sum(round(model$fitted.values) == learnData$dir) / nrow(testData)
      lpp <- sum(ifelse(round(model$fitted.values) == testData$dir, testData$ret, -testData$ret))
      
      testData$prediction <- predict(model, testData)
      tpd <- sum(round(testData$prediction) == testData$dir) / nrow(testData)
      tpp <- sum(ifelse(round(testData$prediction) == testData$dir, testData$ret, -testData$ret))
      
      return(c(r2, lpd, lpp, tpd, tpp))
    }
    else
      return(rep(NA, 5))
  })
  
  colnames(test) <- years + 1
  
  return(test)
  
})

ymeans <- sapply(years+1, function(year){return(cbind(apply(sapply(models, function(share){cbind(share[,as.character(year)])}), 1, mean, na.rm = TRUE)))})
colnames(ymeans) <- years + 1
rownames(ymeans) <- c("R2", "Learn predicted direction", "Learn predicted profit", "Test predicted direction", "Test predicted profit")

foreach(i=1:nrow(ymeans)) %do% plot(ymeans[i,], type = "h", xlab = "years", ylab = rownames(ymeans)[i])
lpp <- c(mean(ymeans[,3]), sum(ymeans[,3]))
tpp <- c(mean(ymeans[,5]), sum(ymeans[,5]))

ymeans <- round(ymeans, 3)

ysharemeans <- apply(sapply(models, function(share){return(cbind(share[2, ]))}), 2, mean, na.rm = TRUE)
plot(ysharemeans, type = "h", xlab = "Stocks", ylab = "Test predicted direction")
ymeans50 <- sapply(years+1, function(year){return(cbind(apply(sapply(models[which(ysharemeans  >.5)], function(share){cbind(share[,as.character(year)])}), 1, mean, na.rm = TRUE)))})
colnames(ymeans50) <- years + 1
rownames(ymeans50) <- c("R2", "Learn predicted direction", "Learn predicted profit", "Test predicted direction", "Test predicted profit")
attributes(ymeans50)$stocks <- which(ysharemeans  >.5)

ymeans51 <- sapply(years+1, function(year){return(cbind(apply(sapply(models[which(ysharemeans  >.51)], function(share){cbind(share[,as.character(year)])}), 1, mean, na.rm = TRUE)))})
colnames(ymeans51) <- years + 1
rownames(ymeans51) <- c("R2", "Learn predicted direction", "Learn predicted profit", "Test predicted direction", "Test predicted profit")
attributes(ymeans51)$stocks <- which(ysharemeans  >.51)

ymeans52 <- sapply(years+1, function(year){return(cbind(apply(sapply(models[which(ysharemeans  >.52)], function(share){cbind(share[,as.character(year)])}), 1, mean, na.rm = TRUE)))})
colnames(ymeans52) <- years + 1
rownames(ymeans52) <- c("R2", "Learn predicted direction", "Learn predicted profit", "Test predicted direction", "Test predicted profit")
attributes(ymeans52)$stocks <- which(ysharemeans  >.52)

ymeans53 <- sapply(years+1, function(year){return(cbind(apply(sapply(models[which(ysharemeans  >.53)], function(share){cbind(share[,as.character(year)])}), 1, mean, na.rm = TRUE)))})
colnames(ymeans53) <- years + 1
rownames(ymeans53) <- c("R2", "Learn predicted direction", "Learn predicted profit", "Test predicted direction", "Test predicted profit")
attributes(ymeans53)$stocks <- which(ysharemeans  >.53)

ymeans54 <- sapply(years+1, function(year){return(cbind(apply(sapply(models[which(ysharemeans  >.54)], function(share){cbind(share[,as.character(year)])}), 1, mean, na.rm = TRUE)))})
colnames(ymeans54) <- years + 1
rownames(ymeans54) <- c("R2", "Learn predicted direction", "Learn predicted profit", "Test predicted direction", "Test predicted profit")
attributes(ymeans54)$stocks <- which(ysharemeans  >.54)

rm(i, quotes)

save.image("../../R/snp500logit.RData")
