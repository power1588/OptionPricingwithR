###########
#Use the program scratch the web data of options from sina
#
#
###########
library(RCurl)
require(fOptions)

#underlying

U <- get.stock.info("sh510050")
underlying <- U$V4

#expiry
TT <- 22/245
#interest and cost of carry
r <- 0.025
b <- 0.01

temp1 = getURL("http://hq.sinajs.cn/list=OP_UP_5100501505")
temp2 = getURL("http://hq.sinajs.cn/list=OP_DOWN_5100501601")
k1 = strsplit(temp1," ")
k2 = strsplit(temp2," ")
k12 <- k1[[1]][2]
k22 <- k2[[1]][2]
callindex <- substr(k12,26,168)
putindex <- substr(k22,28,170)
callindex <- strsplit(callindex,",")
putindex <- strsplit(putindex,",")
callindex <- unlist(callindex)
putindex <- unlist(putindex)
N <- length(callindex)

#type
type1 <- "c"
type2 <- "p"

#generate the urls of specific call option 
url1 <- lapply(1:N, function(i) paste0('http://hq.sinajs.cn/list=',callindex[i]))
callprice <- lapply(1:N, function(i) getURL(url1[i], .encoding='GBK'))
#convert to character
callprice <- unlist(callprice)
c_price <- sapply(1:N, function(i) strsplit(callprice[i], ","))
cask_price <- sapply(1:N, function(i) as.double(c_price[[i]][4]))

#extract the strikes
a <- sapply(1:N, function(i) regexpr("50ETF购1月[0-9]{4}",callprice[i]))
strikes <- sapply(1:N, function(i) substr(callprice[i],a[i], a[i]+11))
X <- sapply(1:N, function(i) as.double(substr(strikes[i],9,12)))

# bid_vol bid present_p ask ask_vol position change strike 
# yesterday_p today_op up_limit down_limit 
# ask / bid 5~1 & order
# date 


#calculate the volatility smile
smile1 <- sapply(1:N, function(i) GBSVolatility(cask_price[i], type1,
                                                S = underlying,
                                                X = X[i]/1000, Time = TT, r = r, b = b))


#generate the urls of specific call option 
url2 <- lapply(1:N, function(i) paste0('http://hq.sinajs.cn/list=',putindex[i]))
putprice <- lapply(1:N, function(i) getURL(url2[i], .encoding='GBK'))
#convert to character
putprice <- unlist(putprice)
p_price <- sapply(1:N, function(i) strsplit(putprice[i], ","))
pask_price <- sapply(1:N, function(i) as.double(p_price[[i]][4]))

#extract the strikes
#a <- sapply(1:N, function(i) regexpr("50ETF沽1月[0-9]{4}",putprice[i]))
#strikes <- sapply(1:N, function(i) substr(callprice[i],a[i], a[i]+11))
#X <- sapply(1:N, function(i) as.double(substr(strikes[i],9,12)))

# bid_vol bid present_p ask ask_vol position change strike 
# yesterday_p today_op up_limit down_limit 
# ask / bid 5~1 & order
# date 


#calculate the volatility smile
smile2 <- sapply(1:N, function(i) GBSVolatility(pask_price[i], type2,
                                                S = underlying,
                                                X = X[i]/1000, Time = TT, r = r, b = b))
#plot the smile
par(mfrow= c(2,1))
plot(X/1000,smile1,type = "l",xlab = "implied volatility smile of 1601 call", ylab="implied vol")
plot(X/1000,smile2,type = "l",xlab = "implied volatility smile of 1601 put ", ylab="implied vol")

#
par(mfrow= c(1,1))
plot(X/1000,smile1,type = "l",ylim = c(0.2,0.5),xlab = "implied volatility smile of 1601 call", ylab="implied vol")
#plot(X/1000,smile2,type = "l",xlab = "implied volatility smile of 1601 put ", ylab="implied vol")
lines(X/1000,smile2, lty=2 )
legend(2.2,0.5,legend = c("call","put"), lty = 1:2)
