###########
#Use the program scratch the web data of options from sina
#
#
###########
library(RCurl)
require(fOptions)
library(plotly)

#timing
ptm <- proc.time()

#obtain the price of underlying
source("get.stock.info.R")
U <- get.stock.info("sh510050")
underlying <- U$V4

#expiry
T1 <- 21/245
T2 <- 49/245
T3 <- 77/245
T4 <- 168/245

#interest and cost of carry
r <- 0.025
b <- 0.01

#generate the url of different options
temp1 = getURL("http://hq.sinajs.cn/list=OP_UP_5100501601")
temp2 = getURL("http://hq.sinajs.cn/list=OP_DOWN_5100501601")
temp3 = getURL("http://hq.sinajs.cn/list=OP_UP_5100501602")
temp4 = getURL("http://hq.sinajs.cn/list=OP_DOWN_5100501602")
temp5 = getURL("http://hq.sinajs.cn/list=OP_UP_5100501603")
temp6 = getURL("http://hq.sinajs.cn/list=OP_DOWN_5100501603")
temp7 = getURL("http://hq.sinajs.cn/list=OP_UP_5100501606")
temp8 = getURL("http://hq.sinajs.cn/list=OP_DOWN_5100501606")


#obtain the index of the options
k1 = strsplit(temp1," ")
k2 = strsplit(temp2," ")
k3 = strsplit(temp3," ")
k4 = strsplit(temp4," ")
k5 = strsplit(temp5," ")
k6 = strsplit(temp6," ")
k7 = strsplit(temp7," ")
k8 = strsplit(temp8," ")


k12 <- k1[[1]][2]
k22 <- k2[[1]][2]
k32 <- k3[[1]][2]
k42 <- k4[[1]][2]
k52 <- k5[[1]][2]
k62 <- k6[[1]][2]
k72 <- k7[[1]][2]
k82 <- k8[[1]][2]



callindex1 <- substr(k12,26,168)
putindex1 <- substr(k22,28,170)
callindex2 <- substr(k32,26,168)
putindex2 <- substr(k42,28,170)
callindex3 <- substr(k52,26,392)
putindex3 <- substr(k62,28,394)
callindex4 <- substr(k72,26,200)
putindex4 <- substr(k82,28,202)


callindex1 <- strsplit(callindex1,",")
putindex1 <- strsplit(putindex1,",")
callindex2 <- strsplit(callindex2,",")
putindex2 <- strsplit(putindex2,",")
callindex3 <- strsplit(callindex3,",")
putindex3 <- strsplit(putindex3,",")
callindex4 <- strsplit(callindex4,",")
putindex4 <- strsplit(putindex4,",")


callindex1 <- unlist(callindex1)
putindex1 <- unlist(putindex1)
callindex2 <- unlist(callindex2)
putindex2 <- unlist(putindex2)
callindex3 <- unlist(callindex3)
putindex3 <- unlist(putindex3)
callindex4 <- unlist(callindex4)
putindex4 <- unlist(putindex4)

if(length(callindex1)==length(putindex1)) N1 <- length(callindex1)
if(length(callindex2)==length(putindex2)) N2 <- length(callindex2)
if(length(callindex3)==length(putindex3)) N3 <- length(callindex3)
if(length(callindex4)==length(putindex4)) N4 <- length(callindex4)

#type
type1 <- "c"
type2 <- "p"


#generate the urls of specific call option 
url1 <- lapply(1:N1, function(i) paste0('http://hq.sinajs.cn/list=',callindex1[i]))
callprice1 <- lapply(1:N1, function(i) getURL(url1[i], .encoding='GBK'))

url2 <- lapply(1:N2, function(i) paste0('http://hq.sinajs.cn/list=',callindex2[i]))
callprice2 <- lapply(1:N2, function(i) getURL(url2[i], .encoding='GBK'))

url3 <- lapply(1:N3, function(i) paste0('http://hq.sinajs.cn/list=',callindex3[i]))
callprice3 <- lapply(1:N3, function(i) getURL(url3[i], .encoding='GBK'))

url4 <- lapply(1:N4, function(i) paste0('http://hq.sinajs.cn/list=',callindex4[i]))
callprice4 <- lapply(1:N4, function(i) getURL(url4[i], .encoding='GBK'))



#convert to character
callprice1 <- unlist(callprice1)
c_price1 <- sapply(1:N1, function(i) strsplit(callprice1[i], ","))
cask_price1 <- sapply(1:N1, function(i) as.double(c_price1[[i]][4]))

callprice2 <- unlist(callprice2)
c_price2 <- sapply(1:N2, function(i) strsplit(callprice2[i], ","))
cask_price2 <- sapply(1:N2, function(i) as.double(c_price2[[i]][4]))

callprice3 <- unlist(callprice3)
c_price3 <- sapply(1:N3, function(i) strsplit(callprice3[i], ","))
cask_price3 <- sapply(1:N3, function(i) as.double(c_price3[[i]][4]))

callprice4 <- unlist(callprice4)
c_price4 <- sapply(1:N4, function(i) strsplit(callprice4[i], ","))
cask_price4 <- sapply(1:N4, function(i) as.double(c_price4[[i]][4]))

#extract the strikes
a1 <- sapply(1:N1, function(i) regexpr("月[0-9]{4}",callprice1[i]))
strikes1 <- sapply(1:N1, function(i) substr(callprice1[i],a1[i]+ 1, a1[i]+4))
X1 <- sapply(1:N1, function(i) as.double(strikes1[i]))

a2 <- sapply(1:N2, function(i) regexpr("月[0-9]{4}",callprice2[i]))
strikes2 <- sapply(1:N2, function(i) substr(callprice2[i],a2[i]+ 1, a2[i]+4))
X2 <- sapply(1:N1, function(i) as.double(strikes2[i]))

a3 <- sapply(1:N3, function(i) regexpr("月[0-9]{4}",callprice3[i]))
strikes3 <- sapply(1:N3, function(i) substr(callprice3[i],a3[i]+ 1, a3[i]+4))
X3 <- sapply(1:N3, function(i) as.double(strikes3[i]))

a4 <- sapply(1:N4, function(i) regexpr("月[0-9]{4}",callprice4[i]))
strikes4 <- sapply(1:N4, function(i) substr(callprice4[i],a4[i]+ 1, a4[i]+4))
X4 <- sapply(1:N4, function(i) as.double(strikes4[i]))

# bid_vol bid present_p ask ask_vol position change strike 
# yesterday_p today_op up_limit down_limit 
# ask / bid 5~1 & order
# date 


#calculate the volatility smile
smilec1 <- sapply(1:N1, function(i) GBSVolatility(cask_price1[i], type1,
                                                S = underlying,
                                                X = X1[i]/1000, Time = T1, r = r, b = b))
smilec2 <- sapply(1:N2, function(i) GBSVolatility(cask_price2[i], type1,
                                                S = underlying,
                                                X = X2[i]/1000, Time = T2, r = r, b = b))
smilec3 <- sapply(1:N3, function(i) GBSVolatility(cask_price3[i], type1,
                                                S = underlying,
                                                X = X3[i]/1000, Time = T3, r = r, b = b))
smilec4 <- sapply(1:N4, function(i) GBSVolatility(cask_price4[i], type1,
                                                 S = underlying,
                                                 X = X4[i]/1000, Time = T4, r = r, b = b))

#ignore the put temporarily
if (1<0 ){
#generate the urls of specific put option 
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

#want to plot the surface of volatility(strikes, maturity, implied volatility)
par(mfrow= c(1,1))
plot(X1/1000,smilec1,type = "l",ylim = c(0.2,0.5),xlab = "implied volatility smile of 1601 call", ylab="implied vol")
lines(X2/1000,smilec2, lty=2 )
lines(X3/1000,smilec3, lty=3 )
lines(X4/1000,smilec2, lty=4 )
legend(2.2,0.5,legend = c("call01","call02","call03","call04"), lty = 1:4)

#
par(mfrow= c(1,1))
plot(X1/1000,smilec1,type = "l",ylim = c(0.2,0.5),xlab = "strikes", ylab="implied vol")
title("The volatility smaile")
#plot(X/1000,smile2,type = "l",xlab = "implied volatility smile of 1601 put ", ylab="implied vol")
lines(X2/1000,smilec2, lty=2 )
legend(2.2,0.5,legend = c("call1601","call1602"), lty = 1:2)

}

#plot the surface
smilec <- data.frame(smilec1, smilec2,smilec3[9:17],smilec4[2:10] )
smilec <- as.matrix(smilec)
#smilec <- as.matrix(smilec, ncol = 4)
Time <- c(T1,T2,T3,T4)
Strike <- as.double(strikes1)
persp(x = Strike, y = Time, z = smilec, xlim = range(Strike), ylim = range(Time), zlim = range(smilec),
      xlab = "Strikes", ylab = "Maturaty", zlab = "IV")

proc.time() - ptm