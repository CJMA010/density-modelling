setwd("~/Documents/projects/r_project/pricing density")

library(readxl)
library(xts)
library(QRM)
library(MASS)
library(ggfortify)
library(splines)

df.options <- read_excel('options_data_1987.xlsx')
df.futures <- read_excel('futures_data_1987.xlsx')

df.bonds <- read_excel('TB_3mo.xls',col_types=c('date','numeric'))
df.bonds <- na.omit(df.bonds)
colnames(df.bonds) <- c('date','yield')
df.bonds <- as.xts(df.bonds,order.by = df.bonds$date)[,2]

dates.plots <- c(19870616,19870716,19870817,19870916,19871016)
date <- 19870616
dates.all <- as.matrix(unique(df.options$`Trade Date`))
start <- which(dates.all==19870616)
end <- which(dates.all==19871016)
dates.estimated <- dates.all[start:end]

return.estimate <- c()
down.prob.all <- c()

for (date in dates.estimated){
  date.xts <- as.Date(as.character(date),tryFormats='%Y%m%d')
  
  yield <- as.double(df.bonds[date.xts,1])/100
  
  futures.price <- as.double(df.futures[df.futures$`Trade Date`==date&df.futures$`Contract Year`== 1987&df.futures$`Contract Month`== 12,][1,'Settlement'])
  
  options.oneday <- df.options[df.options$`Trade Date`== date&df.options$`Contract Year`== 1987&df.options$`Contract Month`== 12,][,c('Put/Call','Strike Price','Settlement')]
  options.oneday.call <- options.oneday[options.oneday$`Put/Call`=='C',]
  # ordering
  options.oneday.call <- options.oneday.call[order(options.oneday.call$`Strike Price`),][,c('Strike Price','Settlement')]
  
  
  options.oneday.put <- options.oneday[options.oneday$`Put/Call`=='P',]
  options.oneday.put <- options.oneday.put[order(options.oneday.put$`Strike Price`),][,c('Strike Price','Settlement')]
  
  num <- c(options.oneday.call$`Strike Price`,options.oneday.put$`Strike Price`)
  max <- max(num)
  min <- min(num)
  seq <- seq(min,max,5)
  
  selltemet.call <- c()
  for (i in seq){
    if (i %in% options.oneday.call$`Strike Price`){
      selltemet.call <- c(selltemet.call,as.double(options.oneday.call[which(options.oneday.call$`Strike Price`==i),2]))
    }
    else{
      selltemet.call <- c(selltemet.call,NA)
    }
  }

  b.call <- as.matrix(c(NA,NA,diff(diff(as.matrix(selltemet.call)))/5))
  b.call[b.call<0&!is.na(b.call)] <- NA

  selltemet.put <- c()
  for (i in seq){
    if (i %in% options.oneday.put$`Strike Price`){
      selltemet.put <- c(selltemet.put,as.double(options.oneday.put[which(options.oneday.put$`Strike Price`==i),2]))
    }
    else{
      selltemet.put <- c(selltemet.put,NA)
    }
  }
  
  b.put <- as.matrix(c(NA,NA,diff(diff(as.matrix(selltemet.put)))/5))
  b.put[b.put<0&!is.na(b.put)] <- NA
  
  b <- c()
  for (i in 1:length(seq)){
    if (!is.na(b.call[i])){
      if (!is.na(b.put[i])){
        b <- c(b,(b.call[i]+b.put[i])/2)
      }
      else{
        b <- c(b,b.call[i])
      }
    }
    else{
      b <- c(b,b.put[i])
    }
  }
  
  delta_t <- length(date.xts:as.Date('1987-12-18'))/365
  
  r <- log(seq/futures.price)
  p <- b/exp(-yield*delta_t)
  q <- p*seq/5
  
  
  hist <- as.data.frame( cbind(r,q))
  colnames(hist) <- c('return','density')
  
  temp <- df.futures[df.futures$`Contract Year`==1987&df.futures$`Contract Month`==12,]
  temp.n <- which(temp$`Trade Date` == date)
  daily.log.returns <- diff(log(temp[(temp.n-20):temp.n,]$Settlement))
  var <- (sd(daily.log.returns))^2*delta_t*365
  
  
  N=1000
  data.simulated <- c()
  hist <- na.omit(hist)
  for (i in 1:nrow(hist)){
    data.simulated <- c(data.simulated,rep(hist[i,1],N*hist[i,2]))
  }
  
  fit <- fitdistr(data.simulated,'normal')
  
  range <- qnorm(c(0.25,0.75),mean=fit$estimate[1],sd=fit$estimate[2])/(1.349*sqrt(var))
  return.estimate <- rbind(return.estimate,matrix(c(date,range),1,3))

  down.prob <- pnorm(-0.1,mean=fit$estimate[1],sd=fit$estimate[2])/pnorm(0.1,mean=fit$estimate[1],sd=fit$estimate[2])
  down.prob.all <- rbind(down.prob.all, matrix(c(date,down.prob),1,2))
  
  if (date %in% dates.plots){
    print(ggplot(hist,aes(return,density))+
            geom_col(col="#E7B800",fill="#E7B800",alpha=0.5)+
            stat_function(fun=function(x) dnorm(x,mean=0,sd=sqrt(var)),col="#00AFBB"))
            #stat_function(fun=function(x) dnorm(x,mean=fit$estimate[1],sd=fit$estimate[2]),col='#FC4E07'))
  }
}

colnames(return.estimate) <- c('date','x1','x2')
return.estimate <- as.data.frame(return.estimate)
return.estimate$date <- as.Date(as.character(return.estimate$date),tryFormats='%Y%m%d')

ggplot(return.estimate)+
  geom_line(aes(x=date, y=x1),col="#E7B800")+
  geom_line(aes(x=date, y=x2),col="#00AFBB")+
  ylab('return')

down.prob.all <- as.data.frame(down.prob.all)
down.prob.all$V1 <- as.Date(as.character(down.prob.all$V1),tryFormats='%Y%m%d')

ggplot(down.prob.all)+
  geom_line(aes(x=V1, y=V2),col="#00AFBB")+
  ylab('down move')+
  xlab('date')
