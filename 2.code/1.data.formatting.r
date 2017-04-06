# load raw data
raw.data <- read.table(data.file, dec = '.', sep = ',', h = T)


raw.data$date <- as.POSIXlt(raw.data$date, format = "%d/%m/%Y")

year.data <- year(raw.data$date)
month.data <- month(raw.data$date)
day.data <- yday(raw.data$date)
day.index <- seq(1, dim(raw.data)[1])

#if(time.step == 1){

cut.off.dates.1 <- as.vector(by(day.index, INDICES = list(month.data, year.data), FUN = min))   
cut.off.dates.2 <- as.vector(floor(by(day.index, INDICES = list(month.data, year.data), FUN = mean)))
cut.off.dates.3 <- cut.off.dates.2 + 1
cut.off.dates.4 <- as.vector(by(day.index, INDICES = list(month.data, year.data), FUN = max))

cut.off.series <- c(cut.off.dates.1, cut.off.dates.2, cut.off.dates.3, cut.off.dates.4) 
cut.off.series <- cut.off.series[order(cut.off.series)]

wt.data <- sapply(1:(length(cut.off.series) / 2), 
                  function(i){
                    ifelse(sum(!is.na(raw.data$water.temp[cut.off.series[1 + 2 * (i - 1)]:cut.off.series[2 * i]])) > 5,
                           mean(raw.data$water.temp[cut.off.series[1 + 2 * (i - 1)]:cut.off.series[2 * i]], na.rm = T),
                           NA)
                  }
)

summary(wt.data)
length(wt.data)
plot(wt.data, type = 'l')

sum(is.na(raw.data$water.temp[cut.off.series[i]:cut.off.series[i + 1]])) > 5




at.data <- sapply(1:(length(cut.off.series) - 1), function(i) if(sum(is.na(raw.data$water.temp[cut.off.series[i]:cut.off.series[i + 1])) > 5)mean(raw.data$air.temp[cut.off.series[i]:cut.off.series[i + 1]] , na.rm = T))
lfl.data <- log(sapply(1:(length(cut.off.series) - 1), function(i) if(sum(is.na(raw.data$water.temp[cut.off.series[i]:cut.off.series[i + 1])) > 5)mean(raw.data$flow[cut.off.series[i]:cut.off.series[i + 1]] , na.rm = T)))

#}

if(time.step == 1){
  
  cut.off.dates.1 <- as.vector(by(day.index, INDICES = list(month.data, year.data), FUN = min))   
  cut.off.dates.2 <- as.vector(floor(by(day.index, INDICES = list(month.data, year.data), FUN = mean)))
  cut.off.dates.3 <- cut.off.dates.2 + 1
  cut.off.dates.4 <- as.vector(by(day.index, INDICES = list(month.data, year.data), FUN = max))
  
  cut.off.series <- c(cut.off.dates.1, cut.off.dates.2, cut.off.dates.3, cut.off.dates.4) 
  cut.off.series <- cut.off.series[order(cut.off.series)]
  
  wt.data <- sapply(1:(length(cut.off.series) - 1), function(i) mean(raw.data$water.temp[cut.off.series[i]:cut.off.series[i + 1]] , na.rm = T))
  at.data <- sapply(1:(length(cut.off.series) - 1), function(i) mean(raw.data$air.temp[cut.off.series[i]:cut.off.series[i + 1]] , na.rm = T))
  lfl.data <- log(sapply(1:(length(cut.off.series) - 1), function(i) mean(raw.data$flow[cut.off.series[i]:cut.off.series[i + 1]] , na.rm = T)))
  
}


par(mfrow = c(3, 1))
plot(wt.data, type = 'l')
plot(at.data, type = 'l')
plot(lfl.data, type = 'l')


