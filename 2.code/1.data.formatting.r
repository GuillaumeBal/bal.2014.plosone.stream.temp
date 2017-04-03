# load raw data
raw.data <- read.table(data.file, dec = '.', sep = ',', h = T)


raw.data$date <- as.POSIXlt(raw.data$date, format = "%d/%m/%Y")

year.data <- year(raw.data$date)
month.data <- month(raw.data$date)
day.data <- yday(raw.data$date)

if(time.step == 1){

cut.off.dates.1 <- rep(1, length(unique(paste(year.data, month.data, sep = '/'))))    
cut.off.dates.2 <- as.vector(floor(by(day.data, INDICES = list(year.data, month.data), FUN = mean)))
cut.off.dates.3 <- as.vector(by(day.data, INDICES = list(year.data, month.data), FUN = max))
  
    
}
