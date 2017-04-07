# load raw data
raw.data <- read.table(data.file, dec = '.', sep = ',', h = T)

# specify date format
raw.data$date <- as.POSIXlt(raw.data$date, format = "%d/%m/%Y")

# subet based on years chosen
raw.data <- raw.data[- which(year(raw.data$date) < year.start | year(raw.data$date) > year.end), ]

# check for negative water temperature and flow data
#raw.data$water.temp[10] <- -1
ifelse(any(raw.data$water.temp < 0 | raw.data$flow < 0, na.rm = TRUE),
       stop("dummy error, negative water temperature or flow data", call. = FALSE),
       print('No negative water temperature or flow data'))

# derive some elements for data formatting
year.data <- year(raw.data$date)
month.data <- month(raw.data$date)
day.data <- yday(raw.data$date)
day.index <- seq(1, dim(raw.data)[1])

# formatting for 15 days time period
if(time.step.option == 1){
  
  # series of dates used for calculating means
  cut.off.dates.1 <- as.vector(by(day.index, INDICES = list(month.data, year.data), FUN = min))   
  cut.off.dates.2 <- as.vector(floor(by(day.index, INDICES = list(month.data, year.data), FUN = mean)))
  cut.off.dates.3 <- cut.off.dates.2 + 1
  cut.off.dates.4 <- as.vector(by(day.index, INDICES = list(month.data, year.data), FUN = max))
  cut.off.series <- c(cut.off.dates.1, cut.off.dates.2, cut.off.dates.3, cut.off.dates.4) 
  cut.off.series <- cut.off.series[order(cut.off.series)]
  
  # water temperature time series
  wt.data <- sapply(1:(length(cut.off.series) / 2), 
                    function(i){
                      ifelse(sum(!is.na(raw.data$water.temp[cut.off.series[1 + 2 * (i - 1)]:cut.off.series[2 * i]])) > 5,
                             mean(raw.data$water.temp[cut.off.series[1 + 2 * (i - 1)]:cut.off.series[2 * i]], na.rm = T),
                             NA)
                    })
  
  # air temperature time series
  at.data <- sapply(1:(length(cut.off.series) / 2), 
                    function(i){
                      ifelse(sum(!is.na(raw.data$air.temp[cut.off.series[1 + 2 * (i - 1)]:cut.off.series[2 * i]])) > 5,
                             mean(raw.data$air.temp[cut.off.series[1 + 2 * (i - 1)]:cut.off.series[2 * i]], na.rm = T),
                             NA)
                    })
  
  # temperature time series, in log scale
  lfl.data <- log(sapply(1:(length(cut.off.series) / 2), 
                         function(i){
                           ifelse(sum(!is.na(raw.data$flow[cut.off.series[1 + 2 * (i - 1)]:cut.off.series[2 * i]])) > 5,
                                  mean(raw.data$flow[cut.off.series[1 + 2 * (i - 1)]:cut.off.series[2 * i]], na.rm = T),
                                  NA)
                         }))
  
  
}else{
  
  bissex.suppress <- which(substring(as.character(raw.data$date), 6, 10) == '02-29')
  raw.data.minus.0229 <- raw.data[-bissex.suppress, ]
  cut.off.series <- seq(1, dim(raw.data.minus.0229)[1], 5)
  
  # water temperature time series
  wt.data <- sapply(1:length(cut.off.series), function(i){
    ifelse(sum(!is.na(raw.data.minus.0229$water.temp[cut.off.series[i]:(cut.off.series[i] + 4)])) >= 2,
           mean(raw.data.minus.0229$water.temp[cut.off.series[i]:(cut.off.series[i] + 4)], na.rm = TRUE),
           NA)
  }) 
  
  # air temperature time series
  at.data <- sapply(1:length(cut.off.series), function(i){
    ifelse(sum(!is.na(raw.data.minus.0229$air.temp[cut.off.series[i]:(cut.off.series[i] + 4)])) >= 2,
           mean(raw.data.minus.0229$air.temp[cut.off.series[i]:(cut.off.series[i] + 4)], na.rm = TRUE),
           NA)
  }) 
  
  # time flow time series
  lfl.data <- log(sapply(1:length(cut.off.series), function(i){
    ifelse(sum(!is.na(raw.data.minus.0229$water.temp[cut.off.series[i]:(cut.off.series[i] + 4)])) >= 2,
           mean(raw.data.minus.0229$water.temp[cut.off.series[i]:(cut.off.series[i] + 4)], na.rm = TRUE),
           NA)
  }))
  
}

# some more indices
time.steps.1 <- seq(1, length(wt.data), 1)
n.time.steps.1 <- length(time.steps.1)
n.time.steps.year.1 <- ifelse(time.step.option == 1, 24, 73)
pi.value <- pi

# plot data used
par(mfrow = c(3, 1),
    mar = c(2, 4, 2, 2),
    oma = c(1, .5, 0.5, .5))
plot(wt.data, type = 'l', main = 'Used data')
plot(at.data, type = 'l')
plot(lfl.data, type = 'l')