# load time series used before
data.ts <- read.table('time.series.data.1.txt', h = T)
attach(data.ts)
date <- as.POSIXlt(date)

# read information about how to shift data
shift.data <- read.table('ts.shift.outputs.txt', h = T)

# extract values
t0.series <- round(shift.data[which(substring(rownames(shift.data), 1, 2) == 't0'), 5], 2)
names(t0.series) <- c('at', 'lfl', 'wt')

# centering values sin
centering.sin.values <- c(6.25, 18.75)

# shift apaplied
shift.values <- round(c(-shift.series - centering.sin.values[time.step.option]), 0) 

# shift time series for model
wt.data.s <- c(wt.data[(shift.values[3]) : length(wt.data)], rep(NA, shift.values[3] - 1))
at.data.s <- c(at.data[(shift.values[1]) : length(at.data)], rep(NA, shift.values[3] - 1))
lfl.data.s <- c(lfl.data[(shift.values[2]) : length(lfl.data)], rep(NA, shift.values[2] - 1))

plot(at.data, type = 'l')
abline(v = 12.5 + seq(0, by = 24, length.out = year.end - year.start + 1), col = 13)
abline(v = seq(1, by = 24, length.out = year.end - year.start + 1), col = 14)