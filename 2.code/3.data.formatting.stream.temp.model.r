# load time series used before
data.ts <- read.table('1.data/formatted.time.series.1.txt', h = T)
attach(data.ts)
date <- as.POSIXlt(date)

# read information about how to t0 data
t0.data <- read.table('2.ts.shift.estimates/jags.fit.details/0.param.summary.txt', h = T)

# extract values
t0.series <- round(t0.data[which(substring(rownames(t0.data), 1, 2) == 't0'), 5], 2)
names(t0.series) <- c('at', 'lfl', 'wt')

# centering values sin
centering.sin.values <- c(6.25, 18.75)

# t0 apaplied
t0.values <- round(c(-t0.series - centering.sin.values[time.step.option]), 0) 

# t0 time series for model
wt.data.s <- c(wt.data[(t0.values[3] + 1) : length(wt.data)], rep(NA, t0.values[3]))
at.data.s <- c(at.data[(t0.values[1] + 1) : length(at.data)], rep(NA, t0.values[1]))
lfl.data.s <- c(lfl.data[(t0.values[2] + 1) : length(lfl.data)], rep(NA, t0.values[2]))

# 1 or two sets param because forecasting assumed to differ
ifelse(year.end == year.end.forecast , n.sets.parameters <- 1, n.sets.parameters <- 2)

# whether to sets param
ifelse(n.sets.parameters == 1 , 
       set <- rep(1, length(at.data.s)),
       set <- c(rep(1, (year.end - year.start + 1) *  c(24, 73)[time.step.option]),
                rep(2, (year.end.forecast - year.end) *  c(24, 73)[time.step.option]))
)


# ind.6m, give each data point its correspond 6 month period
if(time.step.option == 1){
  ind.6m <- rep(seq(1, (year.end.forecast - year.start + 1) * 2, 1), each = 12)
}else{
  ind.6m <- rep(seq(1, (year.end.forecast - year.start + 1) * 2, 1), 
                rep(c(36, 37), year.end.forecast - year.start + 1))
}

# ac period cutoff times
ac.period.cutoff.1 <- c(seq(1, 
                            (year.end - year.start + 1) * c(24, 73)[time.step.option], 
                            by = c(30, 90)[time.step.option]),
                        seq((year.end - year.start + 1) * c(24, 73)[time.step.option], 
                            (year.end.forecast - year.start + 1) * c(24, 73)[time.step.option], 
                            by = c(30, 90)[time.step.option]),
                        length(at.data.s)
)
ac.period.cutoff.2 <- ac.period.cutoff.1 + c(30, 90)[time.step.option] - 1
ac.period.cutoff <- sort(unique(c(ac.period.cutoff.1,  ac.period.cutoff.2)))
ac.period.cutoff <-  ac.period.cutoff[ac.period.cutoff<= length(at.data.s)]

# n.ac.periods
n.ac.periods <- ceiling(length(at.data.s) / c(30, 90)[time.step.option])
n.6m.windows <- max(ind.6m)
n.t.step.year <- c(24, 73)[time.step.option]

# max autocorr
rho.max <- autocor.incl 
pi.value <- pi
