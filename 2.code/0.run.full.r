rm(list = ls())

# define and set working directory
wd <- "C:/Users/gbal/Desktop/bal.2014.plosone.stream.temp/2.code"
setwd(wd)

# data file to use
data.file <- '1.raw.data/scorff.river.data.csv'

# define first and last year
year.start <- 1995 # including hindcasting
year.end <- 2006 # year end of historical data
year.end.forecast <- 2006 # including forecating, option in case simulated air data might bias estiamtes somehow, set to same as year.end is no reason

# time step option
# 1 15 days, ie two values per month
# 2 5 days, leap year, 29th february data suppressed
time.step.option <- 2

# data already given according to time step chosen ?
data.formatted <- 0 # 0 no, 1 yes

# model autocorrelation in residual (the longer the time step the less likely)
autocor.incl <- 1 # 0 for no, 1 for yes

# mcmc param first model
mcmc.burn.1 <- as.integer(2000)
mcmc.length.1 <- as.integer(4000)  
mcmc.thin.1 = 5
mcmc.chains.1 = 3 # needs

# mcmc param first model
mcmc.burn.2 <- as.integer(3000)
mcmc.length.2 <- as.integer(6000)  
mcmc.thin.2 = 5
mcmc.chains.2 = 3 # needs

# list of packages required
require(lubridate)
require(R2jags)

# data formatting
if(data.formatted == 0) source('1.data.formatting.base.r')

# find shift parameters of time series
source('2.run.ts.shift.model.r')

# remove to elements to improve memory space
rm(list = ls()[!ls() %in% c('wd', 'time.step.option',
                            'year.start', 'year.end', 'year.end.forecast',
                            'mcmc.burn.2', 'mcmc.length.2', 'mcmc.thin.2', 'mcmc.chains.2',
                            'pi.value', 'autocor.incl')])

source('3.data.formatting.stream.temp.model.r')

source('4.run.stream.temp.model.r')