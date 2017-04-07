rm(list = ls())

# define and set working directory
wd <- "C:/Users/gbal/Desktop/bal.2014.plosone.stream.temp/2.code"
setwd(wd)

# data file to use
data.file <- '1.raw.data/scorff.river.data.csv'

# define first and last year
year.start <- 1990 # including hindcasting
year.end <- 2006 # including forecating 

# time step option
# 1 15 days, ie two values per month
# 2 5 days, leap year, 29th february data suppressed
time.step <- 1

# data already given according to time step chosen ?
data.formatted <- 0 # 0 no, 1 yes

# model autocorrelation in residual (theloger the time step the less likely)
autocor.incl <- 0 # 0 for no, 1 for yes

# list of packages required
require(lubridate)
require(rjags)

# data formatting
source('1.data.formatting.r')

# find shift parameters of time series
source('shift.estimates.r')