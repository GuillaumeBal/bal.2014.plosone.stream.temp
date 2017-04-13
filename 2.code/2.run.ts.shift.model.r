jags.data.1 <- list('wt.data',
                    'at.data',
                    'lfl.data',
                    'n.time.steps.1',
                    'n.time.steps.year.1',
                    'pi.value')

jags.params.1 <- c('alpha.wt', 'beta.wt', 't0.wt', 'sd.wt',
                   'alpha.at', 'beta.at', 't0.at', 'sd.at',
                   'alpha.lfl', 'beta.lfl', 't0.lfl', 'sd.lfl')


model.loc.1 <- "2.ts.shift.estimates/model.t0.estimates.txt"

### inits gen
jags.inits.1 <- function(){
  list(
    # water temp
    "alpha.wt" = quantile(wt.data, probs = runif(1, 0.25, 0.75), na.rm = TRUE),
    "beta.wt" = (quantile(wt.data, probs = 0.975, na.rm = TRUE) - quantile(wt.data, probs = 0.025, na.rm = TRUE)) / 2 + rnorm(1, 1, 1),
    "tau.wt" = runif(1, 1 / (2 ^ 2), 1 / (0.5 ^ 2)),
    # air temp
    "alpha.at" = quantile(at.data, probs = runif(1, 0.25, 0.75), na.rm = TRUE),
    "beta.at" = (quantile(at.data, probs = 0.975, na.rm = TRUE) - quantile(at.data, probs = 0.025, na.rm = TRUE)) / 2 + rnorm(1, 1, 1),
    "tau.at" = runif(1, 1 / (2 ^ 2), 1 / (0.5 ^ 2)),
    # flow
    "alpha.lfl" = quantile(lfl.data, probs = runif(1, 0.25, 0.75), na.rm = TRUE),
    "beta.lfl" = (quantile(lfl.data, probs = 0.975, na.rm = TRUE) - quantile(lfl.data, probs = 0.025, na.rm = TRUE)) / 2 + rnorm(1, .3, .3),
    "tau.lfl" = runif(1, 1 / (1 ^ 2), 1 / (.1 ^ 2))
  )
}

# run model
jags.outputs.1 <- jags(jags.data.1, parameters.to.save = jags.params.1, model.file = model.loc.1, 
                     n.chains = mcmc.chains.1, n.burnin = mcmc.burn.1, n.thin = mcmc.thin.1, n.iter = mcmc.length.1,
                     refresh = mcmc.burn.1 / 20,
                     inits = jags.inits.1,
                     DIC = TRUE)
# print outputs
print(jags.outputs.1)

# put mcmc chains into r memory 
attach.jags(jags.outputs.1)

# visual check of Rhat values 
hist(jags.outputs.1$BUGSoutput$summary[ ,'Rhat'], breaks = length(jags.params.1),
     main = 'Rhat values, issue if values above 1.10')
abline(v = 1.10, col = 'red')

# save summary outputs
write.table(jags.outputs.1$BUGSoutput$summary, 'ts.shift.outputs.txt')

# add rough curves on time series plots
par(mfrow = c(3, 1),
    mar = c(2, 4, 2, 2),
    oma = c(1, .5, 0.5, .5))
plot(wt.data, type = 'l', main = 'Used data')
lines(mean(alpha.wt) + mean(beta.wt) * sin(2 * pi * (1:length(wt.data) + mean(t0.wt)) / n.time.steps.year.1), 
      col = 'red')
plot(at.data, type = 'l')
lines(mean(alpha.at) + mean(beta.at) * sin(2 * pi * (1:length(at.data) + mean(t0.at)) / n.time.steps.year.1), 
      col = 'red')
plot(lfl.data, type = 'l')
lines(mean(alpha.lfl) - mean(beta.lfl) * sin(2 * pi * (1:length(lfl.data) + mean(t0.lfl)) / n.time.steps.year.1), 
      col = 'red')


# more checks and save ----------------------------------------------------------------------------------

save.folder.1 <- '2.ts.shift.estimates/jags.fit.details/'
dir.create(save.folder.1)

write.table(jags.outputs.1$BUGSoutput$summary, file = paste0(save.folder.1, "0.param.summary.txt"))

list.var.1 <- c(dimnames(jags.outputs.1$BUGSoutput$sims.array)[3])[[1]] #list des var
list.var.1 <- gsub("[^[:alnum:]]", "", list.var.1)

for (i in 1:dim(jags.outputs.1$BUGSoutput$sims.array)[3]){
  assign(paste0(list.var.1[i], "1"), mcmc(jags.outputs.1$BUGSoutput$sims.array[ , 1, i]))
  assign(paste0(list.var.1[i], "2"), mcmc(jags.outputs.1$BUGSoutput$sims.array[ , 2, i]))
  assign(paste0(list.var.1[i], "3"), mcmc(jags.outputs.1$BUGSoutput$sims.array[ , 3, i]))
  assign(list.var.1[i], mcmc.list(list(eval(parse(text = paste0(list.var.1[i], "1"))), 
                                     eval(parse(text = paste0(list.var.1[i], "2"))),
                                     eval(parse(text = paste0(list.var.1[i], "3"))))))
  write.table(eval(parse(text = paste0(list.var.1[i], "3"))),file = paste0(save.folder.1, list.var.1[i], ".txt"))
  #uncomment previous line if you wanna store mcmc chain
}

### list.var.1 minus residuals, dont' include rsiduals in the following to get bayesian diagnoses figures quicker
list.var.1.minus.res <- list.var.1[which(substring(list.var.1, 1, 3) != 'res')]

### trace gelman plot and save in a pdf
pdf(file = paste0(save.folder.1, "/bgr.pdf"), onefile = TRUE, height = 8.25, width = 11.6)
for (i in 1:length(list.var.1.minus.res)){
  gelman.plot(eval(parse(text = list.var.1.minus.res[i])), main = list.var.1.minus.res[i])
}
dev.off()

### trace autocor plot and save in a pdf
pdf(file= paste0(save.folder.1, "/ac.pdf"), onefile = TRUE, height = 8.25, width = 11.6)
for (i in 1:dim(jags.outputs.1$BUGSoutput$sims.array)[3]){
  autocorr.plot(eval(parse(text = list.var.1[i])),main = list.var.1[i])
}
dev.off()

#trace density plot and save in a pdf
pdf(file = paste0(save.folder.1, "/density.pdf"), onefile=TRUE, height = 8.25, width = 11.6)
for (i in 1:dim(jags.outputs.1$BUGSoutput$sims.array)[3]){
  densplot(eval(parse(text = list.var.1[i])), main = list.var.1[i])
}
dev.off()

#trace history plot and save in a pdf
pdf(file = paste0(save.folder.1, "/history.pdf"), onefile = TRUE, height = 8.25, width = 11.6)
for (i in 1:dim(jags.outputs.1$BUGSoutput$sims.array)[3]){
  traceplot(eval(parse(text = list.var.1[i])),main = list.var.1[i])
}
dev.off()