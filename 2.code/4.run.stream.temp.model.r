jags.data.2 <- list('wt.data.s',
                    'at.data.s',
                    'lfl.data.s',
                    'n.t.step.year', 
                    'ind.6m', 'n.6m.windows', 
                    'ac.period.cutoff', 'n.ac.periods',
                    'pi.value',
                    'rho.max',
                    'n.sets.parameters', 'set'
)

jags.params.2 <- c('alpha.at', 'beta.at', 't0.at', 'sigma.at', 'rho.at',
                   'alpha.lfl', 'beta.lfl', 't0.lfl', 'sigma.lfl', 'rho.lfl',
                   'alpha.wt', 'beta.wt', 't0.wt', 'sigma.wt', 'rho.wt',
                   'theta.0', 'theta.1', 'theta.2', 'theta.0.p', 'theta.1.p', 'theta.2.p',
                   'sigma.max.wt', 'sigma.min.wt')


model.loc.2 <- "3.stream.temp.modeling/wt.ts.model.txt"

### inits gen
jags.inits.2 <- function(){
  list(
    "alpha.at" = quantile(at.data, probs = runif(n.6m.windows, 0.25, 0.75), na.rm = TRUE),
    "beta.at" = (quantile(at.data, probs = 0.975, na.rm = TRUE) - quantile(at.data, probs = 0.025, na.rm = TRUE)) / 2 + 
      rnorm(n.6m.windows, 1, 1),
    "tau.at" = rep(runif(1, 1 / (2 ^ 2), 1 / (0.5 ^ 2)), n.sets.parameters),
    'rho.at' = runif(n.sets.parameters, 0.2, 0.6) * autocor.incl,
    "alpha.lfl" = quantile(lfl.data, probs = runif(n.6m.windows, 0.25, 0.75), na.rm = TRUE),
    "beta.lfl" = (quantile(lfl.data, probs = 0.975, na.rm = TRUE) - quantile(lfl.data, probs = 0.025, na.rm = TRUE)) / 2 + 
      rnorm(n.6m.windows, .3, .3),
    "tau.lfl" = rep(runif(1, 1 / (2 ^ 2), 1 / (0.5 ^ 2)), n.sets.parameters),
    'rho.lfl' = runif(n.sets.parameters, 0.2, 0.6) * autocor.incl 
  )
}

# run model
jags.outputs.2 <- jags(jags.data.2, parameters.to.save = jags.params.2, model.file = model.loc.2, 
                       n.chains = mcmc.chains.2, n.burnin = mcmc.burn.2, n.thin = mcmc.thin.2, n.iter = mcmc.length.2,
                       refresh = mcmc.burn.2 / 20,
                       inits = jags.inits.2,
                       DIC = TRUE)

# print outputs
print(jags.outputs.2)

# put mcmc chains into r memory 
attach.jags(jags.outputs.2)

# visual check of Rhat values 
hist(jags.outputs.2$BUGSoutput$summary[ , 'Rhat'], breaks = length(jags.params.2),
     main = 'Rhat values, issue if values above 1.10')
abline(v = 1.10, col = 'red')

# more checks and save ----------------------------------------------------------------------------------

save.folder.2 <- '3.stream.temp.modeling/jags.fit.details/'
dir.create(save.folder.2)

write.table(jags.outputs.2$BUGSoutput$summary, file = paste0(save.folder.2, "0.param.summary.txt"))

list.var.2 <- c(dimnames(jags.outputs.2$BUGSoutput$sims.array)[3])[[1]] #list des var
list.var.2 <- gsub("[^[:alnum:]]", "", list.var.2)

for (i in 1:dim(jags.outputs.2$BUGSoutput$sims.array)[3]){
  assign(paste0(list.var.2[i], "1"), mcmc(jags.outputs.2$BUGSoutput$sims.array[ , 1, i]))
  assign(paste0(list.var.2[i], "2"), mcmc(jags.outputs.2$BUGSoutput$sims.array[ , 2, i]))
  assign(paste0(list.var.2[i], "3"), mcmc(jags.outputs.2$BUGSoutput$sims.array[ , 3, i]))
  assign(list.var.2[i], mcmc.list(list(eval(parse(text = paste0(list.var.2[i], "1"))), 
                                     eval(parse(text = paste0(list.var.2[i], "2"))),
                                     eval(parse(text = paste0(list.var.2[i], "3"))))))
  write.table(eval(parse(text = paste0(list.var.2[i], "3"))),file = paste0(save.folder.2, list.var.2[i], ".txt"))
  #uncomment previous line if you wanna store mcmc chain
}

### list.var.2 minus residuals, dont' include rsiduals in the following to get bayesian diagnoses figures quicker
list.var.2.minus.res <- list.var.2[which(substring(list.var.2, 1, 3) != 'res')]

### trace gelman plot and save in a pdf
pdf(file = paste0(save.folder.2, "/bgr.pdf"), onefile = TRUE, height = 8.25, width = 11.6)
for (i in 1:length(list.var.2.minus.res)){
  gelman.plot(eval(parse(text = list.var.2.minus.res[i])), main = list.var.2.minus.res[i])
}
dev.off()

### trace autocor plot and save in a pdf
pdf(file= paste0(save.folder.2, "/ac.pdf"), onefile = TRUE, height = 8.25, width = 11.6)
for (i in 1:dim(jags.outputs.2$BUGSoutput$sims.array)[3]){
  autocorr.plot(eval(parse(text = list.var.2[i])),main = list.var.2[i])
}
dev.off()

#trace density plot and save in a pdf
pdf(file = paste0(save.folder.2, "/density.pdf"), onefile=TRUE, height = 8.25, width = 11.6)
for (i in 1:dim(jags.outputs.2$BUGSoutput$sims.array)[3]){
  densplot(eval(parse(text = list.var.2[i])), main = list.var.2[i])
}
dev.off()

#trace history plot and save in a pdf
pdf(file = paste0(save.folder.2, "/history.pdf"), onefile = TRUE, height = 8.25, width = 11.6)
for (i in 1:dim(jags.outputs.2$BUGSoutput$sims.array)[3]){
  traceplot(eval(parse(text = list.var.2[i])),main = list.var.2[i])
}
dev.off()