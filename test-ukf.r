###=============================================================================================
### UKFSST
###=============================================================================================
library(ukfsst)
### Fit to data  
data(blue.shark)
get.sst.from.server(blue.shark)
fit <- kfsst(blue.shark, bx.a=F, bsst.a=F)   ### Windows - works for both Windows 7 and XP
### Plotting to file
pdf(height=6, width =6, file = 'ukfsst-exe-test.pdf')
plot(fit)
print(fit)
dev.off()
