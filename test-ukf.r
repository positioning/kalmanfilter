library(ukfsst)
data(blue.shark)
get.sst.from.server(blue.shark)
fit = kfsst(blue.shark)
### Windows - works for both Windows 7 and XP
fit <- kfsst(blue.shark, bx.a=F, bsst.a=F)
pdf(height=6, width =6, file = 'ukfsst-exe-test.pdf')
plot(fit)
print(fit)
dev.off()

