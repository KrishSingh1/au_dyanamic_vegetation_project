


npv <- read.csv("/Users/30044953/Documents/Projects/C3-C4 grass distribution/remote sensing/Sturt Plains/spline_fit_npv.csv")
pv <- read.csv("/Users/30044953/Documents/Projects/C3-C4 grass distribution/remote sensing/Sturt Plains/spline_fit_pv.csv")

npv[,"pv"] <- pv[,"pv"]
npv[,"diff_npv"] <- npv[,'diff']
dat <- npv[,-which(colnames(npv) == "diff")]


plot(dat[,"npv"],col="brown")
points(dat[,'pv'],col="green")


nls(diff_npv ~ k1 * pv - k2 * npv,data=dat,start=list(k1=0.001,k2=0.01),lower=list(0,0),algorithm="port")


# brown as a time-varying lag from green