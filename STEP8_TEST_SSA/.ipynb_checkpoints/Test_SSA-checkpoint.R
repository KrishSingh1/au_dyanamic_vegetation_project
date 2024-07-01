##### Testing SSA #####
## Krish Singh
## 31012024


# Library -----------------------------------------------------------------

library(Rssa)
library(data.table)
library(xts)

# Functions ---------------------------------------------------------------


# Main --------------------------------------------------------------------

test.fc <- fread("../DATASETS/DEA_FC_PROCESSED/SPATIAL_AND_UE_FILTER/QDAEIU0010.csv")
time.ob <- xts(test.fc$pv, test.fc$time)
s <- ssa(time.ob)

ds <- reconstruct(s)
summary(ds)

plot(time.ob)
plot(ds$F1)
plot(ds$F2)
plot(ds$F3)
plot(ds$F4)
plot(ds$F5)
plot(ds$F6)
plot(ds$F7)
plot(ds$F8)
plot(ds$F9)
plot(ds$F10)
