CSV <- read.csv("../../DATA/Giulia Cenzi/23_215717_stc.csv")
DATA <- as.telemetry(CSV)

COL <- color(DATA,by="time")
plot(DATA,col=COL)

plot(DATA$x,DATA$y,type='l')

?Gmedian::kGmedian

# spatial clustering (k=2)
R <- data.frame(DATA[,c('x','y')])
CLUSTER <- Gmedian::kGmedian(R,ncenters=2,nstart=10,iter.max=.Machine$integer.max)
names(CLUSTER)
CLUSTER$centers # medians
CLUSTER$cluster # membership

plot(DATA$timestamp,CLUSTER$cluster,type='o')

COL <- rainbow(2)[CLUSTER$cluster]
plot(DATA,col=COL)

# space-time clustering (k=2)
R <- as.matrix(DATA[,c('t','x','y')]) # space-time vectors
# must standardize
MAD <- apply(R,2,mad)
R <- t(t(R)/MAD)

CLUSTER <- Gmedian::kGmedian(R,ncenters=2,nstart=10,iter.max=.Machine$integer.max)
COL <- rainbow(2)[CLUSTER$cluster]
plot(DATA,col=COL)

# this choice of standardization fails here
R <- as.matrix(DATA[,c('t','x','y')]) # space-time vectors
v <- median( sqrt(diff(R[,'x'])^2 + diff(R[,'y'])^2)/diff(R[,'t']) )
R[,'t'] <- v * R[,'t']

CLUSTER <- Gmedian::kGmedian(R,ncenters=2,nstart=10,iter.max=.Machine$integer.max)
COL <- rainbow(2)[CLUSTER$cluster]
plot(DATA,col=COL) # bad

# Gaussian mixture models can handle this issue of different scales per dimension,
# but not robustness
# For easily distinguished modes, consider the rebmix package

# For detecting change points in the mean or variance,
# also consider changepoint detection packages, such as mcp
