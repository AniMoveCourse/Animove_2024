

## ----message=FALSE, warning=FALSE---------------------------------------------------------------------------
library(marcher)


par(bty="l")
data(Michela)
with(Michela, scan_track(time = time, x = x, y = y))


## -----------------------------------------------------------------------------------------------------------
# first day of first year
day1 <- lubridate::ymd(paste(min(lubridate::year(Michela$time)), 1, 1))

# find time difference and round (and make numeric)
Michela$day <- as.numeric(difftime(Michela$time, day1, unit = "day"))


## ----Michela.fit, cache = FALSE, fig.height = 3, echo = -1--------------------------------------------------
par(bty="l")

#p.m0 <- with(Michela, locate_shift(time = time, x = x, y = y, n.clust = 3))

eval <- FALSE
if(eval){
    p.m0 <- c(x1 = 653.6,  x2 = 658.85, x3 = 653.8, 
               y1 = 5094.8, y2 = 5096,   y3 = 5094.8, 
               t1 = 118, t2 = 318, dt1 = 10.5, dt2 = 15.8)
}

with(Michela, locate_shift(day, x, y, n.clust = 3))

Michela.fit <- with(Michela, 
                    estimate_shift(day, x, y, n.clust = 3, 
                                   model = "ou", method = "like", 
                                   p.m0 = p.m0))
plot(Michela.fit)


## ----message = FALSE, warning = FALSE-----------------------------------------------------------------------
lubridate::ymd("2005 1 1") + lubridate::ddays(Michela.fit$p.hat[c('t1','t2')])


## ----FitMichelaEarly, fig.height = 3, echo = -1, cache=TRUE-------------------------------------------------
par(bty="l")
#  p.m0 <- with(subset(Michela, day < 200), locate_shift(time = day, x = x, y = y, n.clust = 3))
p.m0 <- c(x1 = 653.7,  x2 = 659.1, x3 = 659.0, 
          y1 = 5094.8, y2 = 5096.4, y3 = 5095.9, 
					t1 = 120.2, t2 = 138.3, dt1 = 4, dt2 = 6.8)
Michela.fit.early <- with(subset(Michela, day < 200), 
					estimate_shift(day, x, y, n.clust = 3, 
					               model = "ou", method = "like", p.m0 = p.m0))
plot(Michela.fit.early)
summary(Michela.fit.early)


## -----------------------------------------------------------------------------------------------------------
getRSI(Michela.fit, 1, 2)


## -----------------------------------------------------------------------------------------------------------
data.frame(FirstToSecond = getRSI(Michela.fit, 1,2)[,1],
           SecondToThird= getRSI(Michela.fit, 2,3)[,1], 
           FirstToThird = getRSI(Michela.fit, 1,3)[,1])



## ----MichelaAIC---------------------------------------------------------------------------------------------
c(logLik(Michela.fit.early), AIC(Michela.fit.early))


## ----Michela2b.aic------------------------------------------------------------------------------------------
Michela.fit.early2 <- with(subset(Michela, day < 200), 
					estimate_shift(day, x, y, n.clust = 2, 
					               model = "ou", method = "like"))
c(logLik(Michela.fit.early2), AIC(Michela.fit.early2))


## ----Stopover.test------------------------------------------------------------------------------------------
test_stopover(Michela.fit.early)
Michela.fit.early2
