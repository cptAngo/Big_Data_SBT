
temp <-read.table("jj.dat")
temp_ts<-ts(temp)

#plot
plot.ts(temp_ts)

#уберем тренд, как видно дисперия непостоянна
differences <- diff(temp_ts, differences = 1)
plot.ts(differences)

#посмотрим на логарифм, чуть лучше стало но не идеально конечно
logtime <- log10(temp_ts)
differrence_logtime <- diff(logtime, differences = 1)
plot.ts(differrence_logtime)

#положим что дисперсия постоянна
acf(differrence_logtime)
pacf(differrence_logtime)
arimaj <- arima(differrence_logtime, order = c(0,0,0))
arimaj
arimaj1 <- arima(differrence_logtime, order = c(4,0,0))
arimaj1
arimaj2 <- arima(differrence_logtime, order = c(4,0,1))
arimaj2

# aic = -158.81  ARIMA(0,0,0), aic = -287.7  ARIMA(4,0,0)
# aic = -286.42  ARIMA(4,0,1)
# выбираем ARIMA(4,0,0) т.к. aic меньше

ts.sim_AR4 <- arima.sim(n = 10000, list(ar = c(0.9, -0.5, .2, -.3)))
plot.ts(ts.sim_AR4)
acf(ts.sim_AR4)
pacf(ts.sim_AR4)

ts.sim_AR3 <- arima.sim(n = 10000, list(ar = c(0.9, -0.5, .2)))
plot.ts(ts.sim_AR3)
acf(ts.sim_AR3)
pacf(ts.sim_AR3)

ts.sim_AR2 <- arima.sim(n = 10000, list(ar = c(0.9, -0.5)))
plot.ts(ts.sim_AR2)
acf(ts.sim_AR2)
pacf(ts.sim_AR2)

ts.sim_AR1 <- arima.sim(n = 10000, list(ar = c(0.9)))
plot.ts(ts.sim_AR1)
acf(ts.sim_AR1)
pacf(ts.sim_AR1)

sim_MA4 <- arima.sim(n = 10000, list( ma = c(-1.9, 1.7, -1.5, 1.5)))
plot.ts(sim_MA4)
acf(sim_MA4)
pacf(sim_MA4)

sim_MA3 <- arima.sim(n = 10000, list( ma = c(-1.9, 1.7, -1.5)))
plot.ts(sim_MA3)
acf(sim_MA3)
pacf(sim_MA3)

sim_MA2 <- arima.sim(n = 10000, list( ma = c(-1.9, 1.7)))
plot.ts(sim_MA2)
acf(sim_MA2)
pacf(sim_MA2)

sim_MA1 <- arima.sim(n = 10000, list( ma = c(-1.9)))
plot.ts(sim_MA1)
acf(sim_MA1)
pacf(sim_MA1)

