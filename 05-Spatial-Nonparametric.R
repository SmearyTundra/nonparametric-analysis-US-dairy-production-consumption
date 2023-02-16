library("readxl")
library(sf)
library(maps)
library(ggspatial)
library(tidyr)
library(raster)
library(sp)
library(ggplot2)

source("05-Prepare-data-spatial.R", echo=TRUE)

# Cheese + milk 2007, value
dat1 <- all_sales_county_2007
dat1 <- dat1[-c(1,2,3)]

log_pop <- log(dat1$Population)
log_value <- log(dat1$Value)
log_dat <- as.data.frame(cbind(log_pop,log_value,"x" = dat1$x, "y" = dat1$y))


coordinates(dat1) <- c("y","x")
summary(dat1)


coordinates(log_dat) <- c("y","x")
summary(log_dat)


x11()
scattersplot(dat1)

x11()
scattersplot(log_dat, main = 'Milk and cheese sales', xlab = 'Longitude', ylab = 'Latitude', zlab = 'Sales')


# Trend estimates for log_value
attr_prec <- attributes(precipitation)
border <- attr_prec$border
interior <- attr_prec$interior


x <- coordinates(log_dat)
y <- log_dat$log_value
lp <- locpol(x, y, nbin = c(120, 120), h = diag(c(5, 5)))
lp <- mask(lp, window = border)

attr <- attributes(log_dat)

slim <- range(y)
col <- jet.colors(256)
x11()
simage(lp,  slim = slim, main = "Trend estimates",
       col = col, xlab = "Longitude", ylab = "Latitude", asp = 1)
plot(border, border = "darkgray", lwd = 2, add = TRUE)
plot(interior, border = "lightgray", lwd = 1, add = TRUE)


# Bandwidth selection
bin <- binning(x, y, window = border)
lp0.h <- h.cv(bin)$h
lp0 <- locpol(bin, h = lp0.h, hat.bin = TRUE)
lp0 <- mask(lp0, window = border)
x11()
simage(lp0, slim = slim, main = "Trend estimates",
       col = col, xlab = "Longitude", ylab = "Latitude", asp = 1)
plot(border, border = "darkgray", lwd = 2, add = TRUE)
plot(interior, border = "lightgray", lwd = 1, add = TRUE)


# Variogram estimation
lp0 <- locpol(bin, h = lp0.h, hat.bin = TRUE)
svar.bin <- svariso(x, residuals(lp0), nlags = 40, maxlag = 45)  
svar.h <- h.cv(svar.bin)$h

svar.np <- np.svar(svar.bin, h = svar.h)
svar.np2 <- np.svariso.corr(lp0, nlags = 40, maxlag = 45, 
                            h = svar.h, plot = FALSE)

svm0 <- fitsvar.sb.iso(svar.np, dk = 0) 
svm1 <- fitsvar.sb.iso(svar.np2, dk = 0) 
# plot...
plot(svm1, main = "Nonparametric bias-corrected semivariogram\nand fitted models", 
     legend = FALSE, xlim = c(0,max(coords(svar.np2))), 
     ylim = c(0,max(svar.np2$biny, na.rm = TRUE)))
plot(svm0, lwd = c(1, 1), add = TRUE)
plot(svar.np, type = "p", pch = 2, add = TRUE)
# abline(h = c(svm1$nugget, svm1$sill), lty = 3)
# abline(v = 0, lty = 3)
legend("bottomright", legend = c("corrected", 'biased'),
       lty = c(1, 1), pch = c(1, 2), lwd = c(1, 1))


# Automatic modelling
geomod <- np.fitgeo(x, y, nbin = c(30, 30), maxlag = 45, svm.resid = TRUE, window = border)
x11()
plot(geomod)


# Kriging
krig.grid <- np.kriging(geomod, ngrid = c(120, 120))
# Plot kriging predictions and kriging standard deviations
old.par <- par(mfrow = c(1,2), omd = c(0.0, 0.98, 0.0, 1))
krig.grid2 <- krig.grid
simage(krig.grid2, 'kpred', main = 'Kriging predictions', slim = slim,
       xlab = "Longitude", ylab = "Latitude" , col = col, asp = 1)
plot(border, border = "darkgray", lwd = 2, add = TRUE)
plot(interior, border = "lightgray", lwd = 1, add = TRUE)
simage(krig.grid2, 'ksd', main = 'Kriging sd',
       xlab = "Longitude", ylab = "Latitude" , col = hot.colors(256), asp = 1)
plot(border, border = "darkgray", lwd = 2, add = TRUE)
plot(interior, border = "lightgray", lwd = 1, add = TRUE)


