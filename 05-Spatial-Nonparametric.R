#milk_sales_state <- read.table("data_updated_2021/milk_sales_by_state_2012_2017.csv", 
#                               sep =",", header = T)
#names(milk_sales_state)[6] = "state"
#library(usmap)
#library(ggplot2)
#
#milk_sales_state[which(milk_sales_state$Value == " (D)"), ]$Value = 0
#milk_sales_state$Value = as.numeric(gsub(",", "", milk_sales_state$Value))
#
#library(raster)
#adm <- getData('GADM', country='USA', level=2)
#adm
library("readxl")
library(sf)
library(maps)
library(ggspatial)
library(tidyr)
library(raster)
library(sp)
library(ggplot2)


dat <- st_as_sf(
    milk_sales_county_2012,
    coords = c("y", "x"),
    crs = 4326
)


MainStates <- map_data("state")
p <- ggplot() + 
    geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                  color="black", fill="lightblue" )



p +
    geom_sf(data = dat, aes(size = Value, color = log(Value)))+
    labs(title = "Location",
         color = "Sales",
         size = "Sales")+
    xlab("Longitude")+
    ylab("Latitude")+
    #ggspatial::annotation_north_arrow(location = "br")+
    ggspatial::annotation_scale(location = "bl")



dat1 <- milk_sales_county_2012
dat1 <- dat1[-c(1,2,3)]

log_cows <- log(dat1$Cows)
log_value <- log(dat1$Value)
log_dat <- as.data.frame(cbind(log_cows,log_value,"x" = dat1$x, "y" = dat1$y))


coordinates(dat1) <- c("y","x")
summary(dat1)


coordinates(log_dat) <- c("y","x")
summary(log_dat)



x11()
spoints(dat1)
bubble(dat1,'Cows',do.log=TRUE,key.space='bottom')
x11()
scattersplot(dat1)


x11()
spoints(log_dat)
bubble(log_dat,'log_cows',do.log=TRUE,key.space='bottom')
# Scatterplot per cows loggata
x11()
scattersplot(log_dat)



########################################################################
# Trend estimates for log_cows
attr_prec <- attributes(precipitation)
border <- attr_prec$border
interior <- attr_prec$interior


x <- coordinates(log_dat)
y <- log_dat$log_cows
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


# Smoothing procedures
bin <- binning(x, y, nbin = c(120, 120), window = border)
#lp2 <- locpol(bin, h = diag(c(5, 5)))
lp2 <- locpol(bin, h = diag(c(3, 3)))
x11()
simage(lp2, slim = slim, main = "Trend estimates",
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
svar.bin <- svariso(x, residuals(lp0), nlags = 40, maxlag = 52)  
svar.h <- h.cv(svar.bin)$h

svar.np <- np.svar(svar.bin, h = svar.h)
svar.np2 <- np.svariso.corr(lp0, nlags = 40, maxlag = 52, 
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
geomod <- np.fitgeo(x, y, nbin = c(30, 30), maxlag = 52, svm.resid = TRUE, window = border)
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

par(old.par)

###########################################################

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


# Smoothing procedures
bin <- binning(x, y, nbin = c(120, 120), window = border)
#lp2 <- locpol(bin, h = diag(c(5, 5)))
lp2 <- locpol(bin, h = diag(c(3, 3)))
x11()
simage(lp2, slim = slim, main = "Trend estimates",
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
svar.bin <- svariso(x, residuals(lp0), nlags = 40, maxlag = 52)  
svar.h <- h.cv(svar.bin)$h

svar.np <- np.svar(svar.bin, h = svar.h)
svar.np2 <- np.svariso.corr(lp0, nlags = 40, maxlag = 52, 
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
geomod <- np.fitgeo(x, y, nbin = c(30, 30), maxlag = 52, svm.resid = TRUE, window = border)
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

par(old.par)

##############################################################################
# Estimate in 2017
dat2 <- milk_sales_county_2017
dat2 <- dat2[-c(1,2,3)]

log_cows <- log(dat2$Cows)
log_value <- log(dat2$Value)
log_dat_2 <- as.data.frame(cbind(log_cows,log_value,"x" = dat2$x, "y" = dat2$y))


coordinates(dat2) <- c("y","x")
summary(dat2)


coordinates(log_dat_2) <- c("y","x")
summary(log_dat_2)



x11()
spoints(dat2)
bubble(dat2,'Cows',do.log=TRUE,key.space='bottom')
x11()
scattersplot(dat2)


x11()
spoints(log_dat_2)
bubble(log_dat_2,'log_cows',do.log=TRUE,key.space='bottom')
# Scatterplot per cows loggata
x11()
scattersplot(log_dat_2)

# Trend estimates for log_cows
attr_prec <- attributes(precipitation)
border <- attr_prec$border
interior <- attr_prec$interior


x <- coordinates(log_dat_2)
y <- log_dat_2$log_cows
lp <- locpol(x, y, nbin = c(120, 120), h = diag(c(5, 5)))
lp <- mask(lp, window = border)

attr <- attributes(log_dat_2)

slim <- range(y)
col <- jet.colors(256)
x11()
simage(lp,  slim = slim, main = "Trend estimates",
       col = col, xlab = "Longitude", ylab = "Latitude", asp = 1)
plot(border, border = "darkgray", lwd = 2, add = TRUE)
plot(interior, border = "lightgray", lwd = 1, add = TRUE)


# Smoothing procedures
bin <- binning(x, y, nbin = c(120, 120), window = border)
#lp2 <- locpol(bin, h = diag(c(5, 5)))
lp2 <- locpol(bin, h = diag(c(3, 3)))
x11()
simage(lp2, slim = slim, main = "Trend estimates",
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
svar.bin <- svariso(x, residuals(lp0), nlags = 40, maxlag = 52)  
svar.h <- h.cv(svar.bin)$h

svar.np <- np.svar(svar.bin, h = svar.h)
svar.np2 <- np.svariso.corr(lp0, nlags = 40, maxlag = 52, 
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
geomod <- np.fitgeo(x, y, nbin = c(30, 30), maxlag = 52, svm.resid = TRUE, window = border)
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

par(old.par)



###############################################################################
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


# Smoothing procedures
bin <- binning(x, y, nbin = c(120, 120), window = border)
#lp2 <- locpol(bin, h = diag(c(5, 5)))
lp2 <- locpol(bin, h = diag(c(3, 3)))
x11()
simage(lp2, slim = slim, main = "Trend estimates",
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
x
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

par(old.par)

# Tentativo di predizione puntuale
x1.grid=-100
x2.grid=35
grid=expand.grid(x1.grid,x2.grid)
names(grid) = c("y","x")
predict(geomod, xnew=grid)

c0 = grid.par(
  n = 1,
  min = ,
  max = min + (n - 1) * lag,
  lag = (max - min)/(n - 1),
  dimnames = names(min)
)

c0 = as.matrix(35,-120)
pred = np.kriging(geomod, newx = c0)
pred$kpred


###############################################################################
# Con la popolazione
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
scattersplot(log_dat)


# Trend estimates for log_value
attr_prec <- attributes(precipitation)
border <- attr_prec$border
interior <- attr_prec$interior


x <- coordinates(log_dat)
y <- log_dat$log_pop
lp <- locpol(x, y, nbin = c(120, 120), h = diag(c(5, 5)))
lp <- mask(lp, window = border)

dim(lp$mask)
length(lp$nomask)
dim(lp$data$x)
head(lp$data$x)
saveRDS(lp$data$x, file = "grid_no_masked.rds")

attr <- attributes(log_dat)

slim <- range(y)
col <- jet.colors(256)
x11()
simage(lp,  slim = slim, main = "Trend estimates",
       col = col, xlab = "Longitude", ylab = "Latitude", asp = 1)
plot(border, border = "darkgray", lwd = 2, add = TRUE)
plot(interior, border = "lightgray", lwd = 1, add = TRUE)


# Smoothing procedures
bin <- binning(x, y, nbin = c(120, 120), window = border)
#lp2 <- locpol(bin, h = diag(c(5, 5)))
lp2 <- locpol(bin, h = diag(c(3, 3)))
x11()
simage(lp2, slim = slim, main = "Trend estimates",
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

par(old.par)
