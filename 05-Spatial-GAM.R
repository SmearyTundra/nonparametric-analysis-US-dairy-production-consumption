library(ISLR2)
library(car)
library(sp)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(devtools) 
library(visreg)
#install_github("mfasiolo/mgcViz")
#library(mgcViz) 

source("10-Prepare_data_spatial_all.R")
#which(milk_sales_county_2017$Cows>=400000)
#milk_sales_county_2017 <- milk_sales_county_2017[-34,]


#### MODEL 1: PREDICT ON INTERACTION BETWEEN COORDINATES
mod2d <- mgcv::gam(log(Value) ~ s(y,x), data = all_sales_county_2007, method = "REML")
summary(mod2d)
par(mfrow=c(1,1))
plot(mod2d,scheme=2,pages=0,select = 1, main = "Smooth coefficients of interaction", xlab = "longitude",ylab = "latitude")

b <- getViz(mod2d)
pl <- plot(sm(b, 1)) + l_fitRaster() + l_fitContour() +
    geom_polygon(data = map_data ("state"), 
                 aes(x=long, y = lat,group=group),
                 fill=NA,color="black",lwd = 0.7,inherit.aes = F) + ggtitle("Smooth coefficients of interaction") + 
  xlab("Longitude") + ylab("Latitude")
pl

ggsave("output/model1_coeff.pdf")
x_grid <- map_data ("state")$long
y_grid <- map_data ("state")$lat

#saveRDS(x_grid, file = "x_grid_prediction_real_cases.rds")
#saveRDS(y_grid, file = "y_grid_prediction_real_cases.rds")

# Make the perspective plot with error surfaces
vis.gam(mod2d, view = c("y", "x"), 
        plot.type = "persp", se = 2)
# Rotate the same plot
vis.gam(mod2d, view = c("y", "x"),
        plot.type = "persp", se = 2, theta = 135)
# Make plot with 10% extrapolation
vis.gam(mod2d, view = c("y", "x"),
        plot.type = "contour", too.far = 0.25)
dat1 <- all_sales_county_2007
coordinates(dat1) <- c("y","x")
points(dat1)





#### MODEL 2: PREDICT ON INTERACTION BETWEEN COORDINATES AND POPULATION
# Log works better, y alone not significative, x yes
tensor_mod2 <- gam(log(Value) ~ s(y, x)+s(log(Population)), 
                  data = all_sales_county_2007, method = "REML")

# Summarize and plot
summary(tensor_mod2)
plot(tensor_mod2,pages=1)
par(mfrow=c(1,2))
plot(tensor_mod2,scheme=2,pages=0,select = 1, main = "Smooth coefficients of interaction", xlab = "longitude",ylab = "latitude")
plot(tensor_mod2,scheme=2,pages=0,select = 2, main = "Population effect", ylab = "s(log(Population))")

plot(tensor_mod2, scheme = 2, pages =0,select=1)

par(mfrow=c(1,1))
plot(tensor_mod2,scheme=2,pages=0,select = 1, main = "Smooth coefficients of interaction", xlab = "longitude",ylab = "latitude")

b <- getViz(tensor_mod2)
pl <- plot(sm(b, 1)) + l_fitRaster() + l_fitContour() +
  geom_polygon(data = map_data ("state"), 
               aes(x=long, y = lat,group=group),
               fill=NA,color="black",lwd = 0.7,inherit.aes = F) + ggtitle("Smooth coefficients of interaction") + 
  xlab("Longitude") + ylab("Latitude")
pl

ggsave("output/model2_coeff.pdf")

# Make the perspective plot with error surfaces
vis.gam(tensor_mod2, view = c("y", "x"), 
        plot.type = "persp", se = 2)
# Rotate the same plot
vis.gam(tensor_mod2, view = c("y", "x"),
        plot.type = "persp", se = 2, theta = 135)
# Make plot with 10% extrapolation
vis.gam(tensor_mod2, view = c("y", "x"),
        plot.type = "contour", too.far = 0.25)
points(dat1)

visreg(tensor_mod2)




######### PREDICTION
library(ggplot2)
x1.grid=-100
x2.grid=35
grid=expand.grid(x1.grid,x2.grid)
names(grid) = c("y","x")
predict(mod2d, newdata=grid)



x_grid <- readRDS("x_grid_prediction_real_cases.rds")
y_grid <- readRDS("y_grid_prediction_real_cases.rds")
grid <- data.frame(x=y_grid,y=x_grid)

preds <- predict(mod2d,newdata=grid)
preds

df <- cbind(x_grid,y_grid,as.data.frame(preds))
ggplot(as.data.frame(df), aes(x_grid, y_grid, col=preds)) +
geom_point(alpha=0.5) +
scale_color_gradient(low="red", high="yellow")  +
    geom_polygon( data=map_data("state"), aes(x=long, y=lat, group=group),
                  color="black", fill="lightblue", alpha=0.1 ) 
    
grid <- readRDS("grid_no_masked.rds")
grid <- data.frame(x=grid[,2],y=grid[,1])

x_grid <- seq(-118,-104, by = 0.01)
y_grid <- seq(36,43, by = 0.01)
grid <- expand.grid(x_grid,y_grid)
grid <- data.frame(x=grid$Var2, y=grid$Var1)
preds <- predict(mod2d,newdata=grid)
preds
df <- cbind(grid,"log(Sales)" =as.data.frame(preds))
ggplot(as.data.frame(df), aes(y,x, col=preds)) +
    geom_point(alpha=0.5) +
    scale_color_gradient(low="red", high="yellow")  +
    coord_cartesian(xlim=c(-115.5, -108), ylim = c(37, 42))+
    geom_polygon( data=map_data("county"), aes(x=long, y=lat, group=group),
                  color="black", fill="lightblue", alpha=0.1 ) +
    guides(color = guide_legend(title = "log(Sales)"))
ggsave("output/utah_preds.pdf", width = 5, height = 5)
