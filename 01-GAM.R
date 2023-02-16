rm(list = ls())
data <- read.table('data_updated_2021/production_facts.csv', header=T, sep = ';')
data_infl <- read.table('data_updated_2021/production_facts_inflated.csv', header=T, sep = ';')
infl <- read.table('data_updated_2021/inflation.csv', header=F, sep = ';')
output_path = file.path("output")

library(tidyverse)
library(ggplot2)

#### Regression####

# no inflation
x11()
pdf(file=file.path(output_path,"01_exploration_milkprice_years.pdf"), width = 12, height = 6)
par(mfrow = c(1, 1))
with(
  data_infl,
  plot(
    year,
    avg_price_milk,
    xlab = 'Year',
    ylab = 'Average Milk Price',
    main = 'Average Milk Price over years',
    pch = 16
  )
)
with(data_infl, matlines(year, avg_price_milk, type = 'l'))
grid()
dev.off()


x11()
par(mfrow = c(2,4))
plot(data$avg_milk_cow_number,data$avg_price_milk)
plot(data$milk_per_cow,data$avg_price_milk)
plot(data$milk_production_lbs,data$avg_price_milk)
plot(data$dairy_ration,data$avg_price_milk)
plot(data$milk_feed_price_ratio,data$avg_price_milk)
plot(data$milk_cow_cost_per_animal,data$avg_price_milk)
plot(data$alfalfa_hay_price,data$avg_price_milk)
plot(data$slaughter_cow_price,data$avg_price_milk)


pdf(file=file.path(output_path,"01_exploration_milkprice_allbutyear.pdf"), width = 12, height = 8)
data_infl %>%
  select(-year) %>%
  gather(key = 'var', value = 'value', -avg_price_milk) %>%
  ggplot(aes(x = value, y = avg_price_milk))+
  geom_point()+
  stat_smooth() +
  facet_wrap(~ var, scales = "free")
dev.off()
  


regr=lm(avg_price_milk~.,data=data)
summary(regr)    #year is not influent
# inflation
x11()
par(mfrow = c(1,2))
plot(data_infl$year,data_infl$avg_price_milk)
matplot(data_infl$year,data_infl$avg_price_milk,type='l')
x11()
par(mfrow = c(2,4))
plot(data_infl$avg_milk_cow_number,data_infl$avg_price_milk)
plot(data_infl$milk_per_cow,data_infl$avg_price_milk)
plot(data_infl$milk_production_lbs,data_infl$avg_price_milk)
plot(data_infl$dairy_ration,data_infl$avg_price_milk)
plot(data_infl$milk_feed_price_ratio,data_infl$avg_price_milk)
plot(data_infl$milk_cow_cost_per_animal,data_infl$avg_price_milk)
plot(data_infl$alfalfa_hay_price,data_infl$avg_price_milk)
plot(data_infl$slaughter_cow_price,data_infl$avg_price_milk)

regr=lm(avg_price_milk~.,data=data_infl)
summary(regr) 
####GAM####
library(mgcv)
model_gam2=gam(avg_price_milk ~ avg_milk_cow_number + s(milk_per_cow,bs='cr') 
              + dairy_ration + milk_feed_price_ratio
              + milk_cow_cost_per_animal+ milk_volume_to_buy_cow_in_lbs
              + s(alfalfa_hay_price,bs='cr')+ s(slaughter_cow_price,bs='cr')
              ,data = data)
summary(model_gam2)
x11()
par(mfrow = c(1,2))
hist(model_gam2$residuals)
qqnorm(model_gam2$residuals)
shapiro.test(model_gam2$residuals) #nice
x11()
par(mfrow = c(1,3))
plot(model_gam2)


model_gam3=gam(avg_price_milk ~ avg_milk_cow_number + s(milk_per_cow,bs='cr') 
               + dairy_ration + milk_cow_cost_per_animal
               + milk_volume_to_buy_cow_in_lbs+ s(slaughter_cow_price,bs='cr')
               ,data = data)
summary(model_gam3)
x11()
par(mfrow = c(1,2))
hist(model_gam3$residuals)
qqnorm(model_gam3$residuals)
shapiro.test(model_gam3$residuals) #nice
x11()
par(mfrow = c(1,2))
plot(model_gam3)

model_gam4=gam(avg_price_milk ~ s(milk_per_cow,bs='cr') + s(slaughter_cow_price,bs='cr'),data = data)
summary(model_gam4)
x11()
par(mfrow = c(1,2))
hist(model_gam4$residuals)
qqnorm(model_gam4$residuals)
shapiro.test(model_gam4$residuals) #nice
x11()
par(mfrow = c(1,2))
plot(model_gam4)

library(rgl)
milk_per_cow.grid=seq(range(data$milk_per_cow)[1],range(data$milk_per_cow)[2],length.out = 100)
slaughter_cow_price.grid=seq(range(data$slaughter_cow_price)[1],range(data$slaughter_cow_price)[2],length.out = 100)
grid=expand.grid(milk_per_cow.grid,slaughter_cow_price.grid)
names(grid)=c('milk_per_cow','slaughter_cow_price')
pred_gam=predict(model_gam4,newdata=grid)
persp3d(milk_per_cow.grid,slaughter_cow_price.grid,pred_gam,col='grey30',xlim = range(data$milk_per_cow))
with(data,points3d(milk_per_cow,slaughter_cow_price,avg_price_milk,col='black',size=5))

grid=expand.grid(milk_per_cow=milk_per_cow.grid, 
                 slaughter_cow_price=slaughter_cow_price.grid,
                 avg_milk_cow_number=mean(data$avg_milk_cow_number),
                 dairy_ration=mean(data$dairy_ration),
                 milk_cow_cost_per_animal=mean(data$milk_cow_cost_per_animal),
                 milk_volume_to_buy_cow_in_lbs=mean(data$milk_volume_to_buy_cow_in_lbs))
pred_gam=predict(model_gam3,newdata=grid)
persp3d(milk_per_cow.grid,slaughter_cow_price.grid,pred_gam,col='grey30',xlim = range(data$milk_per_cow))
with(data,points3d(milk_per_cow,slaughter_cow_price,avg_price_milk,col='black',size=5))

####GAM inflation####
library(mgcv)
model_gam_infl=gam(avg_price_milk ~ s(dairy_ration,bs='cr') + s(avg_milk_cow_number,bs='cr') 
                   + milk_per_cow  # s(milk_per_cow,bs='cr') 
                   #+  slaughter_cow_price #s(slaughter_cow_price,bs='cr')
                   + s(alfalfa_hay_price,bs='cr')
                   + milk_cow_cost_per_animal #s(milk_cow_cost_per_animal,bs='cr')
              ,data = data_infl)
summary(model_gam_infl)
x11()
par(mfrow = c(1,2))
hist(model_gam_infl$residuals)
qqnorm(model_gam_infl$residuals)
shapiro.test(model_gam_infl$residuals) #nice
x11()
par(mfrow = c(1,4))
plot(model_gam_infl)
x <- c()
for(i in 2:dim(data_infl)[1]){
  x <- c(x,log(data_infl$avg_price_milk[i])- log(data_infl$avg_price_milk[i-1]))
}
datinf <- data_infl[-1,]
model_gam_infl2=gam(x ~ s(dairy_ration,bs='cr') + s(avg_milk_cow_number,bs='cr') 
                    #+ milk_per_cow  # s(milk_per_cow,bs='cr') 
                    + s(alfalfa_hay_price,bs='cr')
                    + milk_cow_cost_per_animal #s(milk_cow_cost_per_animal,bs='cr')
               ,data = datinf)
summary(model_gam_infl2)
x11()
par(mfrow = c(1,2))
hist(model_gam_infl2$residuals)
qqnorm(model_gam_infl2$residuals)
shapiro.test(model_gam_infl2$residuals) #nice
x11()
par(mfrow = c(1,4))
plot(model_gam_infl2)
library(splines)
formula = avg_price_milk ~ avg_milk_cow_number +#ns(avg_milk_cow_number, df = 3) + 
  #alfalfa_hay_price+#ns(alfalfa_hay_price, df = 3)+
  milk_per_cow+#ns(milk_per_cow, df = 3)#+
  milk_cow_cost_per_animal+#ns(milk_cow_cost_per_animal, df = 3) #+
  #dairy_ration+#ns(dairy_ration, df = 3) #+
  #slaughter_cow_price+#ns(slaughter_cow_price, df = 3)
  milk_volume_to_buy_cow_in_lbs#+
  #milk_feed_price_ratio
mod <- lm(formula,data = data_infl)
summary(mod)
mod$residuals
x11()
par(mfrow = c(2,4))
gam::plot.Gam(mod, se=TRUE)
fit_lts <- ltsReg(formula ,
                  alpha = .75,
                  mcd = TRUE,
                  data = data_infl)

summary(fit_lts)
data_infl$year[which(abs(fit_lts$resid) > 2.5)]

plot(
  fit_lts$RD,
  fit_lts$resid,
  ylim = c(-3, 4),
  pch = 16,
  main = "Regression Diagnostic Plot",
  xlab = "Robust distance computed by MCD",
  ylab = "Standardized LTS residual"
)
abline(h = c(-2.5, 2.5), v = thresh, lwd = 2)
text(
  fit_lts$RD,
  fit_lts$resid,
  labels = ifelse(abs(fit_lts$resid) > 2.5 |
                    fit_lts$RD > thresh, data_infl$year, ""),
  pos = 1
)
thresh = sqrt(qchisq(0.975, ncol(data_infl)))

model_gam_infl3=gam(avg_price_milk ~ avg_milk_cow_number + s(milk_per_cow,bs='cr') 
               + dairy_ration + milk_cow_cost_per_animal
               + milk_volume_to_buy_cow_in_lbs+ s(slaughter_cow_price,bs='cr')
               ,data = data_infl)
summary(model_gam_infl3)
x11()
par(mfrow = c(1,2))
hist(model_gam_infl3$residuals)
qqnorm(model_gam_infl3$residuals)
shapiro.test(model_gam_infl3$residuals) #nice
x11()
par(mfrow = c(1,2))
plot(model_gam_infl3)

model_gam_infl4=gam(avg_price_milk ~ s(milk_per_cow,bs='cr') + s(slaughter_cow_price,bs='cr'),data = data_infl)
summary(model_gam_infl4)
x11()
par(mfrow = c(1,2))
hist(model_gam_infl4$residuals)
qqnorm(model_gam_infl4$residuals)
shapiro.test(model_gam_infl4$residuals) #nice
x11()
par(mfrow = c(1,2))
plot(model_gam_infl4)

library(rgl)
milk_per_cow.grid=seq(range(data_infl$milk_per_cow)[1],range(data_infl$milk_per_cow)[2],length.out = 100)
slaughter_cow_price.grid=seq(range(data_infl$slaughter_cow_price)[1],range(data_infl$slaughter_cow_price)[2],length.out = 100)
grid=expand.grid(milk_per_cow.grid,slaughter_cow_price.grid)
names(grid)=c('milk_per_cow','slaughter_cow_price')
pred_gam=predict(model_gam_infl4,newdata=grid)
persp3d(milk_per_cow.grid,slaughter_cow_price.grid,pred_gam,col='grey30',xlim = range(data_infl$milk_per_cow))
with(data_infl,points3d(milk_per_cow,slaughter_cow_price,avg_price_milk,col='black',size=5))

grid=expand.grid(milk_per_cow=milk_per_cow.grid, 
                 slaughter_cow_price=slaughter_cow_price.grid,
                 avg_milk_cow_number=mean(data_infl$avg_milk_cow_number),
                 dairy_ration=mean(data_infl$dairy_ration),
                 milk_cow_cost_per_animal=mean(data_infl$milk_cow_cost_per_animal),
                 milk_volume_to_buy_cow_in_lbs=mean(data_infl$milk_volume_to_buy_cow_in_lbs))
pred_gam=predict(model_gam_infl3,newdata=grid)
persp3d(milk_per_cow.grid,slaughter_cow_price.grid,pred_gam,col='grey30',xlim = range(data_infl$milk_per_cow))
with(data_infl,points3d(milk_per_cow,slaughter_cow_price,avg_price_milk,col='black',size=5))


####GAM inflation FINAL####

model_gam_infl5=gam(avg_price_milk ~   s(dairy_ration,bs='cr')
                    + milk_cow_cost_per_animal + milk_volume_to_buy_cow_in_lbs
                    + milk_feed_price_ratio + s(milk_per_cow,bs='cr'),data = data_infl)
tab = summary(model_gam_infl5)
format(as.data.frame(tab$p.coeff),scientific=FALSE)




x11()
par(mfrow = c(1,2))
hist(model_gam_infl5$residuals)
qqnorm(model_gam_infl5$residuals)
qqline(model_gam_infl5$residuals, col = 2,lwd=2,lty=2)
shapiro.test(model_gam_infl5$residuals) #nice
x11()
par(mfrow = c(1,2))
plot(model_gam_infl5)

milk_per_cow.grid=seq(range(data_infl$milk_per_cow)[1],range(data_infl$milk_per_cow)[2],length.out = 100)
dairy_ration.grid=seq(range(data_infl$dairy_ration)[1],range(data_infl$dairy_ration)[2],length.out = 100)
grid=expand.grid(milk_per_cow=milk_per_cow.grid, 
                 dairy_ration=dairy_ration.grid,
                 milk_feed_price_ratio=mean(data_infl$milk_feed_price_ratio),
                 milk_cow_cost_per_animal=mean(data_infl$milk_cow_cost_per_animal),
                 milk_volume_to_buy_cow_in_lbs=mean(data_infl$milk_volume_to_buy_cow_in_lbs))
pred_gam=predict(model_gam_infl5,newdata=grid)

pdf(file=file.path(output_path,"01_gam_surf.pdf"), width = 12, height = 8)
plot3D::persp3D(
  x=milk_per_cow.grid,
  y=dairy_ration.grid,
  z=matrix(pred_gam, nrow=length(milk_per_cow.grid), ncol=length(dairy_ration.grid)),
  col.palette = heat.colors,
  #xlim = range(data_infl$milk_per_cow),
  xlab = 'milk_per_cow',
  ylab = 'dairy_ration',
  zlab = 'avg_price_milk',
  box = TRUE,
  #contour = TRUE,
  border='black',
  lwd=0.1,
  shade=0.1,
  bty="b2", # https://rdrr.io/cran/plot3D/man/perspbox.html
  phi = 20, theta = 50
)
dev.off()


with(
  data_infl,
  plot3D::points3D(
    milk_per_cow,
    dairy_ration,
    avg_price_milk,
    col = 'black',
    size = 1,
    pch=16,
    add=TRUE
  )
)



# Save the current viewpoint
view <- par3d("userMatrix")

dput(view)
view <- structure(c(0.907931625843048, 0.267511069774628, -0.322642296552658,
                    0, -0.410978674888611, 0.417272746562958, -0.810543060302734,
                    0, -0.0821993798017502, 0.868516683578491, 0.488796472549438,
                    0, 0, 0, 0, 1), .Dim = c(4L, 4L))

# Restore the saved viewpoint
par3d(userMatrix = view)



rgl.postscript(file.path(output_path, "01_gam.pdf"), fmt = 'pdf')

#### Prediction Pointwise####

model_final=gam(avg_price_milk ~   s(dairy_ration,bs='cr')
                    + milk_cow_cost_per_animal + milk_volume_to_buy_cow_in_lbs
                    + milk_feed_price_ratio + s(milk_per_cow,bs='cr'),data = data_infl)

milk_cow = c(1526.43,1531.21,1436.44)
#bred_heifer_prices = c(1289.58,1254.70,1223.86)
dairy_rat = c(0.12308,0.12732,0.11571)
milk_feed = c(2.467,2.311,2.161)
milk_per_cow.med <- median(data_infl$milk_per_cow)
milk_volume_to_buy_cow_in_lbs.med <- median(data_infl$milk_volume_to_buy_cow_in_lbs)

newdata <- c(milk_per_cow=23745,dairy_ration=0.097,milk_feed_price_ratio=2.01,
             milk_cow_cost_per_animal=2037,milk_volume_to_buy_cow_in_lbs=10000)
#pred_gam=predict(model_final,newdata=newdata)
library(conformalInference)
train_gam = function(x, y, out = NULL) {
  colnames(x) = c('var1', 'var2', 'var3', 'var4', 'var5')
  train_data = data.frame(y, x)
  model_gam = gam(y ~ s(var1, bs = 'cr') + s(var2, bs = 'cr') + var3 + var4 + var5,
                  data = train_data)
}

predict_gam = function(obj, new_x) {
  new_x = data.frame(new_x)
  colnames(new_x) = c('var1','var2','var3','var4','var5')
  predict.gam(obj, new_x)
}
alpha=0.2
n_grid = 200
library(mgcv)
hist(data_infl$avg_price_milk,breaks=10,xlab='Milk Price',main = 'Prediction of milk prices')#,border=NA)
for(i in 1:3){
  newdata <- c(milk_per_cow.med,dairy_rat[i],milk_feed[i],
               milk_cow[i],milk_volume_to_buy_cow_in_lbs.med)
  c_preds = conformal.pred(
    cbind(
      data_infl$milk_per_cow,
      data_infl$dairy_ration,
      data_infl$milk_feed_price_ratio,
      data_infl$milk_cow_cost_per_animal,
      data_infl$milk_volume_to_buy_cow_in_lbs
    ),
    data_infl$avg_price_milk,
    newdata,
    alpha = alpha,
    verbose = T,
    train.fun = train_gam ,
    predict.fun = predict_gam,
    num.grid.pts = n_grid
  )
  
  inter<-c("LOWER" = c_preds$lo,
           "PRED" = c_preds$pred,
           "UPPER" = c_preds$up)
  print(inter)
  jf=1
  set.seed(1)
  if(i==1)
    abline(v=jitter(inter,jf),col=c('darkgray','black','darkgray'),lwd=c(2,3,2))
  if(i==2)
    abline(v=inter,col=c(2,'red',2),lwd=c(2,3,2))
  if(i==3)
    abline(v=inter,col=c('lightblue','blue','lightblue'),lwd=c(2,3,2))
}

hist(data_infl$avg_price_milk,breaks=10)
abline(v=inter)

#### Prediction Ration 2-Dim ####
model_final=gam(avg_price_milk ~   s(dairy_ration,bs='cr')
                + milk_cow_cost_per_animal + milk_volume_to_buy_cow_in_lbs
                + milk_feed_price_ratio + s(milk_per_cow,bs='cr'),data = data_infl)

newdata <- c(milk_per_cow=23745,dairy_ration=0.097,milk_feed_price_ratio=2.01,
           milk_cow_cost_per_animal=2037,milk_volume_to_buy_cow_in_lbs=10000)
dairy_ration.grid=seq(range(data_infl$dairy_ration)[1],range(data_infl$dairy_ration)[2],length.out = 100)

library(pbapply)

wrapper_dairy_ration=function(grid_point){
  newdata_t <- newdata
  newdata_t[2] <- grid_point
  alpha=0.1
  n_grid = 200
  c_preds = conformal.pred(
    cbind(
      data_infl$milk_per_cow,
      data_infl$dairy_ration,
      data_infl$milk_feed_price_ratio,
      data_infl$milk_cow_cost_per_animal,
      data_infl$milk_volume_to_buy_cow_in_lbs
    ),
    data_infl$avg_price_milk,
    newdata_t,
    alpha = alpha,
    verbose = T,
    train.fun = train_gam ,
    predict.fun = predict_gam,
    num.grid.pts = n_grid
  )
  inter<-c("LOWER" = c_preds$lo,
           "PRED" = c_preds$pred,
           "UPPER" = c_preds$up)
  return(inter)
} 

#inter=pbsapply(dairy_ration.grid,wrapper_dairy_ration)

library(parallel)
n_cores <- detectCores()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(DepthProc)))
clusterExport(cl, varlist = list("dairy_ration.grid","wrapper_dairy_ration","newdata","data_infl","gam","predict.gam", "conformal.pred", "train_gam", "predict_gam"))
set.seed(1)
inter=pbsapply(dairy_ration.grid,wrapper_dairy_ration, cl = cl)
stopCluster(cl)

plot(dairy_ration.grid,inter[2,],type='l',ylim=c(0,0.4))
points(dairy_ration.grid,inter[1,],col=2,type='l')
points(dairy_ration.grid,inter[3,],col=3,type='l')

#### Prediction Milkpercow 2-Dim####
model_final=gam(avg_price_milk ~   s(dairy_ration,bs='cr')
                + milk_cow_cost_per_animal + milk_volume_to_buy_cow_in_lbs
                + milk_feed_price_ratio + s(milk_per_cow,bs='cr'),data = data_infl)


newdata <- c(milk_per_cow=23745,dairy_ration=0.097,milk_feed_price_ratio=2.01,
             milk_cow_cost_per_animal=2037,milk_volume_to_buy_cow_in_lbs=10000)
milk_per_cow.grid=seq(range(data_infl$milk_per_cow)[1],range(data_infl$milk_per_cow)[2],length.out = 100)

library(pbapply)

wrapper_milk_per_cow=function(grid_point){
  newdata_t <- newdata
  newdata_t[1] <- grid_point
  alpha=0.1
  n_grid = 200
  c_preds = conformal.pred(
    cbind(
      data_infl$milk_per_cow,
      data_infl$dairy_ration,
      data_infl$milk_feed_price_ratio,
      data_infl$milk_cow_cost_per_animal,
      data_infl$milk_volume_to_buy_cow_in_lbs
    ),
    data_infl$avg_price_milk,
    newdata_t,
    alpha = alpha,
    verbose = T,
    train.fun = train_gam ,
    predict.fun = predict_gam,
    num.grid.pts = n_grid
  )
  inter<-c("LOWER" = c_preds$lo,
           "PRED" = c_preds$pred,
           "UPPER" = c_preds$up)
  return(inter)
} 

#inter=pbsapply(dairy_ration.grid,wrapper_dairy_ration)

library(parallel)
n_cores <- detectCores()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(DepthProc)))
clusterExport(cl, varlist = list("milk_per_cow.grid","wrapper_milk_per_cow","newdata","data_infl","gam","predict.gam", "conformal.pred", "train_gam", "predict_gam"))
set.seed(1)
inter=pbsapply(milk_per_cow.grid,wrapper_milk_per_cow, cl = cl)
stopCluster(cl)

plot(milk_per_cow.grid,inter[2,],type='l',ylim=c(0.15,0.26))
points(milk_per_cow.grid,inter[1,],col=2,type='l')
points(milk_per_cow.grid,inter[3,],col=3,type='l')

#save(inter, file = "ConfIntGam.RData")
#### Prediction 3-Dim####
model_final=gam(avg_price_milk ~   s(dairy_ration,bs='cr')
                + milk_cow_cost_per_animal + milk_volume_to_buy_cow_in_lbs
                + milk_feed_price_ratio + s(milk_per_cow,bs='cr'),data = data_infl)


newdata <- c(milk_per_cow=23745,dairy_ration=0.097,milk_feed_price_ratio=2.01,
             milk_cow_cost_per_animal=2037,milk_volume_to_buy_cow_in_lbs=10000)
milk_per_cow.grid=seq(range(data_infl$milk_per_cow)[1],range(data_infl$milk_per_cow)[2],length.out = 50)
dairy_ration.grid=seq(range(data_infl$dairy_ration)[1],range(data_infl$dairy_ration)[2],length.out = 50)
grid=data.matrix( expand.grid(milk_per_cow=milk_per_cow.grid, 
                 dairy_ration=dairy_ration.grid))
library(pbapply)

wrapper_3D=function(grid_point){
  print(grid_point)
  newdata_s <- newdata
  newdata_s[1] <- grid_point[1]
  newdata_s[2] <- grid_point[2]
  alpha=0.1
  n_grid = 20
  c_preds = conformal.pred(
    cbind(
      data_infl$milk_per_cow,
      data_infl$dairy_ration,
      data_infl$milk_feed_price_ratio,
      data_infl$milk_cow_cost_per_animal,
      data_infl$milk_volume_to_buy_cow_in_lbs
    ),
    data_infl$avg_price_milk,
    newdata_s,
    alpha = alpha,
    verbose = T,
    train.fun = train_gam ,
    predict.fun = predict_gam,
    num.grid.pts = n_grid
  )
  inter<-c("LOWER" = c_preds$lo,
           "PRED" = c_preds$pred,
           "UPPER" = c_preds$up)
  return(inter)
} 

n_cores <- detectCores()
cl = makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(DepthProc)))
clusterExport(cl, varlist = list("grid","wrapper_3D","newdata","data_infl","gam","predict.gam", "conformal.pred", "train_gam", "predict_gam"))
set.seed(1)
inter3D=pbapply(t(grid),MARGIN=2,wrapper_3D,cl = cl)
inter3D
stopCluster(cl)

plot3D::persp3D(
  x=milk_per_cow.grid,
  y=dairy_ration.grid,
  z=matrix(pred_gam, nrow=length(milk_per_cow.grid), ncol=length(dairy_ration.grid)),
  col.palette = heat.colors,
  #xlim = range(data_infl$milk_per_cow),
  xlab = 'milk_per_cow',
  ylab = 'dairy_ration',
  zlab = 'avg_price_milk',
  box = TRUE,
  #contour = TRUE,
  border='black',
  lwd=0.1,
  shade=0.1,
  bty="b2", # https://rdrr.io/cran/plot3D/man/perspbox.html
  phi = 20, theta = 50
)
plot3D::points3D(grid[,1],grid[,2],
    inter3D[3,],
    col = 'green',
    size = 1,
    pch=16,
    add=TRUE
)
plot3D::points3D(grid[,1],grid[,2],
                 inter3D[1,],
   col = 'red',
   size = 1,
   pch=16,
   add=TRUE
)
library(rgl)
milk_per_cow.grid=seq(range(data_infl$milk_per_cow)[1],range(data_infl$milk_per_cow)[2],length.out = 50)
dairy_ration.grid=seq(range(data_infl$dairy_ration)[1],range(data_infl$dairy_ration)[2],length.out = 50)
grid=expand.grid(milk_per_cow=milk_per_cow.grid, 
                 dairy_ration=dairy_ration.grid,
                 milk_feed_price_ratio=newdata[3],
                 milk_cow_cost_per_animal=newdata[4],
                 milk_volume_to_buy_cow_in_lbs=newdata[5])
pred_gam=predict(model_gam_infl5,newdata=grid)
persp3d(
  x=milk_per_cow.grid,
  y=dairy_ration.grid,
  z=matrix(pred_gam, nrow=length(milk_per_cow.grid), ncol=length(dairy_ration.grid)),col='gray')
persp3d(
  milk_per_cow.grid,dairy_ration.grid,
  matrix(inter3D[1,],ncol=50),col = 'red',add = T)  #by true??
persp3d(
  milk_per_cow.grid,dairy_ration.grid,
  matrix(inter3D[3,],ncol=50),col = 'green',add = T)

#### Bootstrap ####
model_gam = gam(
  avg_price_milk ~ s(dairy_ration, bs = 'cr')
  + milk_cow_cost_per_animal + milk_volume_to_buy_cow_in_lbs
  + milk_feed_price_ratio + s(milk_per_cow, bs = 'cr'),
  data = data_infl
)
diagnostic_bootstrap = function(distro, obs){
  print(paste("Standard deviation: ", sd(distro)))
  print(paste("Bias: ", mean(distro) - obs))
  # computing confidence interval
  alpha <- 0.1
  right.quantile <- quantile(distro, 1 - alpha/2)
  left.quantile <- quantile(distro, alpha/2)
  # reverse-percentile
  CI <- c(obs - (right.quantile - obs),
          obs,
          obs - (left.quantile - obs))
  names(CI) <- c("lwr", "lvl", "upr")
  print(CI)
  plot(ecdf(distro), main='Parameter bootstrap distribution')
  abline(v = CI[2], lty=2)
  abline(v = CI[c(1,3)], lty=3)
  return(CI)
}
CI <- matrix(0,3,3)
set.seed(1)
for(i in 1:3){
  newdata <-data.frame(milk_per_cow=milk_per_cow.med,
                       dairy_ration=dairy_rat[i],
                       milk_feed_price_ratio=milk_feed[i],
                       milk_cow_cost_per_animal=milk_cow[i],
                       milk_volume_to_buy_cow_in_lbs=milk_volume_to_buy_cow_in_lbs.med)
  B = 200
  fitted.obs <- predict(model_gam)
  res.obs <- data_infl$avg_price_milk - fitted.obs
  pred.obs = predict(model_gam, newdata = newdata)
  T.boot <- numeric(B)
  library(progress)
  pb <- progress_bar$new(
    format = "  processing [:bar] :percent eta: :eta",
    total = B, clear = FALSE)
  for (b in 1:B) {
    
    perm <- sample(1:nrow(data_infl), replace = T)
    dataset.boot = data_infl[perm,]
    
    model_gam_reduced.boot = mgcv::gam(avg_price_milk ~ s(dairy_ration, bs = 'cr')
                                       + milk_cow_cost_per_animal + milk_volume_to_buy_cow_in_lbs
                                       + milk_feed_price_ratio + s(milk_per_cow, bs = 'cr'), data = dataset.boot)
    
    T.boot[b] <- predict(model_gam_reduced.boot, newdata = newdata)
    pb$tick()
  }
  inter <- diagnostic_bootstrap(distro = T.boot, obs = pred.obs)
  CI[i,] <- inter
} 
jf=0.8
set.seed(1)
hist(data_infl$avg_price_milk,breaks=10,xlab='Milk Price',main = 'Prediction of milk prices')#,border=NA)
abline(v=jitter(CI[1,],jf),col=c('darkgray','black','darkgray'),lwd=c(2,3,2))
abline(v=CI[2,],col=c(2,'red',2),lwd=c(2,3,2))
abline(v=CI[3,],col=c('lightblue','blue','lightblue'),lwd=c(2,3,2))


