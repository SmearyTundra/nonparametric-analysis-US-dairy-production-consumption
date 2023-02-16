milk_per_cow.grid=seq(range(data_infl$milk_per_cow)[1],range(data_infl$milk_per_cow)[2],length.out = 50)
dairy_ration.grid=seq(range(data_infl$dairy_ration)[1],range(data_infl$dairy_ration)[2],length.out = 50)
grid=data.matrix( expand.grid(milk_per_cow=milk_per_cow.grid, 
                              dairy_ration=dairy_ration.grid))

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

wrapper_milkcow=function(grid_point){
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

