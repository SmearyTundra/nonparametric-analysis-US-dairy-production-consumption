library(raster)
library(usmap)
library(ggplot2)
library(maps) 
library(dplyr)

fresh_cow_prices = c(1526.43,1531.21,1436.44)
bred_heifer_prices = c(1289.58,1254.70,1223.86)
large_feeder_prices = c(123.08,127.32,115.71)
milk_price = c(24.67,23.11,21.61)


## CONFORMAL
L = c(0.1636781,0.1728994,0.1821207)
U = c(0.2789444,0.2743337,0.2328379)
y = c(0.2424174,0.2425718,0.2157682)


## BOOTSTRAP
L = c(0.1790048,0.2006431,0.1819760)
U = c(0.3040295,0.2928557,0.2516137)
y = c(0.2424174,0.2425718,0.2157682)


x=c("01 - Dec","02 - Jan","03 - Feb")
df = data.frame(x=x, y =y)
#LOWER      PRED     UPPER 
#0.1636781 0.2424174 0.2789444 
#Initial training on full data set ...
#Processing prediction point 1 (of 1) ...
#LOWER      PRED     UPPER 
#0.1728994 0.2425718 0.2743337 
#Initial training on full data set ...
#Processing prediction point 1 (of 1) ...
#LOWER      PRED     UPPER 
#0.1821207 0.2157682 0.2328379


ggplot(df, aes(x = x, y = y)) +
    geom_errorbar(aes(ymax = U, ymin = L), width = 0.3) +
    geom_point(size = 4, col = "darkorange") +
    coord_flip() +
    labs(x = "Months",
         y = "Milk price [$/lbs]",
         title = "Bootstrap intervals") 

ggsave("output/CI_prices.pdf",width=10,height=2)




milk_sales_state <- read.table("data_updated_2021/milk_sales_by_state_2012_2017.csv", 
                               sep =",", header = T)
names(milk_sales_state)[6] = "region"
milk_sales_state[which(milk_sales_state$Value == " (D)"), ]$Value = 0
milk_sales_state$Value = as.numeric(gsub(",", "", milk_sales_state$Value))
milk_sales_state$region = tolower(milk_sales_state$region)
milk_sales_state <- milk_sales_state[-c(1,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,21)]

milk_sales_state_2017 <- milk_sales_state[milk_sales_state$Year==2017,]
milk_sales_state_2012 <- milk_sales_state[milk_sales_state$Year==2012,]

MainStates <- map_data("state")
ggplot() + 
    geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                  color="black", fill="blue" )

MergedStates <- inner_join(MainStates, milk_sales_state_2017, by = "region")


##### Milk sales per state
g <- ggplot()
g <- g + geom_polygon( data=MergedStates, 
                       aes(x=long, y=lat, group=group, fill = log(Value)), 
                       color="black", size = 0.2) + 
    
    scale_fill_continuous(name="logarithmic scale", low = "lightyellow", 
                          high = "darkorange",limits = c(16,24), breaks=seq(16,24,by=2), 
                          na.value = "grey50") +
    
    labs(title="Milk sales (in dollars) in the Mainland United States")
g
ggsave("output/milk_sales_states.pdf")




######################
source("10-Prepare_data_spatial_all.R")
dat <- st_as_sf(
    all_sales_county_2007,
    coords = c("y", "x"),
    crs = 4326
)


####### COUNTIES MAP
plot_usmap(regions = "counties") + 
    labs(title = "US Counties",
         subtitle = "This is a blank map of the counties of the United States.") + 
    theme(panel.background = element_rect(color = "black", fill = "lightblue"))





########## UTAH MAP
usmap::plot_usmap("counties",
                  include = c("UT"),
                  labels = TRUE, label_color = "black",
                  fill = "yellow", alpha = 0.25, color = "orange", linewidth =0.5)

eq_transformed <- usmap_transform(all_sales_county_2007, input_names = c("y","x"),
                                  output_names = c("lon","lat"))
eq_transformed <- eq_transformed[eq_transformed$State=="utah",]
point_transformed <- usmap_transform(data.frame(lon=-111.8910,lat=40.7608), input_names=c("lon","lat"))

plot_usmap("counties", include = c("UT"), labels = TRUE, fill = "orange", alpha = 0.25, color = "darkorange") +
    geom_point(data = eq_transformed, aes(x = lon, y = lat, color = log(Value)),
               alpha = 0.5, size = 4) +
    geom_point(aes(x=point_transformed$x,y=point_transformed$y), color ="red", size = 6, alpha = 0.5 ) +
    labs(title = "Milk sales per county, Utah",
         subtitle = "Source: USDA, measured in $, logarithmic scale",
         size = "Magnitude") +
    theme(legend.position = "right")
ggsave("output/milk_sales_utah.pdf")




##### UGLY MAP
ggplot() + 
    geom_polygon( data=map_data("county"), aes(x=long, y=lat, group=group),
                  color="black", fill="white" ) +
    geom_point(data = all_sales_county_2007, aes(x=y,y=x,color = log(Value)), size = 5)+
    labs(title = "Location",
         color = "Sales",
         size = "Milk Sales in Utah")+
    xlab("Longitude")+
    ylab("Latitude")+
    #ggspatial::annotation_north_arrow(location = "br")+
    ggspatial::annotation_scale(location = "bl") +
    coord_cartesian(xlim=c(-115, -105), ylim = c(38, 42))+
    geom_point(aes(x=-111.8910,y=40.7608), color ="red", size = 6 ) 

