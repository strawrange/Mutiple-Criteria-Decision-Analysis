install.packages("rgeos")
install.packages("RStoolbox")
install.packages("SDMTools")
install.packages("raster")
install.packages("maptools")
install.packages("OpenStreetMap")
install.packages("RStoolbox")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("tiff")
install.packages("lpSolve")
install.packages("rgeos")
install.packages("plotKML")
install.packages("SDMTools")


library(raster)
library(maptools)
library(OpenStreetMap)
library(RStoolbox)
library(ggplot2)
library(reshape2)
library(tiff)
library(lpSolve)
library(rgeos)
library(plotKML)
library(SDMTools)



#set working environment
workspace = "D:/Master Studium/polybox/Shared/MCDA project/MCDA_Hong_Biyu/data"
setwd(workspace)

##Parameters
big_hub_buf = 5000 # service radius of major transport hub is 5000
N_big_hub = 5 # number of major transport hubs in th city
resol = 500 #resolution
slope_wei = 0.17 # weight of slopw
ponding_wei = 0.33 # weight of ponding point
cover_wei = 0.6 # weight of coverage

#Background
peking_boundary = readShapePoly("Peking_border.shp",proj4string = CRS("+init=epsg:3857"))
peking_boundary_db = fortify(peking_boundary)
map_peking = ggplot(data = peking_boundary)+
  geom_polygon(aes(x = long, y = lat, group = group, fill = hole),
               colour = "black", size = 1)+
  coord_fixed()+
  scale_fill_manual(values=c("white", "#e6e8ed"), guide="none")+theme_void()

# Plot theme
theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
  + theme(plot.title = element_text(face = "bold",
                                    size = rel(1.2), hjust = 0.5),
          text = element_text(),
          plot.background = element_blank(),
          axis.title = element_text(face = "bold",size = rel(1)),
          axis.title.y = element_text(angle=90,vjust =2),
          axis.title.x = element_text(vjust = -0.2),
          axis.text = element_text(), 
          axis.line = element_line(colour="black"),
          axis.ticks = element_line(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold")
  ))
  
}

# --------------------------Cover Problem--------------------------

pot_big_hub = small_result_s # minor transport hub script needs to be run first

pot_big_hub$N_small_hub = NA

for(i in 1:nrow(pot_big_hub)){
  pot_big_hub[i, "N_small_hub"] = sum(small_result[which((small_result$x <= pot_big_hub[i,"x"] + big_hub_buf & small_result$x >= pot_big_hub[i,"x"] - big_hub_buf) & 
                                                           small_result$y <= pot_big_hub[i,"y"] + big_hub_buf & small_result$y >= pot_big_hub[i,"y"] - big_hub_buf),"res"])
}

pot_big_hub$cover_sta = (pot_big_hub$N_small_hub - min(pot_big_hub$N_small_hub))/(max(pot_big_hub$N_small_hub)-min(pot_big_hub$N_small_hub))

ggplot() + theme_Publication() + ggtitle("The number of low-level transport hubs covered by each position") +
  geom_point(data = pot_big_hub, aes(x = x, y = y, colour = pot_big_hub$N_small_hub), size = 2 ) +
  coord_equal() + 
  scale_colour_gradient(low = "white", high = "#a18a70", name = "service level") +
  geom_path(data = peking_boundary, aes(x = long, y = lat, group = group), colour = "black", size = .1)
ggsave("cover.png",path = file.path("../graphs"))

ggplot() + theme_Publication() + ggtitle("The number of low-level hubs covered by each position\n(standardized)") +
  geom_point(data = pot_big_hub, aes(x = x, y = y, colour = pot_big_hub$cover_sta), size = 2 ) +
  coord_equal() + 
  scale_colour_gradient(low = "white", high = "#a18a70", name = "service level") +
  geom_path(data = peking_boundary, aes(x = long, y = lat, group = group), colour = "black", size = .1)
ggsave("cover_sta.png",path = file.path("../graphs"))



# --------------------------Slope--------------------------

pk_dem = raster("dem_pk_p",crs = CRS("+init=epsg:3857"))
pk_slope = terrain(pk_dem,opt = 'slope',unit = 'tangent',neighbors = 4)
pk_slope_df = as.data.frame(pk_slope, xy = TRUE)

xy = pot_big_hub[,c("x", "y")]
pot_big_hub$slope = extract(pk_slope, xy)
pot_big_hub$id = rownames(pot_big_hub)

slope_point_df = pot_big_hub[, c ("x", "y", "slope")]

ggplot() + theme_Publication() + ggtitle("Slope") +
  geom_point(data = pot_big_hub, aes(x = x, y = y, colour = pot_big_hub$slope), size = 2 ) +
  coord_equal() + 
  scale_colour_gradient(low = "#8d9cac", high = "#233d56", name = "slope") +
  geom_path(data = peking_boundary, aes(x = long, y = lat, group = group), colour = "black", size = .1) 
ggsave("slope.png",path = file.path("../graphs"))

pot_big_hub$slope_sta = 1/(1+exp(12*pot_big_hub$slope - 6))

ggplot() + theme_Publication() + ggtitle("Slope \n(standardized)") +
  geom_point(data = pot_big_hub, aes(x = x, y = y, colour = pot_big_hub$slope_sta), size = 2 ) +
  coord_equal() + 
  scale_colour_gradient(low = "#8d9cac", high = "#233d56", name = "slope") +
  geom_path(data = peking_boundary, aes(x = long, y = lat, group = group), colour = "black", size = .1)
ggsave("slope_sta.png",path = file.path("../graphs"))

# --------------------------Ponding Point--------------------------
pond = readShapePoints("ponding_point.shp", proj4string = CRS("+init=epsg:3857"))
pond_df = as.data.frame(pond)

pond_bf = gBuffer(pond, width = 1000,byid = TRUE)

pond_bf_df = fortify(pond_bf)
pond_ints = over(pond,pond_bf)
pond_bf@data = pond_ints
new_rast = raster(ext = extent(peking_boundary),resolution = 500)
pond_rast = rasterize(pond_bf,new_rast,field = "mark",proj4string = CRS("+init=epsg:3857"),mask = FALSE,background = 0)

pond_rast_df <- as.data.frame(rasterToPoints(pond_rast))

pot_big_hub$pond = NA
for(i in 1:nrow(pot_big_hub)){
  pot_big_hub[i, "pond"] = pond_rast_df[rownames(pot_big_hub)[i], "layer"]
}

pond_map = ggplot() + theme_Publication() + ggtitle("Ponding area") +
  geom_point(data = pot_big_hub, aes(x = x, y = y, colour = pond), size = 2.5 ) + 
  coord_equal() + 
  geom_path(data = peking_boundary, aes(x = long, y = lat, group = group), colour = "black", size = .1) + 
  scale_color_gradient(low = "#fbdfe4",high = "#d22b47", name = "Ponding") +
  coord_fixed()

ggsave("ponding.png",path = file.path("../graphs"))

pot_big_hub$pond_sta = 2 ^ (-pot_big_hub$pond)
  
ggplot() + theme_Publication() + ggtitle("Ponding area \n(standardized)") +
  geom_point(data = pot_big_hub, aes(x = x, y = y, colour = pond_sta), size = 2.5 ) + 
  coord_equal() + 
  geom_path(data = peking_boundary, aes(x = long, y = lat, group = group), colour = "black", size = .1) + 
  scale_color_gradient(low = "#fbdfe4",high = "#d22b47", name = "Ponding") +
  coord_fixed()

ggsave("ponding_sta.png",path = file.path("../graphs"))

#-------------------------------Solve Linear Program-------------------------

buffer = c(1:(big_hub_buf/resol))

coef_matrix_big = matrix(0, nrow = nrow(pot_big_hub), ncol = nrow(pot_big_hub))

pot_big_hub$x = as.integer(pot_big_hub$x)
pot_big_hub$y = as.integer(pot_big_hub$y)

rownames(coef_matrix_big) = rownames(pot_big_hub)
colnames(coef_matrix_big) = rownames(pot_big_hub)

for(n in 1:nrow(coef_matrix_big)){
    index_big = which(pot_big_hub$x %in% c(pot_big_hub$x[n] +resol*buffer,pot_big_hub$x[n] - resol*buffer,pot_big_hub$x[n]) )
    index_big_s = which(pot_big_hub$y[index_big] %in% c(pot_big_hub$y[n],pot_big_hub$y[n] + resol*buffer, pot_big_hub$y[n] - resol*buffer))
    coef_matrix_big[n,index_big[index_big_s]] = 1
  
}

test= matrix(nrow = nrow(pot_big_hub), ncol = 4)
test[,1]= coef_matrix_big[,20]
test[,2] = rownames(coef_matrix_big)
test = as.data.frame(test)

test[,c(3,4)] = pot_big_hub[, c("x", "y")]
colnames(test) = c("res","id","x","y")

coef_lim_number = matrix(1, ncol = nrow(pot_big_hub), nrow = 1)
coef_big = rbind(coef_matrix_big, coef_lim_number)


pot_big_hub$coef = pot_big_hub$cover_sta * cover_wei + pot_big_hub$slope_sta * slope_wei + pot_big_hub$pond_sta * ponding_wei
objv_big = pot_big_hub$coef

const.dir = matrix("<=", nrow = nrow(coef_matrix_big) + 1, ncol = 1)
const.rhs = matrix( 1, nrow = nrow(coef_matrix_big) + 1, ncol = 1)
const.rhs[nrow(const.rhs),1] = N_big_hub

r_big = lp(direction = "max", objective.in = objv_big, const.mat = coef_big,const.dir = const.dir,const.rhs = const.rhs,all.bin = TRUE)

big_result_index = matrix(nrow = nrow(pot_big_hub), ncol = 2)
big_result_index[,1]= r_big$solution
big_result_index[,2] = rownames(pot_big_hub)
colnames(big_result_index) = c("res", "id")

big_result = pot_big_hub[,c("x","y")]
big_result$res = r_big$solution

j = 1
for(i in 1:nrow(big_result)){
  if(i == big_result_index[j,"id"]){
    big_result[i,"res"] = big_result_index[j, "res"]
    j = j + 1
  }
  if(j == (nrow(pot_big_hub) + 1))
    break
}

big_result_s = big_result[which(big_result$res == 1),]
big_result_s$res = as.logical(big_result_s$res)

ggplot() + theme_Publication() + ggtitle("Optimized locations for high-level transport hub") +
  geom_point(data = big_result_s, aes(x = x, y = y, color= res), size = 4) + coord_equal() + 
  geom_path(data = peking_boundary, aes(x = long, y = lat, group = group), colour = "black", size = .1) + 
  labs(color = "High-level Transport Hub") +
  coord_fixed()

ggsave("major_result.png",path = file.path("../graphs"))

  