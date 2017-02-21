install.packages("raster")
install.packages("maptools")
install.packages("OpenStreetMap")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("lpSolve")
install.packages("plotly")
install.packages("ggthemes")
install.packages("grid")

library(raster)
library(maptools)
library(OpenStreetMap)
library(ggplot2)
library(reshape2)
library(lpSolve)
library(plotly)

#set working environment
workspace = "D:/Master Studium/polybox/Shared/MCDA project/MCDA_Hong_Biyu/data"
setwd(workspace)


##Parameters
min_bus =8 # minor transport hub should cover at least 8 bus stations or 1 subway station
min_sub = 1 # minor transport hub should cover at least 8 bus stations or 1 subway station

comm_buf = 5 # buffer of community is 5 cells
buss_buf = 5 # buffer of workplace area is 5 cells
recr_buf = 5 # buffer of recreational area is 5 cells

comm_para = 0.6 # weight of community
buss_para = 0.6 # weight of workplace area
recr_para = 0.6 # weight of recreational area


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


#--------------------------Buffer Constrain--------------------------

# Community
comm = readShapePoints("P19Community_point_Project.shp", proj4string = CRS("+init=epsg:3857"))
comm_rast = raster(ext = extent(peking_boundary),resolution = 500)

comm_tab <- table(cellFromXY(comm_rast, comm))
comm_rast[as.numeric(names(comm_tab))] <- comm_tab
comm_rast_df <- as.data.frame(rasterToPoints(comm_rast))

comm_focal_rast = focal(comm_rast, w = matrix(1, nrow = comm_buf, ncol = comm_buf), fun = sum,na.rm =TRUE)
comm_focal_rast@data@values[is.na(comm_focal_rast@data@values)]=-1

comm_focal_rast_df = as.data.frame(rasterToPoints(comm_focal_rast))
comm_map = ggplot() + theme_Publication() + ggtitle("The number of communities covered by each position") +
  geom_raster(data = comm_focal_rast_df, aes(x = x, y = y,fill = layer))+
  scale_fill_gradient(low = "white",high = "#353535", name = "Number of\ncommunities around")+
  geom_path(data = peking_boundary_db, aes(x = long, y = lat, group =
                                             group), colour = "black", size = .1)+
  coord_fixed()

ggsave("comm_map.png",path = file.path("../graphs"))



# Workplace

buss = readShapePoints("P16_17_11Bussiness_Project.shp", proj4string = CRS("+init=epsg:3857"))
buss_rast = raster(ext = extent(peking_boundary),resolution = 500)

buss_tab <- table(cellFromXY(buss_rast, buss))
buss_rast[as.numeric(names(buss_tab))] <- buss_tab
buss_rast_df <- as.data.frame(rasterToPoints(buss_rast))

buss_focal_rast = focal(buss_rast, w = matrix(1, nrow = buss_buf, ncol = buss_buf), fun = sum, na.rm = TRUE)
buss_focal_rast@data@values[is.na(buss_focal_rast@data@values)]=-1

buss_focal_rast_df = as.data.frame(rasterToPoints(buss_focal_rast))
buss_map = ggplot() + theme_Publication() + ggtitle("The number of workplaces covered by each position") +
  geom_raster(data = buss_focal_rast_df, aes(x = x, y = y,fill = layer))+
  scale_fill_gradient(low = "white",high = "#353535", name = "Number of\nworkplaces around")+
  geom_path(data = peking_boundary_db, aes(x = long, y = lat, group =
                                             group), colour = "black", size = .1)+
  coord_fixed()
ggsave("buss_map.png",path = file.path("../graphs"))


# Recreational Area

recr = readShapePoints("P12_14Recreation_Project.shp", proj4string = CRS("+init=epsg:3857"))
recr_rast = raster(ext = extent(peking_boundary),resolution = 500)

recr_tab <- table(cellFromXY(recr_rast, recr))
recr_rast[as.numeric(names(recr_tab))] <- recr_tab
recr_rast_df <- as.data.frame(rasterToPoints(recr_rast))

recr_focal_rast = focal(recr_rast, w = matrix(1, nrow = recr_buf, ncol = recr_buf), fun = sum, na.rm = TRUE)
recr_focal_rast@data@values[is.na(recr_focal_rast@data@values)]=-1

recr_focal_rast_df = as.data.frame(rasterToPoints(recr_focal_rast))
recr_map = ggplot() + theme_Publication() + ggtitle("The number of recreational areas covered by each position") +
  geom_raster(data = recr_focal_rast_df, aes(x = x, y = y,fill = layer))+
  scale_fill_gradient(low = "white",high = "#353535", name = "Number of\nrecreational sites\naround")+
  geom_path(data = peking_boundary_db, aes(x = long, y = lat, group =
                                             group), colour = "black", size = .1)+
  coord_fixed()
ggsave("recr_map.png",path = file.path("../graphs"))


#---------------- Bilinar Constarin Part I: Landuse -------------------------------
new_rast = raster(ext = extent(peking_boundary),resolution = 500)
# Historical area
his_shape = readShapePoly("Historica area.shp",proj4string = CRS("+init=epsg:3857"))

his_rast = rasterize(his_shape, new_rast, getCover = TRUE, field = "Id", background = 1,proj4string = CRS("+init=epsg:3857"))

his_df = as.data.frame(rasterToPoints(his_rast))

his_df$layer = as.logical(his_df$layer)
his_df$layer = -his_df$layer + 1
his_df$layer = as.logical(his_df$layer)

names(his_df) = c("x", "y", "historical")

his_map = ggplot() + theme_Publication() + ggtitle("Historical conservation area") +
  geom_raster(data = his_df, aes(x = x, y = y, fill = historical)) + coord_equal() + 
  geom_path(data = peking_boundary, aes(x = long, y = lat, group = group), colour = "black", size = .1)
ggsave("his_map.png",path = file.path("../graphs"))


# Green Area

gre_shape = readShapePoly("green.shp",proj4string = CRS("+init=epsg:3857"))

gre_rast = rasterize(gre_shape, new_rast, getCover = TRUE, field = "Id", background = 0,proj4string = CRS("+init=epsg:3857"))

gre_df = as.data.frame(rasterToPoints(gre_rast))

gre_df$layer = as.logical(gre_df$layer)
gre_df$layer = 1 - gre_df$layer
gre_df$layer = as.logical(gre_df$layer)

names(gre_df) = c("x", "y", "green")

gre_map = ggplot() + theme_Publication() + ggtitle("Green area") +
  geom_raster(data = gre_df, aes(x = x, y = y, fill = green)) + coord_equal() + 
  geom_path(data = peking_boundary, aes(x = long, y = lat, group = group), colour = "black", size = .1)
ggsave("gre_map.png",path = file.path("../graphs"))


# Water

wat_shape = readShapePoly("water.shp",proj4string = CRS("+init=epsg:3857"))

wat_rast = rasterize(wat_shape, new_rast, getCover = TRUE, field = "Id", background = 0, proj4string = CRS("+init=epsg:3857"))

wat_df = as.data.frame(rasterToPoints(wat_rast))

wat_df$layer = as.logical(wat_df$layer)
wat_df$layer = 1- wat_df$layer
wat_df$layer = as.logical(wat_df$layer)

names(wat_df) = c("x", "y", "water")

wat_map = ggplot() + theme_Publication() + ggtitle("Water protection area") +
  geom_raster(data = wat_df, aes(x = x, y = y, fill = water)) + coord_equal() + 
  geom_path(data = peking_boundary, aes(x = long, y = lat, group = group), colour = "black", size = .1)
ggsave("wat_map.png",path = file.path("../graphs"))

# Buildup area

bui_shape = readShapePoly("built_51_pro.shp",proj4string = CRS("+init=epsg:3857"))

bui_rast = rasterize(bui_shape, new_rast,getCover = TRUE, field = "GRIDCODE", background = 0, proj4string = CRS("+init=epsg:3857"))

bui_df = as.data.frame(bui_rast,xy = TRUE)

bui_df$layer = as.logical(bui_df$layer)

names(bui_df) = c("x", "y", "builtup")

bui_map = ggplot() + theme_Publication() + ggtitle("Built-up area") +
  geom_raster(data = bui_df, aes(x = x, y = y, fill = builtup)) + coord_equal() + 
  geom_path(data = peking_boundary, aes(x = long, y = lat, group = group), colour = "black", size = .1)
ggsave("bui_map.png",path = file.path("../graphs"))

# Landuse

land_df = wat_df
land_df$green = gre_df$green
land_df$builtup = bui_df$builtup
land_df$historical = his_df$historical
land_df$landuse = land_df$water & land_df$green & land_df$historical &land_df$builtup

land_map = ggplot() + theme_Publication() + ggtitle("Suitable land use") +
  geom_raster(data = land_df, aes(x = x, y = y, fill = landuse)) + coord_equal() + 
  geom_path(data = peking_boundary, aes(x = long, y = lat, group = group), colour = "black", size = .1)
ggsave("land_map.png",path = file.path("../graphs"))


#---------------- Bilinar Constarin Part II: Traffic ------------------------------

# Bus
bus = readShapePoints("P05Bus_point_Project.shp", proj4string = CRS("+init=epsg:3857"))

bus_rast = rasterize(bus,new_rast,field = "NAME",fun = "count",proj4string = CRS("+init=epsg:3857"),mask = FALSE,background = 0)
bus_rast_df <- as.data.frame(rasterToPoints(bus_rast))

bus_rast_df$value = 0
bus_rast_df$value[bus_rast_df$layer >= min_bus] = 1
bus_map = ggplot() + theme_Publication() + ggtitle("The number of bus stations covered by each position") +
  geom_raster(data = bus_rast_df, aes(x = x, y = y,fill = layer))+
  coord_equal()+
  scale_fill_gradient(low = "white",high = "#353535", name = "Bus stations around") +
  geom_path(data = peking_boundary_db, aes(x = long, y = lat, group =
                                             group), colour = "black", size = .1)+
  coord_fixed()
ggsave("bus_map.png",path = file.path("../graphs"))


#Subway

subw = readShapePoints("P03subway_point_Project.shp", proj4string = CRS("+init=epsg:3857"))
new_rast = raster(ext = extent(peking_boundary),resolution = 500)
subw_rast = rasterize(subw,new_rast,field = "NAME",fun = "count",proj4string = CRS("+init=epsg:3857"),mask = FALSE,background = 0)
subw_rast_df <- as.data.frame(rasterToPoints(subw_rast))
subw_rast_df$value = 0
subw_rast_df$value[subw_rast_df$layer >= min_sub] = 1

subw_map = ggplot() + theme_Publication() + ggtitle("The number of subway stations covered by each position") +
  geom_raster(data = subw_rast_df, aes(x = x, y = y,fill = layer))+
  coord_equal()+
  scale_fill_gradient(low = "white",high = "#353535", name = "Subway stations around") +
  geom_path(data = peking_boundary_db, aes(x = long, y = lat, group =
                                             group), colour = "black", size = .1)+
  coord_fixed()
subw_map
ggsave("subw_map.png",path = file.path("../graphs"))




#-------------------- Integrated Bilinar Constrain------------------

bilinar = land_df[,c("x","y")]
bilinar$value = land_df$landuse * (bus_rast_df$value + subw_rast_df$value)
bilinar_rast = rasterFromXYZ(bilinar,crs = CRS("+init=epsg:3857"))
bilinar_rast@extent = subw_rast@extent
bilinar_rast_s = bilinar_rast@data@values[bilinar_rast@data@values>0]
bilinar_rast_df = as.data.frame(rasterToPoints(bilinar_rast))
bilinar_s = bilinar[bilinar$value>0,]

ggplot() + theme_Publication() + ggtitle("Combination of all boolean variables") +
  geom_raster(data = bilinar_s, aes(x = x, y = y))+
  coord_equal() +
  geom_path(data = peking_boundary_db, aes(x = long, y = lat, group =
                                             group), colour = "black", size = .1) +
  coord_fixed()
ggsave("bilinar.png",path = file.path("../graphs"))


#-------------------------------Solve Linear Program-------------------------
di = c(1:2)
coef_matrix = matrix(nrow = 3, ncol = nrow(bilinar_s))
coef_index = matrix(0,nrow = nrow(bilinar_s),ncol = nrow(bilinar_s))
bilinar_s$x = as.integer(bilinar_s$x)
bilinar_s$y = as.integer(bilinar_s$y)

for(n in 1:nrow(coef_index)){
    index = which(bilinar_s$x %in% c(bilinar_s$x[n] +500*di,bilinar_s$x[n] -500*di,bilinar_s$x[n]) )
    index_s = which(bilinar_s$y[index] %in% c(bilinar_s$y[n],bilinar_s$y[n] + 500*di, bilinar_s$y[n] - 500*di))
    coef_index[n,index[index_s]] = 1
}




objv = matrix(nrow = 1, ncol = nrow(bilinar_s))


j = 1
coln = vector()

for(i in 1:nrow(bilinar)){
  if(bilinar$value[i]){
    coef_matrix[1,j] = comm_focal_rast_df[i,"layer"]
    coef_matrix[2,j] = buss_focal_rast_df[i,"layer"]
    coef_matrix[3,j] = recr_focal_rast_df[i,"layer"]
    coln[j] = i
    objv[j] = 1
    j = j + 1
  }
}
coef = rbind(coef_matrix,coef_index)
colnames(coef_matrix)= coln

const.dir = rbind(matrix(">=",nrow = nrow(coef_matrix),ncol = 1),matrix("<=",nrow = nrow(coef_index),ncol = 1))
const.rhs = rbind(sum(comm_rast_df$layer)*comm_para,sum(buss_rast_df$layer)*buss_para,sum(recr_rast_df$layer)*recr_para,matrix(1,nrow = nrow(coef_index),ncol = 1))

r = lp(direction = "min", objective.in = objv, const.mat = coef,const.dir = const.dir,const.rhs = const.rhs,all.bin = TRUE)


small_result_index = matrix(nrow = nrow(bilinar_s), ncol = 2)
small_result_index[,1]= r$solution
small_result_index[,2] = coln
colnames(small_result_index) = c("res", "id")

small_result = bilinar[,c("x","y")]
small_result$res = 0

j = 1
for(i in 1:nrow(small_result)){
  if(i == small_result_index[j,"id"]){
    small_result[i,"res"] = small_result_index[j, "res"]
    j = j + 1
  }
  if(j == (nrow(bilinar_s) + 1))
    break
}

small_result$res = as.logical(small_result$res)

small_result_map = ggplot() + 
  geom_raster(data = small_result, aes(x = x, y = y, fill = res)) + coord_equal() + 
  geom_path(data = peking_boundary, aes(x = long, y = lat, group = group), colour = "black", size = .1)

small_result_s = small_result[which(small_result$res==TRUE),]

ggplot() + theme_Publication() + ggtitle("Optimized locations for low-level transport hub") +
  geom_point(data = small_result_s, aes(x = x, y = y, colour = small_result_s$res), size = 1 ) +
  coord_equal() + 
  labs(color = "Low-level Transport Hub") +
  geom_path(data = peking_boundary, aes(x = long, y = lat, group = group), colour = "black", size = .1) 

ggsave("minor_result.png",path = file.path("../graphs"))

