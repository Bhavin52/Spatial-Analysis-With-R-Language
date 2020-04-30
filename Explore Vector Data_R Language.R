
setwd("G:\\Continets\\Vietnam\\LandSet Data\\band")


library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(shapefiles)
###Read Shapepoly of the package maptools

x<-CRS("+proj=longlat +ellps=WGS84")
pa1=readShapePoly("vietnamPA",verbose = TRUE,proj4string = x)
plot(pa1)

###ReadOGR

pa2=readOGR(".","vietnamPA")

###PBSmapping

library(PBSmapping)

pa3=importShapefile("vietnamPA.shp")
plot(pa3)
pa3
pa1utm<-spTransform(pa1,crs("+proj=utm +zone=48 ellps=WGS84"))

##Explore the shape file

pa1
extent(pa1)
names(pa1)
head(pa1@data)
str(pa1@data)
print(pa1$name)
"tam dao" %in% pa1$name


Summary(pa1@data)

summary(pa1@data$iucn_cat)
iucn=table(pa1@data$iucn_cat)
barplot(iucn)

###simple visualizations

library(rworldmap)
wn=getMap(resolution ="coarse")
  plot(wn)
data(wrld_simpl,package = "maptools")
plot(wrld_simpl,add=T)
viet=readShapePoly("VNM_adm0",verbose = TRUE,proj4string = x)
plot(viet,add=T,axes=TRUE,border="blue")

### Visulization vietnam PA System

viet=readShapePoly("VNM_adm0",verbose = TRUE,proj4string = x)
plot(viet,axes=TRUE,border="grey")
plot(pa1,add=T)

#### where tam dao in Vietnam

plot(viet,axe=TRUE,border="black")
plot(tdao,add=T,col="blue")
iucnm=subset(pa1,pa1$iucn_cat=="II")
plot(iucnm)
iucnm
names(iucnm)

(y<-unique(pa1@data$name))
for (i in y[1:168]) 
  {temp = pa1[pa1$name==i,]
  writeOGR(temp,dsn = "G:\\Continets\\Vietnam\\LandSet Data\\Band\\Splitshps",i,driver="ESRI Shapefile")
}
library(rgdal)
gArea(viet)

gArea(viet)*110*110 #### 1 Degree=110km
vietPA=gIntersection(viet,pa1)
plot(vietPA)

###very useful function if we have a polygone  largen the country's surface.

tamdao=readShapePoly("VietnamPA_name__Tam Dao",verbose = TRUE,proj4string = x)
plot(tamdao)
gLength(tamdao)

gCentroid(tamdao)










