#Read in rasters
setwd("G:\\continets\\vietnam\\landset data")

library(raster)

band1=raster("band1.tif.tif")
band1

plot(band1)

band2 = raster("band2.tif.tif")
plot(band2)

setwd("G:\\Continets\\Vietnam\\LandSet Data\\Band")

rlist = list.files(pattern = "TIF$",full.names = TRUE)

rasters =stack(rlist)

names(rasters)
rasters

plot(rasters)

setwd("G:\Continets\Vietnam\LandSet Data")
band1

b1LL<-projectRaster(band1,crs = '+proj=longlat')
b1LL
plot(b1LL)

#latitude Longitude to UTM

X="+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
b1u =projectraster(b1LL,CRS=X)
b1u

band1 =raster("band1.tif.tif")
band1

band3 =raster("band2.tif.tif")
band3

band4 =raster("band4.tif.tif")
band4

stackr =stack(band1,band3,band4)
plot(stackr$band1.tif)

rlist =list.files(pattern = "tif$",full.names = TRUE)

raster = stack(rlist)

rasters

rasterd1 =dropLayer(raster,5)
names(rasterd1)

rasterd2 =dropLayer(raster,c(1,6))
names(rasterd2)

rastadd = addLayer(rasterd2,band6)
names(rastadd)

# false clor composition

plotRGB(rasters,r=3,g=2,b=1,stretch="hist")

plotRGB(rasters,r=3,g=2,b=1,scale=800,stretch = "lin")

plotRGB(rasters,r=4,g=3,b=1,stretch = "hist")

rasters

s = calc(rasters,sum)
func<-function(x)
{
  x[x<300]<-NA 
  return(x)
}
rc2 <-calc(s,func)
s2 = s[s<300]<-NA

# NDVI =(NIR-RED)/(NIR+RED)

ndvi=(rasters$Band4.TIF-rasters$Band3.TIF)/(rasters$Band4.TIF+rasters$Band3.TIF)
plot(ndvi)

m<-c(-0.2,0.2,1,0.2,0.6,2,0.6,1,3)
mat<-matrix(m,ncol = 3,byrow = TRUE) 
rc<-reclassify(ndvi,mat)
plot(rc)

# calculate % landcoverof Categorical map
library(SDMtools)

c=classStat(rc,cellsize=30,latlon= false)
head(c)

#resample rasters

library(raster)

h=raster("tamd_hill.tif")
n= raster("tamd_ndvi.tif")

plot(n)

nLL <-projectRaster(n,crs = '+proj=longlat')

n2=resample(nLL,h,"ngb")
library(raster)
hill=raster("1hillshade.tif")
plot(hill)

e<-extent(105.6,105.7,20.5,21.5)

abs=crop(hill,e2)
plot(abs)
e2=drawExtent()

library(rgdal)
tdao=readOGR(".","VietnamPA_name__Tam Dao")
plot(tdao,add=T)

A=mask(hill,tdao)
plot(A)

pa1=readOGR(".","VietnamPA")
plot(pa1,add=T)
dat<-extract(hill,pa1,fun=mean,na.rm=TRUE)
  
head(dat)

dem=raster("vie_dem_SRTm.tif")
dem2=getData("alt",country="VNM")
plot(dem,zlim=c(1000,3000),main="elevation 1000m-3000m")
slop=terrain(dem,opt="Slope",unit="degrees")
aspect=terrain(dem,opt="aspect")

x<-terrain(dem,opt=c("slope","aspect"),unit="degrees")
plot(x)

hill<-hillShade(slope,aspect,40,270)
plot(hill)

e3=drawExtent()
plot(e3)

####Basic Stastics on Raster

library(raster)
h=raster("tamd_hill.tif")
n=raster("tamd_ndvi.tif")
n2=raster("ndvill.tif")###NDVI with latitude Longitude

summary(h)
summary(n2)
hist(h)

x=stack(n2,h)
head(x)

valuetable<-getValues(x)
valuetable<-na.omit(valuetable)
valuetable<-as.data.frame(valuetable)
head(valuetable,n=10)
str(valuetable)
mysample<-valuetable[sample(1:nrow(valuetable),"750",replace = FALSE),]
head(mysample)
cor.test(mysample$ndvill,mysample$tamd_hill)

fit=lm(ndvill~tamd_hill,data=mysample)
summary(fit)

##extract via cordinates

pt=read.csv("points1.csv")

head(pt)

plot(n2)
points(pt,col="red")

dat1=extract(x,pt)
head(dat1)

dat1=as.data.frame(dat1)
dat1=na.omit(dat1)
cor(dat1$ndvill,dat1$tamd_hill)





