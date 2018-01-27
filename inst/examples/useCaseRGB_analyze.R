require(rgeos)
# training and classifying for green leav classification from UAV ortho imagery
# ---- define global parameters -----------------------------------------------
#### packages
require(link2GI)
require(raster)
require(foreach)
require(doParallel)
require(pastecs)

# define project folder
projRootDir <- "~/temp7/GRASS7"
# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","output/","run/","fun","idx") )



resultFN<- "traddel_1_5m.RData"
# set working directory
setwd(path_run)

#sourceFiles <- list.files(pattern="[.]R$", path=path_fu, full.names=TRUE)
source(paste(path_fu,"countPixel.R",sep=.Platform$file.sep))

# set vars
imageFiles <- list.files(pattern="[.]tif$", path=paste0(path_output,"/"), full.names=TRUE)
position <- shapefile(path.expand("~/temp7/GRASS7/data/cut_positions_classification_corrected/cut_positions_classification_corrected.shp"))
# ----- start preprocessing ---------------------------------------------------

pre="classified_index_"
#tapply(area(r), r[], sum)
df1<-getCounts(position = position,
               imageFiles = imageFiles,
               dropChars = 8,
               pre=pre,
               ext=".tif")

### basic cleanup and  postprocessing
df2<-as.data.frame(df1,na.rm=TRUE)
names(df2)<-c("green","nogreen","plot")
df2$sum<-as.numeric(as.numeric(as.character(df2$green))+as.numeric(as.character(df2$nogreen)))
df2$percentgreen <- as.numeric(as.character(df2$green))/df2$sum
df2$date<-gsub(pattern = "_",replacement = "",x = substr(df2$plot,nchar(pre)+1,nchar(pre)+10))
df2$date<-as.POSIXlt(x = as.character(df2$date),format = "%Y%m%d")
df2$treeplot<-substr(df2$plot,nchar(pre)+16,nchar(pre)+24)
df<-df2[-3]
df$green<-as.numeric(as.character(df$green))
df$nogreen<-as.numeric(as.character(df$nogreen))
df$treeplot<-as.factor(as.character(df$treeplot))
#df$percentgreen <- dfTraddel$green/dfTraddel$sum
#str(df)

cat("::: save tree green leaves abundance results to:",resultFN,"\n")
save(df,file = resultFN)
load(file = resultFN)


options(scipen=100)
options(digits=2)
attach(df)
scores<-cbind(green, nogreen, sum, percentgreen)
stat.desc(scores)
stat.desc(scores,basic = F)

means <- c()
sds <- c()
cNg<-c()
cNng<-c()
var<-c()
daylist<- unique(df$date$yday)
for (i in 1:length(daylist)){
  means[i]<- mean(df$perc[df$date$yday==daylist[i]],na.rm=T)
  sds[i]<- sd(df$perc[df$date$yday==daylist[i]],na.rm=T)
  cNg[i]<- sum(df$green[df$date$yday==daylist[i]],na.rm=T)
  cNng[i]<- sum(df$nogreen[df$date$yday==daylist[i]],na.rm=T)
} 
dfnames<- c("Date","Julian Day" ,"mean" ,"SD" ,"countItem" ,"countnotItem")
dfstat <- data.frame(unique(df$date),julday,means,sds,cNg,cNng)
names(dfstat) <- c(dfnames)
save(dfstat,file = "resultStatTraddel.RData")


### for one day
# df[df["date"] == "2017-05-15 CEST",]


### make some charts
# calculate percantage of green pixel

# define axis format
axisformat<- theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
          axis.text.y = element_text( hjust = 1,size = 10))
# define scaling for date axis
br <- unique(df$date) 

# simple green one plot
p1 <- ggplot(data = df, aes(y = df$perc, x = df$date, group = df$treeplot)) + geom_line(colour="green") 

# colored with legend in one plot
p2 <- ggplot(data = df, aes(y = df$perc, x = df$date, group = df$treeplot, colour = factor(df$treeplot))) +
  geom_line(show.legend = TRUE) +
  xlab("Date") +
  ylab("Green Coverage %") +
  guides(colour=guide_legend(title="Sample Trees")) +
  scale_y_continuous(breaks=c(0.2,0.5,0.8,1.0))  + 
  scale_x_date(breaks=as.Date(br)) + 
  axisformat

#  green with legend in one plot
p3 <- ggplot(data = df, aes(y = df$perc, x = df$date, group = df$treeplot)) + 
            geom_line(colour="green")  + 
            xlab("Date") + 
            ylab("Green Coverage %") +
            guides(colour=guide_legend(title="Sample Trees")) +
            scale_y_continuous(breaks=c(0.2,0.5,0.8,1.0))  + 
            scale_x_date(breaks=as.Date(br)) + 
            axisformat

# panel nach treeplots
p4 <- p3 + facet_wrap(~df$treeplot) 

p4

