########################################################################################   
# script name:  
# author: Mike Double  
# summary:    
#     
# setwd('C:/1. Analyses')
# library(formatR)

# tidy.source(width.cutoff = 70) #This take code from clipboard and reformats
# rm(list=ls(all=TRUE))
#.libPaths()
# sessionInfo()
# options(scipen=999) # Turn off scientific notation
# par(mar=c(5.1,4.1,4.1,2.1)) # better margins for R studio
# par(mfrow=c(2,1)) # set multiple images
# Sys.getenv('R_HOME')
########################################################################################   

########################################################################################   
# Load libraries
########################################################################################

########################################################################################   
# Load functions
########################################################################################")



##### CODE TO BE DEVELOPED FOR INTERROGATING AND PROCESSING DATA FROM ACCESS DATABASE OF LOCATION DATA








setwd("//data-process/argos/program_data/13440/2014")
dir()


# Imports all PRV and DIAG data files
# allwhaledataPRV_postdeploy<-read.csv(file="All whale PRV data post-deploy to Nov 2013.csv",as.is=TRUE)
# allwhaledataDIAG_postdeploy<-read.csv(file="All whale DIAG data post-deploy to Nov 2013.csv",as.is=TRUE)

# str(allwhaledataDIAG_postdeploy)
allwhaledataDIAG_postdeploy$gmt<-strptime(allwhaledataDIAG_postdeploy$gmt, format="%Y-%m-%d %H:%M:%S",tz="GMT")
allwhaledataDIAG_postdeploy$ptt<-as.factor(allwhaledataDIAG_postdeploy$ptt) 
allwhaledataPRV_postdeploy$gmt<-as.POSIXct(allwhaledataPRV_postdeploy$gmt, tz="GMT")



#Adding tag length data
tag_length_data<-tag_summary_unique_ptt[,c(4,5,18,20)]
#write.csv(tag_length_data,file="tag_length_data Nov 2013.csv",row.names = FALSE)
# Imports tag_length_data - these are approx lengths staggered for plotting - need to enter real data in db
tag_length_data<-read.csv(file="tag_length_data Nov 2013.csv",as.is=TRUE)
tag_summary_unique_ptt$tag_length_mm<-tag_length_data$tag_length_mm
tag_summary_unique_ptt$fact_tag_length_mm<-as.factor(tag_length_data$tag_length_mm)
head(tag_summary_unique_ptt)
ncol(tag_summary_unique_ptt)

#Tabling start and end dates for tags
names(allwhaledataPRV_postdeploy)
tag_duration_summary_PRV<-tag_duration_summary(allwhaledataPRV_postdeploy)
tag_duration_summary_DIAG<-tag_duration_summary(allwhaledataDIAG_postdeploy)


# Adding deployment date to table
names(tag_summary_unique_ptt)
tag_duration_summary_PRV$deploy_date<-tag_summary_unique_ptt[match(tag_duration_summary_PRV$ptt,tag_summary_unique_ptt$ptt),5]
tag_duration_summary_DIAG$deploy_date<-tag_summary_unique_ptt[match(tag_duration_summary_DIAG$ptt,tag_summary_unique_ptt$ptt),5]

# Adding last transmission to table
names(tag_summary_unique_ptt)
tag_duration_summary_PRV$date_las_transm<-tag_summary_unique_ptt[match(tag_duration_summary_PRV$ptt,tag_summary_unique_ptt$ptt),23]
tag_duration_summary_DIAG$date_las_transm<-tag_summary_unique_ptt[match(tag_duration_summary_DIAG$ptt,tag_summary_unique_ptt$ptt),23]

# Adding transmission duration from db not calculated to table
names(tag_summary_unique_ptt)
tag_duration_summary_PRV$transm_dur<-tag_summary_unique_ptt[match(tag_duration_summary_PRV$ptt,tag_summary_unique_ptt$ptt),24]
tag_duration_summary_DIAG$transm_dur<-tag_summary_unique_ptt[match(tag_duration_summary_DIAG$ptt,tag_summary_unique_ptt$ptt),24]

# Adding tag length
names(tag_summary_unique_ptt)
tag_duration_summary_PRV$tag_length_mm<-tag_summary_unique_ptt[match(tag_duration_summary_PRV$ptt,tag_summary_unique_ptt$ptt),20]
tag_duration_summary_DIAG$tag_length_mm<-tag_summary_unique_ptt[match(tag_duration_summary_DIAG$ptt,tag_summary_unique_ptt$ptt),20]


# Adding tag length factor
names(tag_summary_unique_ptt)
tag_duration_summary_PRV$factor_tag_length_mm<-tag_summary_unique_ptt[match(tag_duration_summary_PRV$ptt,tag_summary_unique_ptt$ptt),25]
tag_duration_summary_DIAG$tag_length_mm<-tag_summary_unique_ptt[match(tag_duration_summary_DIAG$ptt,tag_summary_unique_ptt$ptt),25]

# Adding species
names(tag_summary_unique_ptt)
tag_duration_summary_PRV$species<-tag_summary_unique_ptt[match(tag_duration_summary_PRV$ptt,tag_summary_unique_ptt$ptt),10]
tag_duration_summary_DIAG$species<-tag_summary_unique_ptt[match(tag_duration_summary_DIAG$ptt,tag_summary_unique_ptt$ptt),10]

# Adding campaigns
names(tag_summary_unique_ptt)
tag_duration_summary_PRV$campaigns<-tag_summary_unique_ptt[match(tag_duration_summary_PRV$ptt,tag_summary_unique_ptt$ptt),1]
tag_duration_summary_DIAG$campaigns<-tag_summary_unique_ptt[match(tag_duration_summary_DIAG$ptt,tag_summary_unique_ptt$ptt),1]

# Adding deploy_ID
names(tag_summary_unique_ptt)
tag_duration_summary_PRV$deploy_ID<-tag_summary_unique_ptt[match(tag_duration_summary_PRV$ptt,tag_summary_unique_ptt$ptt),4]
tag_duration_summary_DIAG$deploy_ID<-tag_summary_unique_ptt[match(tag_duration_summary_DIAG$ptt,tag_summary_unique_ptt$ptt),4]


# Adding deploy_ID
names(tag_summary_unique_ptt)
tag_duration_summary_PRV$tag_type<-tag_summary_unique_ptt[match(tag_duration_summary_PRV$ptt,tag_summary_unique_ptt$ptt),18]
tag_duration_summary_DIAG$tag_type<-tag_summary_unique_ptt[match(tag_duration_summary_DIAG$ptt,tag_summary_unique_ptt$ptt),18]

# Adding % implant
names(tag_summary_unique_ptt)
tag_duration_summary_PRV$perc_implant<-tag_summary_unique_ptt[match(tag_duration_summary_PRV$ptt,tag_summary_unique_ptt$ptt),16]
tag_duration_summary_DIAG$perc_implant<-tag_summary_unique_ptt[match(tag_duration_summary_DIAG$ptt,tag_summary_unique_ptt$ptt),16]


# Sort by deploy_date

tag_duration_summary_PRV<-tag_duration_summary_PRV[with(tag_duration_summary_PRV, order(deploy_date)), ] 
tag_duration_summary_DIAG<-tag_duration_summary_PRV[with(tag_duration_summary_DIAG, order(deploy_date)), ] 


head(tag_duration_summary_PRV)

write.csv(tag_duration_summary_PRV, file = "Summary of tag durations PRV.csv",row.names=FALSE)
write.csv(tag_duration_summary_DIAG, file = "Summary of tag durations DIAG.csv",row.names=FALSE)

#Remove first record cos suction cup
tag_duration_summary_PRV<-tag_duration_summary_PRV[-1,]

library(lubridate)
tag_duration_summary_PRV$year<-year(tag_duration_summary_PRV$gmt)

###############################################################################
# for Alex
###############################################################################
Alex_data<-allwhaledataPRV_postdeploy[allwhaledataPRV_postdeploy$ptt==123232,]

write.csv(Alex_data, file="ptt 12332 data for Alex.csv")
###############################################################################
# mapping
###############################################################################
library(maps)
data<-allwhaledataPRV_postdeploy[allwhaledataPRV_postdeploy$ptt==123232,]
map("world", boundary = TRUE, wrap=TRUE, xlim=c(-200,0), ylim=c(-70,0))

points(298,-64, cex=50, pch = 19)
points(data$longitude, data$latitude, cex=1, pch = 19)

tag_duration_summary_PRV[,1:6]
tag_summary_unique_ptt[tag_summary_unique_ptt$ptt==40617,]

#to_turn_to_zero<-c(40617,40619, 40620, 40621)
library(ggmap)
al1 <- get_map(location = c(-70.304474, lat = -30.362563), zoom = 3, maptype = 'terrain')
ggmap(al1)
al1MAP <- ggmap(al1)+ geom_point(data=data, aes(x=(data$longitude-360), y=latitude))
al1MAP
###############################################################################
# plotting
###############################################################################

# Plot by year

library(ggplot2)
library(scales)
library(grid)

names(tag_duration_summary_PRV)
str(tag_duration_summary_PRV)

ggplot(data=tag_duration_summary_PRV, aes(x=as.factor(year), y=as.numeric(duration)))+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()+ # shorthand for  stat_boxplot(geom='boxplot')
  xlab("Year")+
  ylab("Number of days")+
  theme(axis.title.x = element_text(size = 14, face = 'bold', vjust=-0.1))+
  theme(axis.title.y = element_text(size = 14, face = 'bold', vjust=-0.1))+
  theme(axis.text.y=element_text(size=9, colour="black"))+
  theme(axis.text.x=element_text(size=8.5, colour="black", vjust=-0.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  theme(panel.margin = unit(c(1,1,1,1), "cm"))

# Plot by length

names(tag_duration_summary_PRV)
str(tag_duration_summary_PRV)

ggplot(data=tag_duration_summary_PRV, aes(x=tag_length_mm, y=as.numeric(duration)))+
  geom_point()+
  xlab("Tag length mm")+
  ylab("Number of days")+
  theme(axis.title.x = element_text(size = 14, face = 'bold', vjust=-0.1))+
  theme(axis.title.y = element_text(size = 14, face = 'bold', vjust=-0.1))+
  theme(axis.text.y=element_text(size=9, colour="black"))+
  theme(axis.text.x=element_text(size=8.5, colour="black", vjust=-0.5))+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  theme(panel.margin = unit(c(1,1,1,1), "cm"))

# Plot by length(factor)
names(tag_duration_summary_PRV)
str(tag_duration_summary_PRV)

library(plyr)
round_any(132.1, 10)               # returns 130
round_any(132.1, 10, f = ceiling)  # returns 140
round_any(132.1, 5, f = ceiling)   # returns 135

roundUp(tag_duration_summary_PRV$tag_length_mm)
x<-tag_duration_summary_PRV$tag_length_mm
round_any(x,10)

ggplot(data=tag_duration_summary_PRV, aes(x=factor_tag_length_mm, y=as.numeric(duration), fill=as.factor(species)))+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()+
  #scale_x_discrete(limits=seq(130, 350, 10))+
  xlab("Tag length mm")+
  ylab("Number of days")+
  theme(axis.title.x = element_text(size = 14, face = 'bold', vjust=-0.1))+
  theme(axis.title.y = element_text(size = 14, face = 'bold', vjust=-0.1))+
  theme(axis.text.y=element_text(size=9, colour="black"))+
  theme(axis.text.x=element_text(size=8.5, colour="black"))+
  scale_fill_brewer(name = "Species", type="div")+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  theme(panel.margin = unit(c(1,1,1,1), "cm"))

# WAP only data

names(tag_summary_unique_ptt)
tag_summary_unique_ptt$deploy_ID

nrow(tag_duration_summary_PRV)
tag_duration_summary_PRV_WAP<-tag_duration_summary_PRV[154:187, ]


ggplot(data=tag_duration_summary_PRV_WAP, aes(x=factor_tag_length_mm, y=as.numeric(duration), fill=as.factor(species)))+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()+
  #scale_x_discrete(limits=seq(130, 350, 10))+
  xlab("Tag length mm")+
  ylab("Number of days")+
  theme(axis.title.x = element_text(size = 14, face = 'bold', vjust=-0.1))+
  theme(axis.title.y = element_text(size = 14, face = 'bold', vjust=-0.1))+
  theme(axis.text.y=element_text(size=9, colour="black"))+
  theme(axis.text.x=element_text(size=8.5, colour="black"))+
  scale_fill_brewer(name = "Species", type="div")+
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))+
  theme(panel.margin = unit(c(1,1,1,1), "cm"))
table(tag_duration_summary_PRV_WAP$tag_type)
tag_duration_summary_PRV_WAP[tag_duration_summary_PRV_WAP$tag_length_mm==225,]
str(tag_duration_summary_PRV_WAP)

tag_duration_summary_PRV_WAP[grep("small", tag_duration_summary_PRV_WAP$tag_type),]

tag_duration_summary_PRV_WAP[,13:14]