##Jacob McCrone 
  
##Set Working Directory 
setwd("~/Desktop/R")

install.packages("maps")
install.packages("ggplot2")
library(ggplot2)

##Read Data
shoot <- read.csv(file = 'shootingsmj.csv')
gfi <- read.csv("GFI.csv")
goi <- read.csv("GOI.csv")
avgrank <- read.csv("StrictRank.csv")
gunviolence <- read.csv("gunviolence.csv")
gundeaths <- read.csv("gundeaths_pc.csv")

##Standardize GFI to 0 to 1. 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) ##Creating a function to normalize
} 

##Calling function: 
gfi$GFI_norm <- normalize(gfi$GFI)
goi$GFI_norm <- normalize(goi$GFI)



##USA Map
library(maps)
map(database = "usa")
statesus <- map(database = "state")

lon <- c(shoot$longitude)
lat <- c(shoot$latitude)
state <- c(shoot$state)
nfat <- c(shoot$fatalities)
  df <- as.data.frame(cbind(lon,lat,nfat))
  statesus <- map_data("state")
  
  ## Plot USA
  ggplot()+
    geom_polygon(data=statesus, aes(x=long, y=lat, group=group), fill="white", colour="black")+
    
    ggtitle("Mass Shootings in USA")+
    coord_quickmap()+
    theme_void()
  
  geom_polygon(data=statesus, aes(x=lon, y=lat), fill="gray", colour="black")+
  ## Add points to plot 
  ggplot()+
    geom_polygon(data=statesus, aes(x=long, y=lat, group=group), fill="white", colour="black")+
    ## add points with size in proportion to fatalities
    geom_point(data=df, aes(x=lon, y=lat, size = nfat),
               alpha=.4, colour="red")+ # alpha makes points transparent
    
    scale_size(name="Number of Fatalities", range=c(.5, 3))+
    
    ggtitle("Mass Shootings in the US 1982 - 2021")+
    coord_quickmap()+
    theme_void()
  ##

##Average GFI for both data sets: 
mg1 <- merge(x=gfi,y=goi,by="State")
  mg1$avg_gfi <- ((mg1$GFI_norm.x + mg1$GFI_norm.y)/2)
  
  ##Tallying Mass Shootings by State: 
mg2$numshoot <- table(shoot2$state)  
  mg2 <- subset(mg1, State != "AL"& State != "AK" & State != "D.C" & State 
                    != "DE"& State != "ID"& State != "ME"& State != "MT"& State 
                    != "NH"& State != "NM"& State != "ND" & State != "RH" & State 
                    != "SD"& State != "VT"& State != "WV"& State != "WY")
  
  shoot2 <- subset(shoot, state !="D.C.")
  
  
##Plot gfi with Mass Shooting Data
  plot(mg2$avg_gfi,mg2$numshoot, main="Mass Shootings vs. Gun Friendliness",
       xlab="Gun Friendly Index", ylab="# Of Mass Shootings by State", pch=20)
  
  abline(lm(mg2$numshoot ~ mg2$avg_gfi), col = "red") ##Not the Expected Result... 
  
mg_new <- merge(x=gundeaths,y=mg1,by="State")
  
plot(mg_new$avg_gfi,mg_new$deaths, main="Gun Laws vs. Gun Deaths by State",
     xlab="Gun Friendly Index", ylab="Gun Deaths per 100k", pch=20)
abline(lm(mg_new$deaths ~ mg_new$avg_gfi), col = "red")
  
##
states <- map_data('state')
  
 