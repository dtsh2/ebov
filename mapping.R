rm(list=ls())

require(doBy)
require(plyr)
require(rworldmap)
require(TeachingDemos)

dF <- getMap()@data  

f_data<-read.csv("filo_data_wa.csv",header=T,
                 stringsAsFactors=FALSE, row.names = NULL)
ff_data<-subset(f_data,select=c("Country","Positive",
                                "Number","Genus"))

RES<-aggregate(. ~ Country+Genus, data=ff_data,FUN=sum)
RES$Negative <-RES$Number-RES$Positive
# 
ebov<- RES[RES$Genus == "EBOV", ]
marv<- RES[RES$Genus == "MARV", ]

ebov$Country[2]<-"Burkina Faso"
ebov$Country[4]<-"Central African Republic"
ebov$Country[10]<-"Sierra Leone"

marv$Country[2]<-"Central African Republic"
marv$Country[6]<-"Sierra Leone"

### plot for real
#############

theCountries <- c("BEN","BFA","CAF","CIV",
                  "CMR","GHA","GIN","GMB",
                  "GNB","LBR","NER","NGA",
                  "SEN","SLE","TCD","TGO","MLI")

# These are the ISO3 names of the countries you'd like to plot in red

malDF <- data.frame(country = c("BEN","BFA","CAF","CIV",
                                "CMR","GHA","GIN","GMB",
                                "GNB","LBR","NER","NGA",
                                "SEN","SLE","TCD","TGO","MLI"),
                    West_Africa = rep(1,17))
# malDF is a data.frame with the ISO3 country names plus a variable to
# merge to the map data

malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
# This will join your malDF data.frame to the country map data

pdf("ebov.pdf", width=8, height=6)

mapCountryData(malMap, nameColumnToPlot="West_Africa", 
               catMethod = "categorical",
               mapTitle="",addLegend=F,
               missingCountryCol = "wheat",
               mapRegion="world",
               xlim=c(-15,10),
               ylim=c(-5,25),
               oceanCol = "lightblue",
               colourPalette = "terrain"
)
#               add=T)


##### 4.1 merging with existing data
sPDF <- joinCountryData2Map(ebov,
                            joinCode = "NAME",
                            nameJoinColumn = "Country")

## This the data that we will plot
dF <- sPDF@data

### make our pie plot
par(mai= c(0,0,0.6,0),
    xaxs = "i",
    yaxs = "i")

mapPies(dF =dF,
        nameX="LON",
        nameY="LAT",
        nameZs =c("Positive",
                  "Negative"),#,
        zColours=c("red",
                   "powderblue"
        ),
        symbolSize = 5,        
        #oceanCol = "lightblue",
        #        landCol = "wheat",
        addSizeLegend=F,
        addCatLegend=F,
        mapRegion="world",
        xlim=c(-15,10),
        ylim=c(-5,25)
        ,add=T)

title(main=paste("Antibodies against Ebolavirus in West African people"),
      cex=3)

legend("bottomleft",#-180.1516,90,
       legend=c("Antibody positive",
                "Antibody negative"),
       col=c("red",
             "powderblue"
       ),
       pch=16,
       cex=0.8,
       pt.cex=1.5,
       bty="o",
       box.lty=0,
       horiz = F,
       bg="#FFFFFF70")
##
dev.off()
##

## Marv
theCountries <- c("BEN","BFA","CAF","CIV",
                  "CMR","GHA","GIN","GMB",
                  "GNB","LBR","NER","NGA",
                  "SEN","SLE","TCD","TGO","MLI")

# These are the ISO3 names of the countries you'd like to plot in red

malDF <- data.frame(country = c("BEN","BFA","CAF","CIV",
                                "CMR","GHA","GIN","GMB",
                                "GNB","LBR","NER","NGA",
                                "SEN","SLE","TCD","TGO","MLI"),
                    West_Africa = rep(1,17))
# malDF is a data.frame with the ISO3 country names plus a variable to
# merge to the map data

malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
# This will join your malDF data.frame to the country map data

pdf("marv.pdf", width=8, height=6)

mapCountryData(malMap, nameColumnToPlot="West_Africa", 
               catMethod = "categorical",
               mapTitle="",addLegend=F,
               missingCountryCol = "wheat",
               mapRegion="world",
               xlim=c(-15,10),
               ylim=c(-5,25),
               oceanCol = "lightblue",
               colourPalette = "terrain"
)
#               add=T)


##### 4.1 merging with existing data
sPDF <- joinCountryData2Map(marv,
                            joinCode = "NAME",
                            nameJoinColumn = "Country")

## This the data that we will plot
dF <- sPDF@data

### make our pie plot
par(mai= c(0,0,0.6,0),
    xaxs = "i",
    yaxs = "i")

mapPies(dF =dF,
        nameX="LON",
        nameY="LAT",
        nameZs =c("Positive",
                  "Negative"),#,
        zColours=c("red",
                   "powderblue"
        ),
        symbolSize = 5,        
        #oceanCol = "lightblue",
        #        landCol = "wheat",
        addSizeLegend=F,
        addCatLegend=F,
        mapRegion="world",
        xlim=c(-15,10),
        ylim=c(-5,25)
        ,add=T)

title(main=paste("Antibodies against Marburgvirus in West African people"),
      cex=3)

legend("bottomleft",#-180.1516,90,
       legend=c("Antibody positive",
                "Antibody negative"),
       col=c("red",
             "powderblue"
       ),
       pch=16,
       cex=0.8,
       pt.cex=1.5,
       bty="o",
       box.lty=0,
       horiz = F,
       bg="#FFFFFF70")
##
dev.off()

###

RES$sp<-RES$Positive/RES$Number
RES$sp_scale<-RES$sp/RES$Number
ebov<- RES[RES$Genus == "EBOV", ]
marv<- RES[RES$Genus == "MARV", ]

ebov$Country[2]<-"Burkina Faso"
ebov$Country[4]<-"Central African Republic"
ebov$Country[10]<-"Sierra Leone"

marv$Country[2]<-"Central African Republic"
marv$Country[6]<-"Sierra Leone"
#
ebov$country <- c("BEN","BFA","CMR",'CAF','TCD',"GIN","LBR","NGA",
                  "SEN","SLE")
marv$country <- c("CMR",'CAF','TCD',"LBR","NGA",
                  "SLE")

# These are the ISO3 names of the countries you'd like to plot in red
# merge to the map data

malERsp <- joinCountryData2Map(ebov, joinCode = "ISO3",
                               nameJoinColumn = "country")
#
# This will join your malDF data.frame to the country map data
pdf("ebov_sp_results.pdf", width=8, height=6)

mapCountryData(malMap, nameColumnToPlot="West_Africa", 
               catMethod = "categorical",
               mapTitle="",addLegend=F,
               missingCountryCol = "wheat",
               mapRegion="world",
               xlim=c(-15,20),
               ylim=c(-5,25),
               oceanCol = "lightblue",
               colourPalette = "terrain"
)

mapParams<-mapCountryData(malERsp, nameColumnToPlot="sp", 
                          catMethod = "categorical",
                          mapTitle="Risk based on anti-Ebolavirus seroprevalence results",
                          addLegend=F,
                          #missingCountryCol = "wheat",
                          mapRegion="world",
                          xlim=c(-15,20),
                          ylim=c(-5,25),
                          oceanCol = "lightblue",
                          colourPalette = "heat",
               add=T)

##
dev.off()

max(marv$sp)/max(ebov$sp)

malMRsp <- joinCountryData2Map(marv, joinCode = "ISO3",
                               nameJoinColumn = "country")
#
# This will join your malDF data.frame to the country map data
pdf("marv_sp_results.pdf", width=8, height=6)

mapCountryData(malMap, nameColumnToPlot="West_Africa", 
               catMethod = "categorical",
               mapTitle="",addLegend=F,
               missingCountryCol = "wheat",
               mapRegion="world",
               xlim=c(-15,20),
               ylim=c(-5,25),
               oceanCol = "lightblue",
               colourPalette = "terrain"
)

mapParams<-mapCountryData(malMRsp, nameColumnToPlot="sp", 
                          catMethod = "categorical",
                          mapTitle="Anti-Marburg seroprevalence",
                          addLegend=F,
                          #missingCountryCol = "wheat",
                          mapRegion="world",
                          xlim=c(-15,20),
                          ylim=c(-5,25),
                          oceanCol = "lightblue",
                          colourPalette = c("#FFFFD5FF", # 1
                                            "#FFFF80FF", # 2
                                            "#FFFF2AFF", # 3
                                            "#FFFF00FF", # 4
                                            "#FFDF00FF", # 5
                                            "#FFBF00FF"), # 6
                          #)
                          add=T)
dev.off()
##
## scaled by total tested
pdf("ebov_sp_results_scaled.pdf", width=8, height=6)

mapCountryData(malMap, nameColumnToPlot="West_Africa", 
               catMethod = "categorical",
               mapTitle="",addLegend=F,
               missingCountryCol = "wheat",
               mapRegion="world",
               xlim=c(-15,20),
               ylim=c(-5,25),
               oceanCol = "lightblue",
               colourPalette = "terrain"
)

mapParams<-mapCountryData(malERsp, nameColumnToPlot="sp_scale", 
                          catMethod = "categorical",
                          mapTitle="Risk based on anti-Ebolavirus seroprevalence results",
                          addLegend=F,
                          #missingCountryCol = "wheat",
                          mapRegion="world",
                          xlim=c(-15,20),
                          ylim=c(-5,25),
                          oceanCol = "lightblue",
                          colourPalette = "heat",
                          add=T)

##
dev.off()

max(marv$sp)/max(ebov$sp)

malMRsp <- joinCountryData2Map(marv, joinCode = "ISO3",
                               nameJoinColumn = "country")
#
# This will join your malDF data.frame to the country map data
pdf("marv_sp_results_scaled.pdf", width=8, height=6)

mapCountryData(malMap, nameColumnToPlot="West_Africa", 
               catMethod = "categorical",
               mapTitle="",addLegend=F,
               missingCountryCol = "wheat",
               mapRegion="world",
               xlim=c(-15,20),
               ylim=c(-5,25),
               oceanCol = "lightblue",
               colourPalette = "terrain"
)

mapParams<-mapCountryData(malMRsp, nameColumnToPlot="sp_scale", 
                          catMethod = "categorical",
                          mapTitle="Anti-Marburg seroprevalence",
                          addLegend=F,
                          #missingCountryCol = "wheat",
                          mapRegion="world",
                          xlim=c(-15,20),
                          ylim=c(-5,25),
                          oceanCol = "lightblue",
                          colourPalette = c("#FFFFD5FF", # 1
                                            "#FFFF80FF", # 2
                                            "#FFFF2AFF", # 3
                                            "#FFFF00FF", # 4
                                            "#FFDF00FF", # 5
                                            "#FFBF00FF"), # 6
                          #)
                          add=T)
dev.off()
##

data_tot<-aggregate(f_data$Number~ f_data$Country,FUN=sum)
data_tot$Country<-c("BEN","BFA","CMR","CAF","TCD","CIV",
                  "GIN","LBR","NGA",
                  "SEN","SLE")
colnames(data_tot)<-c("C","Number","Country")
data_tot<-data_tot[,-1]
missing_dat <- data.frame(Country = c("GHA","GMB",
                                "GNB","NER",
                                "TGO","MLI"),
                    Number = rep(0,6) 
                                )
dat<-rbind(data_tot,missing_dat)

malMap <- joinCountryData2Map(dat, joinCode = "ISO3",
nameJoinColumn = "Country")
# This will join your malDF data.frame to the country map data
pdf("tested.pdf", width=8, height=6)

mapParams<-mapCountryData(malMap, nameColumnToPlot="Number", 
                             catMethod = "categorical",
                             mapTitle="People tested for filovirus antibodies",
                              addLegend=F,
                             missingCountryCol = "wheat",
                             mapRegion="world",
                             xlim=c(-15,20),
                             ylim=c(-5,25),
                             oceanCol = "lightblue",
                             colourPalette = "heat"
                    )
                    #               add=T)

#adding a modified legend by specifying extra parameters
do.call( addMapLegendBoxes, c(mapParams,title="Numbers",cex=0.8))
dev.off()

##
