####################################################################################
##############        GRADIENT EFFECTS ON BIRD COMMUNITIES           ###############
##############              Luis Daniel Montalvo                     ###############
####################################################################################

#########################   DATA MANIPULATION   ####################################

#  Set directory
setwd("C:/Users/Owner/Desktop/Dropbox/Thesis/Count_Analysis/data_carpentry")
  
load("data_carpentry_gradt.image.RData")

#####  BIRD COUNTS
  
# Data with bird records per point count
survey.all.gradt <- read.csv("raw_data.csv", header=TRUE, sep=",")
survey.all.gradt <- as.data.frame(survey.all.gradt)
# Formating the date to subset
survey.all.gradt$Date <-as.Date(as.character(survey.all.gradt$Date), "%m/%d/%Y")
# Filtering data using date
survey.gradt <-survey.all.gradt[survey.all.gradt$Date >="2016-01-01" & survey.all.gradt$Date <= "2019-12-31",]
# Selecting the columns needed
survey.raw.gradt <- survey.gradt[,c("scientific.name", "Ind", "Point", "Time","Date", "Obs")]
# Getting rid of the records that 
survey.raw.gradt <- survey.raw.gradt[survey.raw.gradt[, "Obs"] != "FC",]
survey.raw.gradt <- survey.raw.gradt[,-6]
# formating time as seconds of the day
#install.packages("lubridate")
library(lubridate)

survey.raw2 <- survey.raw.gradt
colnames(survey.raw2)[4:5] <- c("time.ori", "date.ori")
# Formating time
survey.raw2$Time <- as.numeric(hm(survey.raw2$time.ori))
# Formatting date as numeric
survey.raw2$Date <- as.numeric(survey.raw2$date.ori)
# Generating unique code for point and day of survey. I will use it to merge the survey data
survey.raw2$pdate <- apply(survey.raw2[ , c("Point", "Date")] , 1 , paste , collapse = "_" )


# Remove the no indentified "NI"
survey.raw.ni <- survey.raw2[survey.raw2$scientific.name != "NI",]
# Updating the taxonomy to Clements 2019
# Reading Clements 2019 from Cornell Lab of Ornithology
clements <- read.csv("Clements2019.csv", header=TRUE, sep=",")
# Merging data bases to identify mismatches between clements and the used taxonomy
scinames.merge <- merge(survey.raw.ni, clements, by=c("scientific.name"), all.x = TRUE)
# Get the names that don't match the Clements 2019
names.change <- unique(subset(scinames.merge, is.na(scinames.merge$English.name))$scientific.name)

# Names that should be changed:

# 1	Actitis macularia	Actitis macularius
# 2	Contopus punensis	Contopus cinereus
# 3	Cyanocompsa cyanoides	Cyanoloxia cyanoides
# 4	Damophila julie	Juliamyia julie
# 5	Dives warszewiczi	Dives warczewiczi
# 6	Myarchus tuberculifer	Myiarchus tuberculifer
# 7	Myrmeciza berlepschi	Sipia berlepschi
# 8	Myrmeciza exsul	Poliocrania exsul
# 9	Pachyramphus spodirus	Pachyramphus spodiurus
# 10	Parula pitiayumi	Setophaga pitiayumi
# 11	Phaethornis baroni	Phaethornis longirostris
# 12	Pipra mentalis	Ceratopipra mentalis
# 13	Sporagra siemiradzkii	Spinus siemiradzkii
# 14	Sturnella bellicosa	Leistes bellicosus
# 15	Tangara cyanicollis	Stilpnia cyanicollis
# 16	Thalurania hypochlora	Thalurania colombica
# 17	Tolmomyias flavotectus	Tolmomyias assimilis
# 18	Veniliornis callonotus	Dryobates callonotus
# 19	Veniliornis kirkii	Dryobates kirkii


# Update the names
survey.raw.ni$scientific.name<- gsub("Actitis macularia", 
                                     "Actitis macularius", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Contopus punensis", 
                                     "Contopus cinereus", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Cyanocompsa cyanoides", 
                                  "Cyanoloxia cyanoides", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Damophila julie", 
                                     "Juliamyia julie", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Dives warszewiczi", 
                                  "Dives warczewiczi", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Myarchus tuberculifer", 
                                     "Myiarchus tuberculifer", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Myrmeciza berlepschi", 
                                     "Sipia berlepschi", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Myrmeciza exsul", 
                                  "Poliocrania exsul", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Pachyramphus spodirus", 
                                     "Pachyramphus spodiurus", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Parula pitiayumi", 
                                     "Setophaga pitiayumi", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Phaethornis baroni", 
                                     "Phaethornis longirostris", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Pipra mentalis", 
                                     "Ceratopipra mentalis", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Sporagra siemiradzkii", 
                                     "Spinus siemiradzkii", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Sturnella bellicosa", 
                                     "Leistes bellicosus", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Tangara cyanicollis", 
                                     "Stilpnia cyanicollis", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Thalurania hypochlora", 
                                     "Thalurania colombica", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Tolmomyias flavotectus", 
                                     "Tolmomyias assimilis", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Veniliornis callonotus",
                                  "Dryobates callonotus", survey.raw.ni$scientific.name)
survey.raw.ni$scientific.name<- gsub("Veniliornis kirkii",
                                     "Dryobates kirkii", survey.raw.ni$scientific.name)



##### TIME COVARIATES AND EFFORT

# Data time effort
time.grandt <- read.csv("Time.csv", header=TRUE, sep=",")
# Changing names of dates
colnames(time.grandt)[c(1,3)] <- c("Point", "date.ori")
# Formating the date to subset
time.grandt$date.ori <-as.Date(as.character(time.grandt$date.ori), "%m/%d/%Y")
# Formatting date as numeric
time.grandt$Date <- as.numeric(time.grandt$date.ori)
# Generating unique code for point and day of survey. I will use it to merge the survey data
time.grandt$pdate <- apply(time.grandt[ , c("Point", "Date")] , 1 , paste , collapse = "_" )
# Getting a column for time as numeric
time.grandt$Time <- as.numeric(hm(time.grandt$Time.1))


# GETTING COVARIATES
#install.packages("doBy")
library(doBy)

# Getting a dataframe with covariates
cov.det.gran <- summaryBy(Time ~ pdate + Point + Survey + Season + Date, data=time.grandt, FUN=c(min))

# Get the number survey per point
test <- as.data.frame.matrix(table(cov.det.gran$Point, cov.det.gran$Survey))
test$freq <- rowSums(test[1:7])
# Getting the point with just one or more than six surveys
test2 <- test[test[ ,8] == 1 | test[ ,8] == 7, ]

# The points that should not be included (one or more than six surveys)
'%ni%' <- Negate('%in%')
cov.det.gran <- cov.det.gran[cov.det.gran$Point %ni% c("EP21", "EP22", "EP23", "EP24", "EP25", "LC11", "LC12"), ]         


##### Getting a data frame with zeros in the point counts where the species weren't recorded

# Getting the list of species
spp.names.gran <- unique(survey.raw.ni$scientific.name)

# Getting the list of names for points
point.names.gran <- (unique(cov.det.gran$pdate))

# Getting an combination of species and points and then getting them in the same column
comb2 <- expand.grid(spp.names.gran, point.names.gran, stringsAsFactors = FALSE)
comb2$spp.point <- apply(comb2[ , c("Var1", "Var2")] , 1 , paste , collapse = "_" )

# Getting the combination of species and points in the same column for survey.raw
survey.raw.ni$spp.point <- apply(survey.raw.ni[,c("scientific.name","Point", "Date")], 1, paste, collapse = "_")

# Join the surveys with data and the list of point sampled
survey.gran.merged <- merge(survey.raw.ni, comb2,  by=c("spp.point"), all.y = TRUE)

# Getting the columns that we want
survey.gran.merged2 <- survey.gran.merged[,c("spp.point", "Var1", "Ind", "Var2")]
colnames(survey.gran.merged2)[c(2,4)] <- c("scientific.name", "pdate")

# Filling the NA cells with zeros
survey.gran.merged2[is.na(survey.gran.merged2)] <- 0

# There are some duplicates because for a given survey and point I recorded 
# male and female or one record visually and another acustically in different lines
# so here I sum these records per survey and point
birdland.gran <- summaryBy(Ind ~ spp.point + scientific.name + pdate, data=survey.gran.merged2, FUN=c(sum))
colnames(birdland.gran)[c(4)] <- c("count")

# Adding covariates for time, date, survey, season
birdland.cov <- merge(birdland.gran, cov.det.gran, by=("pdate"))

# Creating an ID for the points
id.point <- as.data.frame(unique(birdland.cov$Point))
id.point$id_point <- cumsum(!duplicated(id.point[1]))
names(id.point)[1] <- paste("Point")
birdland.cov <- merge(birdland.cov, id.point, by=("Point"))


# These two lines create a species id pasting the three first letter of the genus and 
# specific names. First, I changed to character species names, then split the genus and specific name,
# extract the three first letters of the first words (genus) and paste it with the 
# three first letters of the second word (specific name) and convert them to upper case
birdland.cov$idspp <- toupper(paste(substring(sapply(strsplit(as.character(birdland.cov$scientific.name), " "), "[", 1), 1, 3),
                                  substring(sapply(strsplit(as.character(birdland.cov$scientific.name), " "), "[", 2), 1, 3), sep = ""))

# Cyanerpes cyaneus and Cyanocompsa cyanoides and Henicorhina leucosticta 
# and Henicorhina leucophrys have the same species ID

birdland.cov$idspp[birdland.cov$scientific.name %in% "Cyanerpes cyaneus"] <- "CYRCYU"
birdland.cov$idspp[birdland.cov$scientific.name %in% "Henicorhina leucosticta"] <- "HENLET"


# Reordering the columns of the dataframe
birdland.cov <- birdland.cov[,c(4,11,1,10,5,6,7,8,9)]
colnames(birdland.cov) <- c("species", "idspp", "point", "idpoint", "count", "survey", "season", "date", "time")


##########################################################################################

#####  ESTIMATING PCA WITH VEGETATION STRUCTURE

#  Using correlation with raw data
  
# Reading vegetation data
veg.den <- read.csv("VegDen.csv", header=TRUE, sep=",")
BA <- read.csv("Trees.csv", header=TRUE, sep=",")

# Merge the Basal areas of trees and vegetation density data
veg.struc.raw <- merge(veg.den, BA, by=("Point"))
veg.struc.raw <- veg.struc.raw[,c("Point", "CanDen", "Fol","GroundDen","Height","BA")]
  


#  PCA FOR VEGETATION
vegstrucpca2 <- princomp(veg.struc.raw[,2:6], cor = T)
summary(vegstrucpca2)
plot(vegstrucpca2, type="lines")
loadings(vegstrucpca2)
veg.score2 <- vegstrucpca2$scores
as.data.frame(veg.score2)

#install.packages("Hmisc")
library(Hmisc)


veg.struc.raw $veg1 <- veg.score2[,1]
veg.struc.raw $veg2 <- veg.score2[,2]

IV <- veg.struc.raw[,c(2:8)]
rcorr(as.matrix(IV), type=c("pearson"))

####  Plotting multiple correlations

#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
cor.veg <- veg.struc.raw[,c("CanDen","GroundDen", "Fol", "Height","BA",
                                           "veg1","veg2")]

chart.Correlation(cor.veg, method = "pearson", histogram = TRUE, pch = 16)


####    with scaled data
#veg.struc.scaled <- as.data.frame(scale(veg.struc.raw[,c(2:6)]))
#vegstrucpca3 <- princomp(veg.struc.scaled, cor = F)


#veg.score3 <- vegstrucpca3$scores
#as.data.frame(veg.score3)

# VEG STRUCTURE PER POINT COUNT
#veg.struc.scaled$veg1 <- veg.score3[,1]
#veg.struc.scaled$veg2 <- veg.score3[,2]
#IV2 <- veg.struc.scaled[,c(1:7)]

#rcorr(as.matrix(IV2), type=c("pearson"))


##########################################################################################

#####  SPATIAL DATA

# Reading the spatial data
spatial.raw <- read.csv("Space.csv", header=TRUE, sep=",")

spatial.raw <- spatial.raw[spatial.raw$Point %ni% c("EP21", "EP22", "EP23", "EP24", "EP25", "LC11", "LC12"), ]         


library(raster)
library(sp)

# Getting the worldclim values
r <- getData("worldclim",var="bio",res=10)

# Getting the bioclim variables I need: Annual mean prep and Prep seasonality
r <- r[[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]]
names(r) <- c("AMT","MDR","ISO","TS","MTWM","MTCM","TAR","MTWetQ","MTDQ","MTWarQ","MTCQ","AMP","PWetM","PDM","PS","PWetQ","PDQ","PWarQ","PCQ")

# Getting the columns I need
coords <- spatial.raw[,c("Point", "Site", "X", "Y")]

# Projecting the coordinates
points <- SpatialPoints(coords[,3:4], proj4string = r@crs)

# Extracting values of climate
values <- extract(r,points)

# Merging the coordinates and the values
coor.clim <- cbind.data.frame(coordinates(points),values)

# Merging the names of points, values and coordinates
clim.value <- cbind.data.frame(coords, coor.clim)
clim.value <- clim.value[,-c(3:4)]

write.csv(clim.value, file = "climate.csv", row.names = FALSE)

# Code for the sites
clim.value$site <- as.numeric(clim.value$Site)


##########################################################################################

#####  MERGING THE DATA

###############
####### COMPLETE DATA-SET: with 248 sites and 247 species

# Merging climate and vegetation structure
clim.veg2 <- merge(veg.struc.raw, clim.value, by=c("Point"))

# Writting the counts per survey for all samples
write.csv(clim.veg2, file = "climveg.csv", row.names = FALSE)


### Getting the complete data set for 247 species

# Merging count to filter the point we have data for point with covaraites
birdland1 <- merge(clim.veg2, birdland.cov, by.x = c("Point"), by.y = c("point"))

# Final count data
birdland.gradt <- birdland1[,-c(2:14)]

# Writting the counts per survey
write.csv(birdland.gradt, file = "birdcountgrad.csv", row.names = FALSE)


###############
####### REDUCED DATA-SET: with 50 sites and 50 species

### Reduced climate-vegetation date set (50 points)

# Reducing the number of points to test the model
clim.veg50 <- clim.veg2[sample(nrow(clim.veg2), 50), ]

# Writting the counts per survey for just 50
write.csv(clim.veg50, file = "climveg50.csv", row.names = FALSE)


### Getting reduce date set (50 species)

# Getting the list of species to reduce the number of species
spec.list1 <- as.data.frame(unique(birdland.cov$idspp))


# Reducing the number of species to test the model
spec.list.red <- spec.list1[sample(nrow(spec.list1), 50), ]

# Reduce the original data set with the vector
birdland.cov50 <- as.data.frame(birdland.cov[birdland.cov$idspp %in% spec.list.red,])

# Merging count to filter the point we have data for point with covariates
birdland50 <- merge(clim.veg50, birdland.cov50, by.x = c("Point"), by.y = c("point"))

# Final count data
birdland.gradt50 <- birdland50[,-c(2:14)]

# Writting the counts per survey
write.csv(birdland.gradt50, file = "birdcountgrad50.csv", row.names = FALSE)



# Number of points were each species was observed
point.spec <- dcast(birdland.gradt, species + idspp ~ idpoint, sum, value.var = "count")
point.spec$npoint <- rowSums(point.spec[,3:250] != 0)
point.spec <- point.spec[,c(1,2,251)]
# how many species were recorded in how many points
table(point.spec$npoint)


# Number of points were each species was observed
point.spec50 <- dcast(birdland.gradt50, species + idspp ~ idpoint, sum, value.var = "count")
point.spec50$npoint <- rowSums(point.spec50[,3:52] != 0)
point.spec50 <- point.spec50[,c(1,2,53)]
# how many species were recorded in how many points
table(point.spec50$npoint)


###############
####### REDUCED DATA-SET: with 248 sites and 50 species

### Getting reduce date set (50 species)

### Getting the complete data set for 247 species

# Merging count to filter the point we have data for point with covariates
birdland.sp50 <- merge(clim.veg2, birdland.cov50, by.x = c("Point"), by.y = c("point"))


# Final count data
birdland.gradt.sp50 <- birdland.sp50[,-c(2:14)]

# Writting the counts per survey
write.csv(birdland.gradt.sp50, file = "birdcount.sp50.csv", row.names = FALSE)







#############################################################################################

####### OBTAINING THE HABITATS FOR EACH SPECIES


## Gettting the habitat of the species using rredlist
{
  

  ### Getting just the column of species
  bird_spp_all <- as.data.frame(unique(birdland.cov$species))
  #write.csv(bird_spp_all, file = "species_list.csv")
  ### Changing the names of the column of the species
  colnames(bird_spp_all)<-c("species")
  
  #install.packages("rredlist")
  library(rredlist)
  # This function requires obtaining a token (alpha numeric key) from IUCN
  # It is possible ask for it at http://apiv3.iucnredlist.org/api/v3/token
  # Setting the environmental variable after obtaining the token from IUCN
  Sys.setenv("IUCN_REDLIST_KEY"="91c8d31431e9630b85b03c5f4a49a6344750fc467bda9f8a11532857688c6513")
  Sys.getenv("IUCN_REDLIST_KEY")
  
  # Running the function for all species in the vector of species and storage the result in hab.list
  # I couln't run it for all species at once, I run it every n species changing the numbers
  spp1<-as.character(bird_spp_all$species[1:246]) # it requires to transform to character
  hab.list<-list()
  for (i in 1:length(spp1)){
    hab.list[[i]]<-rl_habitats(spp1[i])
  }
  
  
  # Storage the results in a list with the name of the species for all rows
  list.m <- list()
  for(i in 1:length(hab.list)){
    list.m[[i]]<- as.data.frame(hab.list[[i]]$result)
    list.m[[i]]$SPECIES <-as.vector(rep(hab.list[[i]]$name, nrow(list.m[[i]])))
  }
  
  # Converting the list in a data frame
  #install.packages("dplyr")
  library(dplyr)
  
  # Converting to dataframe
  test<-bind_rows(list.m)
  
  # There might be some species names that don't match  the IUCN names
  # Here, we get the list of species that we couldn't find in IUCN
  list.lack <- unique((birdland.cov[!birdland.cov$species %in% test$SPECIES,])$species)
  
  # We got 12 species, we change the names in a new data set and repeat the process above
  # Getting a copy of the original data set
  bird.iucn <-birdland.cov
  
  
  bird.iucn$species <- gsub("Phylloscartes ophthalmicus", 
                               "Pogonotriccus ophthalmicus", bird.iucn$species)
  bird.iucn$species <- gsub("Chlorothraupis olivacea", 
                               "Habia olivacea", bird.iucn$species)
  bird.iucn$species <- gsub("Dryobates callonotus", 
                            "Veniliornis callonotus", bird.iucn$species)
  bird.iucn$species <- gsub("Tachyphonus luctuosus", 
                            "Islerothraupis luctuosa", bird.iucn$species)
  bird.iucn$species <- gsub("Dryocopus lineatus", 
                            "Hylatomus lineatus", bird.iucn$species)
  bird.iucn$species <- gsub("Thraupis episcopus", 
                            "Tangara episcopus", bird.iucn$species)
  bird.iucn$species <- gsub("Trogon caligatus", 
                            "Trogon violaceus", bird.iucn$species)
  bird.iucn$species <- gsub("Tachyphonus delatrii", 
                            "Chrysocorypha delatrii", bird.iucn$species)
  bird.iucn$species <- gsub("Dryobates kirkii", 
                            "Veniliornis kirkii", bird.iucn$species)
  bird.iucn$species <- gsub("Thraupis palmarum", 
                            "Tangara palmarum", bird.iucn$species)
  bird.iucn$species <- gsub("Stilpnia cyanicollis", 
                            "Tangara cyanicollis", bird.iucn$species)
  bird.iucn$species <- gsub("Xenops rutilans", 
                            "Xenops rutilus", bird.iucn$species)
  
  # After changing the names, repeat the proces above using bird.iucn data set
  
  ### Getting just the column of species
  bird_spp_iucn <- as.data.frame(unique(bird.iucn$species))
  #write.csv(bird_spp_all, file = "species_list.csv")
  ### Changing the names of the column of the species
  colnames(bird_spp_iucn)<-c("species")
  
  # Running the function for all species in the vector of species and storage the result in hab.list
  # I couln't run it for all species at once, I run it every n species changing the numbers
  spp1.iucn<-as.character(bird_spp_iucn$species[1:246]) # it requires to transform to character
  hab.list.iucn <- list()
  for (i in 1:length(spp1.iucn)){
    hab.list.iucn[[i]]<-rl_habitats(spp1.iucn[i])
  }
  
  # Storage the results in a list with the name of the species for all rows
  list.iucn <- list()
  for(i in 1:length(hab.list.iucn)){
    list.iucn[[i]]<- as.data.frame(hab.list.iucn[[i]]$result)
    list.iucn[[i]]$SPECIES <-as.vector(rep(hab.list.iucn[[i]]$name, nrow(list.iucn[[i]])))
  }
  
  # Converting to dataframe
  test.iucn<-as.data.frame(bind_rows(list.iucn))
  
  
  #######  DRY FOREST SPECIES
  
  ###Getting a list of forest-dependent species
  forest.dry <- c("Forest - Subtropical/Tropical Dry")
  
  ## DRY FOREST SPECIALIST, species for which forest is the main habitat
  spp.forest.dry <- subset(test.iucn, (test.iucn$habitat == forest.dry & test.iucn$majorimportance == "Yes")) 
  dry.for.spp <- as.data.frame(unique(spp.forest.dry$SPECIES))
  colnames(dry.for.spp) <- "species"
  
  ##  Deleting some species that I know are not dry forest dependent based on experience
  dry.for.spp <- dry.for.spp[dry.for.spp$species %ni% c("Amazilia amazilia", "Columbina buckleyi"),]
  
  ## Getting into format 
  dry.for.spp <- as.data.frame(dry.for.spp)
  colnames(dry.for.spp) <- "species"
  
  ## Adding a column with the name of the habitat
  dry.for.spp$hab <- "dry forest"
  
  
  #######  WET FOREST SPECIES
  
  ###Getting a list of forest-dependent species
  forest.wet <- c("Forest - Subtropical/Tropical Moist Lowland", "Forest - Subtropical/Tropical Moist Montane")
  
  ## WET FOREST SPECIALIST, species for which forest is the main habitat
  spp.forest.wet <- subset(test.iucn, (test.iucn$habitat == forest.wet & test.iucn$majorimportance == "Yes")) 
  wet.for.spp <- as.data.frame(unique(spp.forest.wet$SPECIES))
  colnames(wet.for.spp) <- "species"
  
  ##  Deleting some species that I know are not forest dependent based on experience
  wet.for.spp <- wet.for.spp[wet.for.spp$species %ni% c("Heliomaster longirostris", "Myrmotherula pacifica",
                                                 "Taraba major", "Pachyramphus spodiurus", "Sittasomus griseicapillus",
                                                 "Tangara palmarum", "Leptotila ochraceiventris", "Leptotila ochraceiventris",
                                                 "Amazilia tzacatl", "Megaceryle torquata", "Brotogeris pyrrhoptera",
                                                 "Dysithamnus mentalis"),]
  
  ## Getting into format 
  wet.for.spp <- as.data.frame(wet.for.spp)
  colnames(wet.for.spp) <- "species"
  
  ## Adding a column with the name of the habitat
  wet.for.spp$hab <- "wet forest"
  

    # Putting together the wet and dry forest specialists
  hab.for <- rbind(wet.for.spp, dry.for.spp)
  
  #######  DISTURBANCE-TOLERANT SPECIES 
  
  ### Getting a list of disturbance tolerant species, not in forest at all and are present in any of these habitat
  dist1 <- c("Artificial/Terrestrial - Arable Land")
  dist2 <- c("Artificial/Terrestrial - Pastureland")
  dist3 <- c("Artificial/Terrestrial - Plantations")
  dist4 <- c("Artificial/Terrestrial - Rural Gardens")
  dist5 <- c("Artificial/Terrestrial - Urban Areas")
  dist6 <- c("Artificial/Terrestrial - Subtropical/Tropical Heavily Former Forest")
  dist7 <- c("Artificial/Aquatic - Water Storage Areas (over 8ha)")
  dist8 <- c("Shrubland - Subtropical/Tropical Moist")
  dist9 <- c("Shrubland - Subtropical/Tropical Dry")
  dist10 <- c("Shrubland - Subtropical/Tropical High Altitude")
  dist11 <- c("Grassland - Subtropical/Tropical Dry")
  dist12 <- c("Shrubland - Temperate")
  dist13 <- c("Grassland - Temperate")
  dist14 <- c("Grassland - Subtropical/Tropical Seasonally Wet/Flooded")
  dist15 <- c("Grassland - Subtropical/Tropical High Altitude")
  dist16 <- c("Artificial/Aquatic - Canals and Drainage Channels, Ditches")
  dist17 <- c("Artificial/Aquatic - Irrigated Land (includes irrigation channels)")
  
  # subsetting the data set for disturbed habitats
  spp.dist1 <- subset(test.iucn,  test.iucn$habitat == dist1 | test.iucn$habitat == dist2 | 
                                  test.iucn$habitat == dist3 | test.iucn$habitat == dist4 | 
                                  test.iucn$habitat == dist5 | test.iucn$habitat == dist6 |
                                  test.iucn$habitat == dist7 | test.iucn$habitat == dist8 |
                                  test.iucn$habitat == dist10 |
                                  test.iucn$habitat == dist11 | test.iucn$habitat == dist12 |
                                  test.iucn$habitat == dist13 | test.iucn$habitat == dist14 |
                                  test.iucn$habitat == dist15 | test.iucn$habitat == dist16 |
                                  test.iucn$habitat == dist17)
  
  # List of species in disturbed habitats
  spp.dist2 <- as.data.frame(unique(spp.dist1$SPECIES))
  colnames(spp.dist2) <- "species"
  
  ## Deleting the species that are forest dependent
  spp.dist2 <- as.data.frame(spp.dist2[!spp.dist2$species %in% hab.for$species, ])
  colnames(spp.dist2) <- "species"
  
  # Deleting the species that are not open-habitats on experience
  spp.dist2 <- as.data.frame(spp.dist2[spp.dist2$species %ni% c("Campylorhamphus trochilirostris", "Camptostoma obsoletum",
                                                  "Cercomacroides tyrannina", "Sittasomus griseicapillus", 
                                                  "Malacoptila panamensis", "Ramphastos ambiguus", 
                                                  "Mionectes olivaceus", "Mionectes oleagineus",
                                                  "Aulacorhynchus haematopygus", "Dendrocincla fuliginosa", 
                                                  "Turdus reevei", "Euscarthmus meloryphus",
                                                  "Tachyphonus rufus", "Ramphocaenus melanurus"),])
 
   colnames(spp.dist2) <- "species"
   
   ## Adding a column with the name of the habitat
   spp.dist2$hab <- "open-hab"
  
   ## Joint all the habitat we have so far
   hab.for.dis <- rbind(hab.for, spp.dist2)
  
  ## See what species we have left to classify
  gen1 <- as.data.frame(test.iucn[!test.iucn$SPECIES %in% hab.for.dis$species, ])
  gen <- as.data.frame(unique(gen1$SPECIES))
  
  colnames(gen) <- "species"
  
  ## Adding a column with the name of the habitat
  gen$hab <- "generalist"
  
  ## Joint all the data set
  hab.spp <- rbind(hab.for.dis, gen)
  
  
  ## Changing some names manually
  hab.spp1 <- edit(hab.spp)
  
  ## Creating a copy
  hab.spp2 <- hab.spp2
  
  
  ## Changing back the names of species that didn't match and were changed before
  hab.spp2$species <- gsub("Pogonotriccus ophthalmicus", 
                            "Phylloscartes ophthalmicus", hab.spp2$species)
  hab.spp2$species <- gsub("Habia olivacea", 
                            "Chlorothraupis olivacea", hab.spp2$species)
  hab.spp2$species <- gsub("Veniliornis callonotus", 
                            "Dryobates callonotus", hab.spp2$species)
  hab.spp2$species <- gsub("Islerothraupis luctuosa", 
                            "Tachyphonus luctuosus", hab.spp2$species)
  hab.spp2$species <- gsub("Hylatomus lineatus", 
                            "Dryocopus lineatus", hab.spp2$species)
  hab.spp2$species <- gsub("Tangara episcopus", 
                            "Thraupis episcopus", hab.spp2$species)
  hab.spp2$species <- gsub("Trogon violaceus", 
                            "Trogon caligatus", hab.spp2$species)
  hab.spp2$species <- gsub("Chrysocorypha delatrii", 
                            "Tachyphonus delatrii", hab.spp2$species)
  hab.spp2$species <- gsub("Veniliornis kirkii", 
                            "Dryobates kirkii", hab.spp2$species)
  hab.spp2$species <- gsub("Tangara palmarum", 
                            "Thraupis palmarum", hab.spp2$species)
  hab.spp2$species <- gsub("Tangara cyanicollis", 
                            "Stilpnia cyanicollis", hab.spp2$species)
  hab.spp2$species <- gsub("Xenops rutilus", 
                            "Xenops rutilans", hab.spp2$species)
  
 ## Removing duplicated row'
  hab.spp3 <- hab.spp2[!duplicated(hab.spp2$species), ]
  
  
  
  # Writting the habitat per species
  write.csv(hab.spp3, file = "hab.spp3.csv", row.names = FALSE)
  
  
  
  
  
  
  
  
  
  
  ###  SOME OTHER USEFUL CODE
  ### Getting the terrestrial species
  #spp.terr1 <- filter(test, !grepl("Marine-", habitat))
  #spp.terr <- unique(spp.terr1[c("SPECIES")])
  #colnames(spp.terr)<- "species"
  #spp.terr<-as.data.frame(apply(spp.terr,2,function(x)gsub('\\s+', '',x)))
  

  
}

















save.image("C:/Users/Owner/Desktop/Dropbox/Thesis/Count_Analysis/data_carpentry/data_carpentry_gradt.image.RData")

