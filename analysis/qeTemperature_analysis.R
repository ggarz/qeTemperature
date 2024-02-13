# qeTemperature_analysis.R
## scrip[t to analyze the quantum efficiency temperature response data

## load libraries
library(tidyverse)

## load in data
qe_data_raw <- read.csv("../data/licor/licor_data_clean.csv")

## have a look at the data
head(qe_data_raw$PhiPS2) # looks good!
hist(qe_data_raw$PhiPS2) # not a numeric column because of some div/0 errors

## subset data to include only good phips2 values
qe_data <- subset(qe_data_raw, PhiPS2 != "#DIV/0!")
qe_data$PhiPS2

## make a new phips2 numeric variable
qe_data$PhiPS2_num <- as.numeric(as.character(qe_data$PhiPS2))
hist(qe_data$PhiPS2_num)

## fit temperature response curves
plant_ids <- levels(as.factor(qe_data$id)) # extract all plant ids
qe_data$Tleaf_squared <- qe_data$Tleaf * qe_data$Tleaf

### plant_ids[1]
#plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[1]))
#tresp_id1 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[1]))
#summary(tresp_id1)
### keep: NO
### comments: only bouteloa so throwing this out

### plant_ids[2]
#plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[2]))
#tresp_id2 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[2]))
#summary(tresp_id2)
### keep: NO
### comments: only one point for the curve, so remove

### plant_ids[3]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[3] & PhiPS2 < 0.12))
tresp_id3 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, 
                data = subset(qe_data, id == plant_ids[3] & PhiPS2 < 0.12))
summary(tresp_id3)
### keep: YES
### comments: removed one outlier point at Tleaf = 30 (much higher than all other values)

### plant_ids[4]
#plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[4]))
#tresp_id4 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, 
                data = subset(qe_data, id == plant_ids[4]))
#summary(tresp_id4)
### keep: NO
### comments: only one point for the curve, so remove

###########
### TODO
##########
# fit temperature response curves for all individual plants

### plant_ids[4]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[4]))
tresp_id4 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[4]))
summary(tresp_id4)
