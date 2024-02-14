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

###plant_ids[2]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[2]))
tresp_id2 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[2]))
summary(tresp_id2)
### keep: yes
### comments: no issues, good curve

###plant_ids[3]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[3]))
tresp_id3 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[3]))
summary(tresp_id3)
### keep: yes
### comments: no issue, good curve

###plant_ids[4]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[4]))
tresp_id4 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[4]))
summary(tresp_id4)
### keep: yes
### comments: no issue

###plant_ids[5]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[5]))
tresp_id5 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[5]))
summary(tresp_id5)
### keep: yes
### comments: no issue

###plant_ids[6]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[6]))
tresp_id6 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[6]))
summary(tresp_id6)
### keep: yes
### comments: no issue

###plant_ids[7]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[7]))
tresp_id7 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[7]))
summary(tresp_id7)
### keep: yes
### comments: no issue

###plant_ids[8]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[8]))
tresp_id8 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[8]))
summary(tresp_id8)
### keep: yes
### comments: one outlier, ask nick to cut

###plant_ids[9]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[9]))
tresp_id9 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[9]))
summary(tresp_id9)
### keep: yes
### comments: Looks good, ask nick

###plant_ids[10]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[10]))
tresp_id10 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[10]))
summary(tresp_id10)
### keep: yes
### comments: Looks good

###plant_ids[11]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[11]))
tresp_id11 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[11]))
summary(tresp_id11)
### keep: yes
### comments: Looks good

###plant_ids[12]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[12]))
tresp_id12 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[12]))
summary(tresp_id12)
### keep: yes
### comments: Looks good, ask nick

##plant_ids[13]
#plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[13]))
##summary(tresp_id13)
### keep: Ask Nick
### comments: weird data error

plant_ids[12]
