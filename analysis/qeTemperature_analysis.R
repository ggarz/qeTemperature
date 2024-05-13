# qeTemperature_analysis.R
## scrip[t to analyze the quantum efficiency temperature response data

## to do March 18
# 1. install updated version of R
# 2. try to run the library commands right below this
# 3. if problems with 2, try installing the estimability package (install.packages('estimability'))

## load libraries
library(tidyverse)
library(lme4)
library(car)
library(emmeans)

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
### comments: 

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

#plant_ids[13]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[13]))
tresp_id13 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[13]))
summary(tresp_id13)
### keep: yes
### comments: looks good

#plant_ids[14]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[14]))
tresp_id14 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[14]))
summary(tresp_id14)
### keep: yes
### comments: looks good

#plant_ids[15]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[15]))
tresp_id15 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[15]))
summary(tresp_id15)
### keep: yes
### comments: looks good

#plant_ids[16]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[16]))
tresp_id16 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[16]))
summary(tresp_id16)
### keep: yes
### comments: looks good

#plant_ids[17]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[17]))
tresp_id17 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[17]))
summary(tresp_id17)
### keep: yes
### comments: Maybe look at?

#plant_ids[18]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[18]))
tresp_id18 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[18]))
summary(tresp_id18)
### keep: yes
### comments: looks good

#plant_ids[19]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[19]))
tresp_id19 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[19]))
summary(tresp_id19)
### keep: yes
### comments: looks good

#plant_ids[20]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[20]))
tresp_id20 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[20]))
summary(tresp_id20)
### keep: yes
### comments: little spread out but should be ok

#plant_ids[21]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[21]))
tresp_id21 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[21]))
summary(tresp_id21)
### keep: maybe
### comments: very odd dataset, ask nick

#plant_ids[22]
#plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[22]))
#tresp_id22 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[22]))
#summary(tresp_id22)
### keep: Probably Not
### comments: very odd graph

#plant_ids[23]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[23]))
tresp_id23 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[23]))
summary(tresp_id23)
### keep: Yes
### comments: Looks good

#plant_ids[24]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[24]))
tresp_id24 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[24]))
summary(tresp_id24)
### keep: Yes
### comments: Looks good

#plant_ids[25]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[25]))
tresp_id25 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[25]))
summary(tresp_id25)
### keep: Yes
### comments: Looks good

#plant_ids[26]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[26]))
tresp_id26 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[26]))
summary(tresp_id26)
### keep: Yes
### comments: Looks good

#plant_ids[27]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[27]))
tresp_id27 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[27]))
summary(tresp_id27)
### keep: Yes
### comments: Looks good I think..? Similar yet better than 22

#plant_ids[28]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[28]))
tresp_id28 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[28]))
summary(tresp_id28)
### keep: Yes
### comments: Looks Good

#plant_ids[29]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[29]))
tresp_id29 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[29]))
summary(tresp_id29)
### keep: Yes
### comments: Looks Good, same as 27

#plant_ids[30]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[30]))
tresp_id30 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[30]))
summary(tresp_id30)
### keep: Yes
### comments: Looks Good

#plant_ids[31]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[31]))
tresp_id31 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[31]))
summary(tresp_id31)
### keep: Yes
### comments: Looks Good

#plant_ids[32]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[32]))
tresp_id32 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[32]))
summary(tresp_id32)
### keep: Yes
### comments: Looks Good

#plant_ids[33]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[33]))
tresp_id33 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[33]))
summary(tresp_id33)
### keep: Yes
### comments: Looks Good

#plant_ids[34]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[34]))
tresp_id34 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[34]))
summary(tresp_id34)
### keep: Yes
### comments: Looks Good

#plant_ids[35]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[35]))
tresp_id35 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[35]))
summary(tresp_id35)
### keep: Yes
### comments: Looks Good

#plant_ids[36]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[36]))
tresp_id36 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[36]))
summary(tresp_id36)
### keep: Yes
### comments: Looks Good

#plant_ids[37]
plot(PhiPS2~Tleaf, data = subset(qe_data, id == plant_ids[37]))
tresp_id37 <- lm(PhiPS2 ~ Tleaf_squared + Tleaf, data = subset(qe_data, id == plant_ids[37]))
summary(tresp_id37)
### keep: Yes
### comments: Little wonky but should be ok

### make a table of a, b, and c values with their SE's
plant2_coefs <- c(plant_ids[2], 
                  summary(tresp_id2)[[4]][2,1],
                  summary(tresp_id2)[[4]][2,2],
                  summary(tresp_id2)[[4]][3,1],
                  summary(tresp_id2)[[4]][3,2],
                  summary(tresp_id2)[[4]][1,1],
                  summary(tresp_id2)[[4]][1,2])

plant3_coefs <- c(plant_ids[3], 
                  summary(tresp_id3)[[4]][2,1],
                  summary(tresp_id3)[[4]][2,2],
                  summary(tresp_id3)[[4]][3,1],
                  summary(tresp_id3)[[4]][3,2],
                  summary(tresp_id3)[[4]][1,1],
                  summary(tresp_id3)[[4]][1,2])

plant4_coefs <- c(plant_ids[4], 
                  summary(tresp_id4)[[4]][2,1],
                  summary(tresp_id4)[[4]][2,2],
                  summary(tresp_id4)[[4]][3,1],
                  summary(tresp_id4)[[4]][3,2],
                  summary(tresp_id4)[[4]][1,1],
                  summary(tresp_id4)[[4]][1,2])

plant5_coefs <- c(plant_ids[5], 
                  summary(tresp_id5)[[4]][2,1],
                  summary(tresp_id5)[[4]][2,2],
                  summary(tresp_id5)[[4]][3,1],
                  summary(tresp_id5)[[4]][3,2],
                  summary(tresp_id5)[[4]][1,1],
                  summary(tresp_id5)[[4]][1,2])

plant6_coefs <- c(plant_ids[6], 
                  summary(tresp_id6)[[4]][2,1],
                  summary(tresp_id6)[[4]][2,2],
                  summary(tresp_id6)[[4]][3,1],
                  summary(tresp_id6)[[4]][3,2],
                  summary(tresp_id6)[[4]][1,1],
                  summary(tresp_id6)[[4]][1,2])

plant7_coefs <- c(plant_ids[7], 
                  summary(tresp_id7)[[4]][2,1],
                  summary(tresp_id7)[[4]][2,2],
                  summary(tresp_id7)[[4]][3,1],
                  summary(tresp_id7)[[4]][3,2],
                  summary(tresp_id7)[[4]][1,1],
                  summary(tresp_id7)[[4]][1,2])

plant8_coefs <- c(plant_ids[8], 
                  summary(tresp_id8)[[4]][2,1],
                  summary(tresp_id8)[[4]][2,2],
                  summary(tresp_id8)[[4]][3,1],
                  summary(tresp_id8)[[4]][3,2],
                  summary(tresp_id8)[[4]][1,1],
                  summary(tresp_id8)[[4]][1,2])

plant9_coefs <- c(plant_ids[9], 
                  summary(tresp_id9)[[4]][2,1],
                  summary(tresp_id9)[[4]][2,2],
                  summary(tresp_id9)[[4]][3,1],
                  summary(tresp_id9)[[4]][3,2],
                  summary(tresp_id9)[[4]][1,1],
                  summary(tresp_id9)[[4]][1,2])

plant10_coefs <- c(plant_ids[10], 
                  summary(tresp_id10)[[4]][2,1],
                  summary(tresp_id10)[[4]][2,2],
                  summary(tresp_id10)[[4]][3,1],
                  summary(tresp_id10)[[4]][3,2],
                  summary(tresp_id10)[[4]][1,1],
                  summary(tresp_id10)[[4]][1,2])

plant11_coefs <- c(plant_ids[11], 
                   summary(tresp_id11)[[4]][2,1],
                   summary(tresp_id11)[[4]][2,2],
                   summary(tresp_id11)[[4]][3,1],
                   summary(tresp_id11)[[4]][3,2],
                   summary(tresp_id11)[[4]][1,1],
                   summary(tresp_id11)[[4]][1,2])

plant12_coefs <- c(plant_ids[12], 
                   summary(tresp_id12)[[4]][2,1],
                   summary(tresp_id12)[[4]][2,2],
                   summary(tresp_id12)[[4]][3,1],
                   summary(tresp_id12)[[4]][3,2],
                   summary(tresp_id12)[[4]][1,1],
                   summary(tresp_id12)[[4]][1,2])

plant13_coefs <- c(plant_ids[13], 
                   summary(tresp_id13)[[4]][2,1],
                   summary(tresp_id13)[[4]][2,2],
                   summary(tresp_id13)[[4]][3,1],
                   summary(tresp_id13)[[4]][3,2],
                   summary(tresp_id13)[[4]][1,1],
                   summary(tresp_id13)[[4]][1,2])

plant14_coefs <- c(plant_ids[14], 
                   summary(tresp_id14)[[4]][2,1],
                   summary(tresp_id14)[[4]][2,2],
                   summary(tresp_id14)[[4]][3,1],
                   summary(tresp_id14)[[4]][3,2],
                   summary(tresp_id14)[[4]][1,1],
                   summary(tresp_id14)[[4]][1,2])

plant15_coefs <- c(plant_ids[15], 
                   summary(tresp_id15)[[4]][2,1],
                   summary(tresp_id15)[[4]][2,2],
                   summary(tresp_id15)[[4]][3,1],
                   summary(tresp_id15)[[4]][3,2],
                   summary(tresp_id15)[[4]][1,1],
                   summary(tresp_id15)[[4]][1,2])

plant16_coefs <- c(plant_ids[16], 
                   summary(tresp_id16)[[4]][2,1],
                   summary(tresp_id16)[[4]][2,2],
                   summary(tresp_id16)[[4]][3,1],
                   summary(tresp_id16)[[4]][3,2],
                   summary(tresp_id16)[[4]][1,1],
                   summary(tresp_id16)[[4]][1,2])

plant17_coefs <- c(plant_ids[17], 
                   summary(tresp_id17)[[4]][2,1],
                   summary(tresp_id17)[[4]][2,2],
                   summary(tresp_id17)[[4]][3,1],
                   summary(tresp_id17)[[4]][3,2],
                   summary(tresp_id17)[[4]][1,1],
                   summary(tresp_id17)[[4]][1,2])

plant18_coefs <- c(plant_ids[18], 
                   summary(tresp_id18)[[4]][2,1],
                   summary(tresp_id18)[[4]][2,2],
                   summary(tresp_id18)[[4]][3,1],
                   summary(tresp_id18)[[4]][3,2],
                   summary(tresp_id18)[[4]][1,1],
                   summary(tresp_id18)[[4]][1,2])

plant19_coefs <- c(plant_ids[19], 
                   summary(tresp_id19)[[4]][2,1],
                   summary(tresp_id19)[[4]][2,2],
                   summary(tresp_id19)[[4]][3,1],
                   summary(tresp_id19)[[4]][3,2],
                   summary(tresp_id19)[[4]][1,1],
                   summary(tresp_id19)[[4]][1,2])

plant20_coefs <- c(plant_ids[20], 
                   summary(tresp_id20)[[4]][2,1],
                   summary(tresp_id20)[[4]][2,2],
                   summary(tresp_id20)[[4]][3,1],
                   summary(tresp_id20)[[4]][3,2],
                   summary(tresp_id20)[[4]][1,1],
                   summary(tresp_id20)[[4]][1,2])

plant21_coefs <- c(plant_ids[21], 
                   summary(tresp_id21)[[4]][2,1],
                   summary(tresp_id21)[[4]][2,2],
                   summary(tresp_id21)[[4]][3,1],
                   summary(tresp_id21)[[4]][3,2],
                   summary(tresp_id21)[[4]][1,1],
                   summary(tresp_id21)[[4]][1,2])

plant23_coefs <- c(plant_ids[23], 
                   summary(tresp_id23)[[4]][2,1],
                   summary(tresp_id23)[[4]][2,2],
                   summary(tresp_id23)[[4]][3,1],
                   summary(tresp_id23)[[4]][3,2],
                   summary(tresp_id23)[[4]][1,1],
                   summary(tresp_id23)[[4]][1,2])

plant24_coefs <- c(plant_ids[24], 
                   summary(tresp_id24)[[4]][2,1],
                   summary(tresp_id24)[[4]][2,2],
                   summary(tresp_id24)[[4]][3,1],
                   summary(tresp_id24)[[4]][3,2],
                   summary(tresp_id24)[[4]][1,1],
                   summary(tresp_id24)[[4]][1,2])

plant25_coefs <- c(plant_ids[25], 
                   summary(tresp_id25)[[4]][2,1],
                   summary(tresp_id25)[[4]][2,2],
                   summary(tresp_id25)[[4]][3,1],
                   summary(tresp_id25)[[4]][3,2],
                   summary(tresp_id25)[[4]][1,1],
                   summary(tresp_id25)[[4]][1,2])

plant26_coefs <- c(plant_ids[26], 
                   summary(tresp_id26)[[4]][2,1],
                   summary(tresp_id26)[[4]][2,2],
                   summary(tresp_id26)[[4]][3,1],
                   summary(tresp_id26)[[4]][3,2],
                   summary(tresp_id26)[[4]][1,1],
                   summary(tresp_id26)[[4]][1,2])

plant27_coefs <- c(plant_ids[27], 
                   summary(tresp_id27)[[4]][2,1],
                   summary(tresp_id27)[[4]][2,2],
                   summary(tresp_id27)[[4]][3,1],
                   summary(tresp_id27)[[4]][3,2],
                   summary(tresp_id27)[[4]][1,1],
                   summary(tresp_id27)[[4]][1,2])

plant28_coefs <- c(plant_ids[28], 
                   summary(tresp_id28)[[4]][2,1],
                   summary(tresp_id28)[[4]][2,2],
                   summary(tresp_id28)[[4]][3,1],
                   summary(tresp_id28)[[4]][3,2],
                   summary(tresp_id28)[[4]][1,1],
                   summary(tresp_id28)[[4]][1,2])

plant29_coefs <- c(plant_ids[29], 
                   summary(tresp_id29)[[4]][2,1],
                   summary(tresp_id29)[[4]][2,2],
                   summary(tresp_id29)[[4]][3,1],
                   summary(tresp_id29)[[4]][3,2],
                   summary(tresp_id29)[[4]][1,1],
                   summary(tresp_id29)[[4]][1,2])

plant30_coefs <- c(plant_ids[30], 
                   summary(tresp_id30)[[4]][2,1],
                   summary(tresp_id30)[[4]][2,2],
                   summary(tresp_id30)[[4]][3,1],
                   summary(tresp_id30)[[4]][3,2],
                   summary(tresp_id30)[[4]][1,1],
                   summary(tresp_id30)[[4]][1,2])

plant31_coefs <- c(plant_ids[31], 
                   summary(tresp_id31)[[4]][2,1],
                   summary(tresp_id31)[[4]][2,2],
                   summary(tresp_id31)[[4]][3,1],
                   summary(tresp_id31)[[4]][3,2],
                   summary(tresp_id31)[[4]][1,1],
                   summary(tresp_id31)[[4]][1,2])

plant32_coefs <- c(plant_ids[32], 
                   summary(tresp_id32)[[4]][2,1],
                   summary(tresp_id32)[[4]][2,2],
                   summary(tresp_id32)[[4]][3,1],
                   summary(tresp_id32)[[4]][3,2],
                   summary(tresp_id32)[[4]][1,1],
                   summary(tresp_id32)[[4]][1,2])

plant33_coefs <- c(plant_ids[33], 
                   summary(tresp_id33)[[4]][2,1],
                   summary(tresp_id33)[[4]][2,2],
                   summary(tresp_id33)[[4]][3,1],
                   summary(tresp_id33)[[4]][3,2],
                   summary(tresp_id33)[[4]][1,1],
                   summary(tresp_id33)[[4]][1,2])

plant34_coefs <- c(plant_ids[34], 
                   summary(tresp_id34)[[4]][2,1],
                   summary(tresp_id34)[[4]][2,2],
                   summary(tresp_id34)[[4]][3,1],
                   summary(tresp_id34)[[4]][3,2],
                   summary(tresp_id34)[[4]][1,1],
                   summary(tresp_id34)[[4]][1,2])

plant35_coefs <- c(plant_ids[35], 
                   summary(tresp_id35)[[4]][2,1],
                   summary(tresp_id35)[[4]][2,2],
                   summary(tresp_id35)[[4]][3,1],
                   summary(tresp_id35)[[4]][3,2],
                   summary(tresp_id35)[[4]][1,1],
                   summary(tresp_id35)[[4]][1,2])

plant36_coefs <- c(plant_ids[36], 
                   summary(tresp_id36)[[4]][2,1],
                   summary(tresp_id36)[[4]][2,2],
                   summary(tresp_id36)[[4]][3,1],
                   summary(tresp_id36)[[4]][3,2],
                   summary(tresp_id36)[[4]][1,1],
                   summary(tresp_id36)[[4]][1,2])

plant37_coefs <- c(plant_ids[37], 
                   summary(tresp_id37)[[4]][2,1],
                   summary(tresp_id37)[[4]][2,2],
                   summary(tresp_id37)[[4]][3,1],
                   summary(tresp_id37)[[4]][3,2],
                   summary(tresp_id37)[[4]][1,1],
                   summary(tresp_id37)[[4]][1,2])

##### complete for all plants

#### combine into a  table, add rest below
coef_table <- rbind(plant2_coefs, plant3_coefs,
                    plant4_coefs, plant5_coefs,
                    plant6_coefs, plant7_coefs,
                    plant8_coefs, plant9_coefs,
                    plant10_coefs, plant11_coefs,
                    plant12_coefs, plant13_coefs,
                    plant14_coefs, plant15_coefs,
                    plant16_coefs, plant17_coefs,
                    plant18_coefs, plant19_coefs,
                    plant20_coefs, plant21_coefs,
                    plant23_coefs, plant24_coefs,
                    plant25_coefs, plant26_coefs,
                    plant27_coefs, plant28_coefs,
                    plant29_coefs, plant30_coefs,
                    plant31_coefs, plant32_coefs,
                    plant33_coefs, plant34_coefs,
                    plant35_coefs, plant36_coefs,
                    plant37_coefs)

colnames(coef_table) <- c('plantid', 'a', 'a_se', 'b', 'b_se', 'c', 'c_se')

### turn table into dataframe
class(coef_table)
coef_df <- as.data.frame(coef_table)

### check that numbers are numbers, etc
class(coef_df$a) # thinks it's a character, so turn these all into numbers
cols.num <- c('a', 'a_se', 'b', 'b_se', 'c', 'c_se')
coef_df[cols.num] <- sapply(coef_df[cols.num], as.numeric)

### add c3/c4 and block information to the tables
coef_df$species <- substr(coef_df$plantid, start = 1, stop= 3)
coef_df$photosynthetic_pathway <- NA
coef_df$photosynthetic_pathway[coef_df$species == 'ely' | coef_df$species == 'pas'] <- 'c3'
coef_df$photosynthetic_pathway[coef_df$species == 'sor' | coef_df$species == 'scz'] <- 'c4'

coef_df$block <- substr(coef_df$plantid, start = 5, stop = 5)

### now that we have the table, run analyses to test the hypotheses

#### hypothesis 1: temperature optimum is higher for c4 than c3
coef_df$topt <- (-coef_df$b) / (2*coef_df$a)
hist(coef_df$topt) # some very high values (check later?)
topt_lmer <- lmer(topt ~ photosynthetic_pathway + (1|species) + (1|block), data = coef_df)
plot(resid(topt_lmer) ~ fitted(topt_lmer)) # a bit greater spread for high topt values (check normality of data)
summary(topt_lmer) # c4 have higher topt than c3
Anova(topt_lmer) # and that difference is significant
emmeans(topt_lmer, ~photosynthetic_pathway)

#### hypothesis 2: breadth of curve (b) is higher for c4 than c3
hist(coef_df$b) 
b_lmer <- lmer(b ~ photosynthetic_pathway + (1|species) + (1|block), data = coef_df)
plot(resid(b_lmer) ~ fitted(b_lmer)) 
summary(b_lmer) 
Anova(b_lmer) 
emmeans(b_lmer, ~photosynthetic_pathway)

#### hypothesis 3: phi at temperature optimum is higher for c4 than c3
coef_df$phi_opt <- (coef_df$a * coef_df$topt^2) + (coef_df$b * coef_df$topt) + coef_df$c
hist(coef_df$phi_opt) 
phi_opt_lmer <- lmer(phi_opt ~ photosynthetic_pathway + (1|species) + (1|block), data = coef_df)
plot(resid(phi_opt_lmer) ~ fitted(phi_opt_lmer))
summary(phi_opt_lmer) 
Anova(phi_opt_lmer) 
emmeans(phi_opt_lmer, ~photosynthetic_pathway)


#### make models for (a) and (c) values

### (a) model
hist(coef_df$a) 
a_lmer <- lmer(a ~ photosynthetic_pathway + (1|species) + (1|block), data = coef_df)
plot(resid(a_lmer) ~ fitted(a_lmer)) 
summary(a_lmer) 
Anova(a_lmer) 
emmeans(a_lmer, ~photosynthetic_pathway)

### (c) model
hist(coef_df$c) 
c_lmer <- lmer(c ~ photosynthetic_pathway + (1|species) + (1|block), data = coef_df)
plot(resid(c_lmer) ~ fitted(c_lmer)) 
summary(c_lmer) 
Anova(c_lmer) 
emmeans(c_lmer, ~photosynthetic_pathway)

###extract emmeans data for a,b,c

summary(emmeans(a_lmer, ~photosynthetic_pathway))

summary(emmeans(b_lmer, ~photosynthetic_pathway))

summary(emmeans(c_lmer, ~photosynthetic_pathway))


c3_table <- c(summary(emmeans(a_lmer, ~photosynthetic_pathway))[2][1,1],
              summary(emmeans(a_lmer, ~photosynthetic_pathway))[3][1,1],
              summary(emmeans(b_lmer, ~photosynthetic_pathway))[2][1,1],
              summary(emmeans(b_lmer, ~photosynthetic_pathway))[3][1,1],
              summary(emmeans(c_lmer, ~photosynthetic_pathway))[2][1,1],
              summary(emmeans(c_lmer, ~photosynthetic_pathway))[3][1,1])

c4_table <- c(summary(emmeans(a_lmer, ~photosynthetic_pathway))[2][2,1],
              summary(emmeans(a_lmer, ~photosynthetic_pathway))[3][2,1],
              summary(emmeans(b_lmer, ~photosynthetic_pathway))[2][2,1],
              summary(emmeans(b_lmer, ~photosynthetic_pathway))[3][2,1],
              summary(emmeans(c_lmer, ~photosynthetic_pathway))[2][2,1],
              summary(emmeans(c_lmer, ~photosynthetic_pathway))[3][2,1])

photo_table <- rbind(c3_table, c4_table)
colnames(photo_table) <- c('a', 'a_SE', 'b', 'b_SE', 'c', 'c_SE')
       
####### now we will try and fit this data to an equation

###string of temperature values
temp_values <- seq(15, 50, 1)

###c3 equation
phi_c3 <- (photo_table[1,1] * temp_values^2) + (photo_table[1,3] * temp_values) + (photo_table[1,5])

###c4 equation
phi_c4 <- (photo_table[2,1] * temp_values^2) + (photo_table[2,3] * temp_values) + (photo_table[2,5])


### ggplots of (1) boxplots for 3 hypotheses, (2) average temperature responses

topt_boxplot <- ggplot(data = coef_df, 
                       aes(x = photosynthetic_pathway, y = topt, fill = photosynthetic_pathway)) +
  theme(legend.position = 'none',
        axis.title.x = element_text(size = rel(2.75)),
        axis.title.y = element_text(size = rel(2.75)),
        axis.text.x = element_text(size = rel(2.5)),
        axis.text.y = element_text(size = rel(2.5)),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = 'grey')) +
  geom_boxplot(color = 'black') +
  scale_fill_manual(values = c('#1887ab', '#d34467')) +
  xlab('Photosynthetic pathway') +
  ylab('Temperature optimum \u00b0C\n') +
  scale_x_discrete(labels = c(expression('C'[3]), expression('C'[4])))
 
phiopt_boxplot <- ggplot(data = coef_df, 
                       aes(x = photosynthetic_pathway, y = phi_opt, fill = photosynthetic_pathway)) +
  theme(legend.position = 'none',
        axis.title.x = element_text(size = rel(2.75)),
        axis.title.y = element_text(size = rel(2.75)),
        axis.text.x = element_text(size = rel(2.5)),
        axis.text.y = element_text(size = rel(2.5)),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = 'grey')) +
  geom_boxplot(color = 'black') +
  scale_fill_manual(values = c('#1887ab', '#d34467')) +
  xlab('Photosynthetic pathway') +
  ylab(expression('PhiPSII optimum (mol mol'^'-1' * ')')) +
  scale_x_discrete(labels = c(expression('C'[3]), expression('C'[4])))

b_boxplot <- ggplot(data = coef_df, 
                         aes(x = photosynthetic_pathway, y = -b, fill = photosynthetic_pathway)) +
  theme(legend.position = 'none',
        axis.title.x = element_text(size = rel(2.75)),
        axis.title.y = element_text(size = rel(2.75)),
        axis.text.x = element_text(size = rel(2.5)),
        axis.text.y = element_text(size = rel(2.5)),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = 'grey')) +
  geom_boxplot(color = 'black') +
  scale_fill_manual(values = c('#1887ab', '#d34467')) +
  xlab('Photosynthetic pathway') +
  ylab('Temperature breadth') +
  scale_x_discrete(labels = c(expression('C'[3]), expression('C'[4])))

tresp_dataframe <- as.data.frame(cbind(temp_values, phi_c3, phi_c4))

tresp_lineplot <- ggplot(data = tresp_dataframe, aes(x = temp_values, y = phi_c3)) +
  theme(legend.position = 'top', 
        axis.title.x = element_text(size = rel(2.75)),
        axis.title.y = element_text(size = rel(2.75)),
        axis.text.x = element_text(size = rel(2.25)),
        axis.text.y = element_text(size = rel(2.25)),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = 'grey'),) +
  geom_line(color = '#1887ab', linewidth = 3) +
  geom_line(aes(y = phi_c4), color = '#d34467', linewidth = 3) +
  xlab('Leaf temperature \u00b0C\n') +
  ylab(expression('PhiPSII (mol mol'^'-1' * ')'))

### print figures
tiff(filename = 'plots/topt_boxplot.tiff',
     width = 8, height = 8, units = 'in', res = 300)
plot(topt_boxplot)
dev.off()

tiff(filename = 'plots/phiopt_boxplot.tiff',
     width = 8, height = 8, units = 'in', res = 300)
plot(phiopt_boxplot)
dev.off()

tiff(filename = 'plots/b_boxplot.tiff',
     width = 8, height = 8, units = 'in', res = 300)
plot(b_boxplot)
dev.off()

tiff(filename = 'plots/tresp_lineplot.tiff',
     width = 8, height = 8, units = 'in', res = 300)
plot(tresp_lineplot)
dev.off()










###Starting over with species
coef_df

#### hypothesis 1: temperature optimum is higher for c4 species than c3 species
coef_df$topt <- (-coef_df$b) / (2*coef_df$a)
hist(coef_df$topt) 
s_topt_lmer <- lmer(topt ~ species + (1|block), data = coef_df)
plot(resid(s_topt_lmer) ~ fitted(s_topt_lmer)) 
summary(s_topt_lmer)
Anova(s_topt_lmer) 
emmeans(s_topt_lmer, ~species)

#### hypothesis 2: breadth of curve (b) is higher for c4 species than c3 species
hist(coef_df$b) 
s_b_lmer <- lmer(b ~ species + (1|block), data = coef_df)
plot(resid(s_b_lmer) ~ fitted(s_b_lmer)) 
summary(s_b_lmer) 
Anova(s_b_lmer) 
emmeans(s_b_lmer, ~species)

#### hypothesis 3: phi at temperature optimum is higher for c4 species than c3 species
coef_df$phi_opt <- (coef_df$a * coef_df$topt^2) + (coef_df$b * coef_df$topt) + coef_df$c
hist(coef_df$phi_opt) 
s_phi_opt_lmer <- lmer(phi_opt ~ species + (1|block), data = coef_df)
plot(resid(s_phi_opt_lmer) ~ fitted(s_phi_opt_lmer))
summary(s_phi_opt_lmer) 
Anova(s_phi_opt_lmer) 
emmeans(s_phi_opt_lmer, ~species)

#### make models for (a) and (c) values

### (a) model
hist(coef_df$a) 
s_a_lmer <- lmer(a ~ species + (1|block), data = coef_df)
plot(resid(s_a_lmer) ~ fitted(s_a_lmer)) 
summary(s_a_lmer) 
Anova(s_a_lmer) 
emmeans(s_a_lmer, ~species)

### (c) model
hist(coef_df$c) 
s_c_lmer <- lmer(c ~ species + (1|block), data = coef_df)
plot(resid(s_c_lmer) ~ fitted(s_c_lmer)) 
summary(s_c_lmer) 
Anova(s_c_lmer) 
emmeans(s_c_lmer, ~species)

###extract emmeans data for a,b,c

summary(emmeans(s_a_lmer, ~species))

summary(emmeans(s_b_lmer, ~species))

summary(emmeans(s_c_lmer, ~species))


###Box plots!
s_topt_boxplot <- ggplot(data = coef_df,
                         aes(x = species, y=topt,
                             fill = species)) + 
  theme(legend.position = 'none',
        axis.title.x = element_text(size = rel(2.75)),
        axis.title.y = element_text(size = rel(2.75)),
        axis.text.x = element_text(size = rel(2.5)),
        axis.text.y = element_text(size = rel(2.5)),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = 'grey')) +
  geom_boxplot(color = 'black') + 
  scale_fill_manual(values = c('#1887ab', "#87ab18", '#d34467', '#8518ab'))
  
s_b_boxplot <- ggplot(data = coef_df,
                      aes(x = species, y=-b,
                          fill = species)) + 
  theme(legend.position = 'none',
        axis.title.x = element_text(size = rel(2.75)),
        axis.title.y = element_text(size = rel(2.75)),
        axis.text.x = element_text(size = rel(2.5)),
        axis.text.y = element_text(size = rel(2.5)),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = 'grey')) +
  geom_boxplot(color = 'black') + 
  scale_fill_manual(values = c('#1887ab', "#87ab18", '#d34467', '#8518ab'))

s_phiopt_boxplot <- ggplot(data = coef_df,
                           aes(x = species, y=phi_opt,
                               fill = species)) + 
  theme(legend.position = 'none',
        axis.title.x = element_text(size = rel(2.75)),
        axis.title.y = element_text(size = rel(2.75)),
        axis.text.x = element_text(size = rel(2.5)),
        axis.text.y = element_text(size = rel(2.5)),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = 'grey')) +
  geom_boxplot(color = 'black') + 
  scale_fill_manual(values = c('#1887ab', "#87ab18", '#d34467', '#8518ab'))


###now for line graph...

ely_table <- c(summary(emmeans(s_a_lmer, ~species))[2][1,1],
                   summary(emmeans(s_b_lmer, ~species))[2][1,1],
                   summary(emmeans(s_c_lmer, ~species))[2][1,1])

pas_table <- c(summary(emmeans(s_a_lmer, ~species))[2][2,1],
                   summary(emmeans(s_b_lmer, ~species))[2][2,1],
                   summary(emmeans(s_c_lmer, ~species))[2][2,1])

scz_table <- c(summary(emmeans(s_a_lmer, ~species))[2][3,1],
                   summary(emmeans(s_b_lmer, ~species))[2][3,1],
                   summary(emmeans(s_c_lmer, ~species))[2][3,1])

sor_table <- c(summary(emmeans(s_a_lmer, ~species))[2][4,1],
               summary(emmeans(s_b_lmer, ~species))[2][4,1],
               summary(emmeans(s_c_lmer, ~species))[2][4,1])

species_table <- rbind(ely_table, pas_table, scz_table, sor_table)
colnames(species_table) <- c('a', 'b', 'c')

###same temp range, make equations..?

phi_ely <- (species_table[1,1] * temp_values^2) + (species_table[1,2] * temp_values) + (species_table[1,3])
phi_pas <- (species_table[2,1] * temp_values^2) + (species_table[2,2] * temp_values) + (species_table[2,3])
phi_scz <- (species_table[3,1] * temp_values^2) + (species_table[3,2] * temp_values) + (species_table[3,3])
phi_sor <- (species_table[4,1] * temp_values^2) + (species_table[4,2] * temp_values) + (species_table[4,3])

species_dataframe <- as.data.frame(cbind(temp_values, phi_ely, phi_pas, phi_scz, phi_sor))

###lineplot

species_lineplot <- ggplot(data = species_dataframe, aes(x = temp_values, y = phi_ely)) +
  theme(legend.position = 'top', 
        axis.title.x = element_text(size = rel(2.75)),
        axis.title.y = element_text(size = rel(2.75)),
        axis.text.x = element_text(size = rel(2.25)),
        axis.text.y = element_text(size = rel(2.25)),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = 'grey'),) +
  geom_line(color = '#1887ab', linewidth = 2) +
  geom_line(aes(y = phi_pas), color = '#87ab18', linewidth = 2) +
  geom_line(aes(y = phi_scz), color = '#d34467', linewidth = 2) +
  geom_line(aes(y = phi_sor), color = '#8518ab', linewidth = 2) +
  geom_line(aes(y = phi_c3), color = 'black', linewidth = 1, linetype = 'dashed') +
  geom_line(aes(y = phi_c4), color = 'black', linewidth = 1, linetype = 'dotted') +
  xlab('Leaf temperature \u00b0C\n') +
  ylab(expression('PhiPSII (mol mol'^'-1' * ')'))

### print figures
tiff(filename = 'plots/s_topt_boxplot.tiff',
     width = 8, height = 8, units = 'in', res = 300)
plot(s_topt_boxplot)
dev.off()

tiff(filename = 'plots/s_phiopt_boxplot.tiff',
     width = 8, height = 8, units = 'in', res = 300)
plot(s_phiopt_boxplot)
dev.off()

tiff(filename = 'plots/s_b_boxplot.tiff',
     width = 8, height = 8, units = 'in', res = 300)
plot(s_b_boxplot)
dev.off()

tiff(filename = 'plots/species_lineplot.tiff',
     width = 8, height = 8, units = 'in', res = 300)
plot(species_lineplot)
dev.off()
  
