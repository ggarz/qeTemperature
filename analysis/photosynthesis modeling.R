#### Attempting to run phipsii values through photosynthesis models

## libraries
# install.packages('R.utils')
library(R.utils)
library(ggplot2)
library(colorRamps)
library(RColorBrewer)

# Load in data frame with values
garry_data <- read.csv("coefdataframe.csv")
head(garry_data$a) #looks like it is reading it OK
