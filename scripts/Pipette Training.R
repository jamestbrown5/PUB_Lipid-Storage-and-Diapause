##Load Libraries
library(readxl)
library(ggplot2)

##Read in the data

library(readxl)
data=X20161215_Pipette_Training <- read_excel("~/GitHub/ECB_mass_measures/data/raw/validation/Data/20161215_Pipette Training.xlsx")



##Plot data as a bar graph
ggplot(data, aes(x=volume_ul, y=mass_mg)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)