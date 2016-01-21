
# ****************
# Qualitative Data
# ****************

# Frequency Distribution of Qualitative Data
# ------------------------------------------

# Problem - Find the frequency distribution of the painter schools in the data set painters (MASS)

library(MASS)
head(painters)
table(painters$School)

# Exercise - Find programmatically the school that has the most painters.

t <- data.frame(table(painters$School))
head(t[order(t$Freq, decreasing = TRUE),],1)

# Relative Frequency Distribution of Qualitative Data
# ---------------------------------------------------

# Problem - Find the relative frequency distribution of the painter schools in the data set painters

cbind(round(table(painters$School) / nrow(painters),2))

# Bar Graph
# ---------

# Problem - Find the bar graph of the painter schools in the data set painters. 

barplot(table(painters$School), col='red', density=75, main="Painters Schools")

# Pie Chart
# ---------

# Problem - Find the  pie graph of the painter schools in the data set painters

pie(table(painters$School), main="Painters Schools")

# Category Statistics
# -------------------

# Problem - Find out the mean composition score of school C in the data set painters

mean(painters[painters$School == 'C', ]$Composition) # result = 13.16667

# Exercise - Find programmatically the school with the highest composition scores.

sort(tapply(painters$Composition,painters$School, mean), decreasing = TRUE)[1]

# Exercise - Find the percentage of painters whose color score is equal to or above 14.

nrow(painters[painters$Colour >= 14, ]) / nrow(painters) # result = 0.3703704
