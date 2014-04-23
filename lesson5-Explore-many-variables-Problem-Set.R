
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(dplyr)
library(GGally)
library(reshape2) ## Package to transform data between wide and long formats
# ===========================================
# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.

# The plot should look something like this.
# http://i.imgur.com/b5xyrOu.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the histogram using
# scale_fill_brewer(type = 'qual')

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ===========================================
str(diamonds)
names(diamonds)

ggplot(aes(x = price), data = diamonds) +
  facet_wrap( ~ color) + 
  geom_histogram(aes(color = 'black', fill = cut)) + 
  scale_x_log10()

# ===========================================
# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

# The plot should look something like this.
# http://i.imgur.com/rQF9jQr.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the scatterplot using
# scale_color_brewer(type = 'qual')

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ===========================================
ggplot(aes(x = table, y = price), data = diamonds) +
  geom_point(aes(color = cut), binwidth = 2) + 
  scale_color_brewer(type = 'qual') +
  scale_x_continuous(lim = c(50,80), breaks = seq(0, 80, 2) ) 
 

# ===========================================
# What is the typical table range for the majority of diamonds
# of ideal cut?
by(diamonds$table, diamonds$cut, range)

# What is the typical table range for the majority of diamonds 
# of premium cut?
by(diamonds$table, diamonds$cut, range)


# ===========================================
# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.

# Note: Volume is a very rough approximation of
# a diamond's actual volume.

# The plot should look something like this.
# http://i.imgur.com/excUpea.jpg

# Note: In the link, a color palette of type
# 'div' was used to color the scatterplot using
# scale_color_brewer(type = 'div')

# This assignment is not graded and
# will be marked as correct when you submit.

# ENTER YOUR CODE BELOW THIS LINE
# ===========================================
diamonds$volume = diamonds$x * diamonds$y * diamonds$z
  
ggplot(aes(x = volume, y = price), data = subset(diamonds)) + 
  scale_y_log10() +
  scale_x_continuous(lim = c(40,500), breaks = seq(0, 500, 100) )+ 
  geom_point(aes(color = clarity) ) +
  scale_color_brewer(type = 'div')
