library(ggplot2)
library(ggthemes)
library(gridExtra)
library(dplyr)
library(plyr)
library(alr3)

library(mgcv)

## Here, we will continue working with the "Diamonds" data set.
str(diamonds)
##############################################################################
##############################################################################
## scatterplot of price vs x.
##############################################################################
ggplot(aes(x = x, y = price), 
       data = diamonds) +
  geom_point()
## Observations:
# There are some outliers and an exponential relationship between "price" and "x"

## Sacado del foro:
ggplot(aes(x=x**6, y=price), 
       data = subset(diamonds, color == 'D' & cut == "Ideal" & clarity == "IF")) + 
  geom_point() + 
  geom_smooth(method = 'lm', color = 'red')

##############################################################################
## Analyze correlations
##############################################################################
## What is the correlation between "price" and "x"?
cor.test(diamonds$price, diamonds$x, method = 'pearson')

## What is the correlation between "price" and "y"?
cor.test(diamonds$price, diamonds$y, method = 'pearson')

## What is the correlation between "price" and "z"?
cor.test(diamonds$price, diamonds$z, method = 'pearson')

##############################################################################
## Create a simple scatter plot of price vs depth.
##############################################################################
ggplot(aes(x = price, y = depth), 
       data = diamonds) +
  geom_point()

# Change the code to make the transparency of the
# points to be 1/100 of what they are now and mark
# the x-axis every 2 units. See the instructor notes
# for two hints.
ggplot(aes(x = depth, y = price), 
       data = diamonds) +
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks = seq(50, 75, 2) )

##############################################################################
## Correlations of depth vs price
##############################################################################
cor.test(diamonds$depth, diamonds$price, method = 'pearson')

## Based on the correlation coefficient would you use depth to predict the price
## of a diamond?
# No. Because the correlation factor is very small, so we can say that the variables
# are not realted at all.

##############################################################################
## Create a scatterplot of price vs carat
## and omit the top 1% of price and carat
## values.
##############################################################################
ggplot(aes(x = carat, y = price), 
       data = subset(diamonds, price > (price*0.1), carat > (carat*0.1)) ) +
  geom_point()

ggplot(aes(x = carat, y = price), 
       data = diamonds ) +
  geom_point()

##############################################################################
## Create a scatterplot of price vs. volume (x * y * z).
##############################################################################
# Create a new variable for volume in the diamonds data frame.
diamonds$volume = diamonds$x * diamonds$y * diamonds$z

ggplot(aes(x = volume, y = price),
       data = diamonds) +
  geom_point()

##############################################################################
## Cuantos diamantes tienen volumen cero?
##############################################################################
summary(diamonds$volume == 0)

#Otra forma
count(diamonds$volume == 0)


##############################################################################
## Correlations of price vs volume
##############################################################################
## Exclude diamonds that have a volume of 0 or that are greater than or equal to 800
volume_filter = subset(diamonds, volume > 0 & volume <= 800 )
cor.test(volume_filter$price, volume_filter$volume, method = 'pearson')
#o sino:
cor(volume_filter$price, volume_filter$volume, method = 'pearson')

##############################################################################
## Subset the data to exclude diamonds with a volume
## greater than or equal to 800. Also, exclude diamonds
## with a volume of 0. Adjust the transparency of the
## points and add a linear model to the plot (price vs volume).
##############################################################################
diamonds_filter = subset(diamonds, volume > 0 & volume < 800 )

ggplot(aes(x = volume, y = price), 
       data = diamonds_filter) + 
  geom_point(alpha = 1/10) + 
  geom_smooth()

ggplot(aes(x = volume, y = price), 
       data = diamonds_filter) + 
  geom_point(alpha = 1/10) + 
  stat_smooth(method = "lm", size = 1)

# Linear model smooth:
ggplot(aes(x = volume, y = price), 
       data = diamonds_filter) + 
  geom_point(alpha = 1/10) + 
  stat_smooth(method = "lm", formula = y ~ x, size = 1)
#stat_smooth = geom_smooth

# Locally weighted regression
ggplot(aes(x = volume, y = price), 
       data = diamonds_filter) + 
  geom_point(alpha = 1/10) + 
  stat_smooth(method = "loess", formula = y ~ x, size = 1)

# second order (quadratic) polynomial
ggplot(aes(x = volume, y = price), 
       data = diamonds_filter) + 
  geom_point(alpha = 1/10) + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)

# We now fit a GAM (use pacjage mgvc) adding a penalized smoother with x
ggplot(aes(x = volume, y = price), 
       data = diamonds_filter) + 
  geom_point(alpha = 1/10) + 
  stat_smooth(method = "gam", formula = y ~ s(x, k=100), size = 1)

# We now fit a GAM (use pacjage mgvc) adding a penalized smoother with x
ggplot(aes(x = volume, y = price), 
       data = diamonds_filter) + 
  geom_point(alpha = 1/10) + 
  stat_smooth(method = "gam", formula = y ~ s(x), size = 1)


# Smooths can also be fit separately by levels of another variable. 
# This allows a sort of examination of 'interactions' in the data.
ggplot(aes(x = volume, y = price, colour = factor(carat)), 
       data = diamonds_filter) + 
  geom_point(alpha = 1/10) + 
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE)

# if we wanted the points coloured, but not separate lines there are two
# options---force stat_smooth() to have one group
ggplot(aes(x = volume, y = price, colour = factor(carat)), 
       data = diamonds_filter) + 
  geom_point(alpha = 1/10) + 
  stat_smooth(aes(group = 1), method = "lm", formula = y ~ x, se = FALSE)

# or only add colour to the points, not in the global ggplot() call
ggplot(aes(x = volume, y = price), 
       data = diamonds_filter) + 
  geom_point(alpha = 1/10, aes(colour = factor(carat)) ) + 
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE)


##############################################################################
# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity
##############################################################################
# Name the data frame diamondsByClarity
# The data frame should contain the following
# variables in this order.

#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n

# where n is the number of diamonds in each
# level of clarity.
detach("package:plyr", unload=TRUE) # el paquete dplyr da conflicto con el paquete plyr

clarity_groups = group_by(diamonds, clarity)
diamondsByClarity = summarise(clarity_groups,
                                mean_price = mean(price),
                                median_price = median(price),
                                min_price = min(price),
                                max_price = max(price),
                                n = n() )

head(diamondsByClarity, 2)

##############################################################################
# write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.
##############################################################################
# We've created summary data frames with the mean price
# by clarity and color. You can run the code in R to
# verify what data is in the variables diamonds_mp_by_clarity
# and diamonds_mp_by_color.
diamonds_by_clarity = group_by(diamonds, clarity)
diamonds_mp_by_clarity = summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color = group_by(diamonds, color)
diamonds_mp_by_color = summarise(diamonds_by_color, mean_price = mean(price))


#==========================================================#
# Diferentes tipos de modelos con smooth:
# ---------------------------------------
# http://www.ats.ucla.edu/stat/r/faq/smooths.htm








