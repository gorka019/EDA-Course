 ######################################################
# Udacity Course: EDA - Exploratory Data Analysis
# Lesson 3: Explore One Variable - Problem Set 3
######################################################
library(ggplot2)
library(ggthemes)
library(gridExtra)

str(diamonds)
#===========================================================#
## Histograma del precio de los diamantes:
qplot(x = price, data = diamonds,
      color = I('black'), fill = I('#099DD9') )
## Como puede apreciarse, el histograma tiene una cola demasiado larga.

## Para ver las medidas Median y Mean
summary(diamonds$price)

#===========================================================#
## How many diamonds cost less than 500$?
summary(diamonds$price < 500)
    # > summary(diamonds$price < 500)
    # Mode   FALSE    TRUE    NA's 
    # logical   52211    1729       0 

## How many diamonds cost less than 250$?
summary(diamonds$price < 250)
    # > summary(diamonds$price < 250)
    # Mode   FALSE    NA's 
    # logical   53940       0

## How many diamonds cost 15000$ or more$?
summary(diamonds$price >= 15000)

#===========================================================#
# Explore the largest peak in the
# price histogram you created earlier.

# Try limiting the x-axis, altering the bin width,
# and setting different breaks on the x-axis.

# There won't be a solution video for this
# question so go to the discussions to
# share your thoughts and discover
# what other people find.

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Submit your final code when you are ready.

# TYPE YOUR CODE BELOW THE LINE
# ======================================================================
qplot(x = price, data = diamonds,
      binwidth = 50,
      color = I('black'), fill = I('#099DD9') ) +
  scale_x_continuous(lim = c(300,2500), breaks = seq(300, 2500, 100) )

## Save the plot image in the workspace
ggsave('diamonds-price.png')


# ======================================================
# Break out the histogram of diamond prices by cut.

# You should have five histograms in separate
# panels on your resulting plot.

# TYPE YOUR CODE BELOW THE LINE
# ======================================================
qplot(x = price, data = diamonds, 
      binwidth = 30) +
  scale_x_continuous(lim = c(300,2500), breaks = seq(300, 2500, 100) ) + 
  facet_wrap(~cut, ncol=2) 

# ======================================================
## Check the previous histograms: diamonds price depending on the cut.
# ======================================================
## Which cut has the highest priced diamond?
#  Ideal
sort(by(diamonds$price, diamonds$cut, max), decreasing=TRUE)
by(diamonds$price, diamonds$cut, max, decreasing=TRUE)

qplot(x = price, data = diamonds, 
      binwidth = 50, geom = 'freqpoly', color = cut) +  
  scale_x_continuous(lim = c(20000, 22000), breaks = seq(20000, 22000, 1000) ) 

## Which cut has the lowest priced diamond?
sort(by(diamonds$price, diamonds$cut, min), decreasing=FALSE)
by(diamonds$price, diamonds$cut, min, decreasing=FALSE)

## Which cut has the lowest median price?
sort(by(diamonds$price, diamonds$cut, median), decreasing=FALSE)
by(diamonds$price, diamonds$cut, median, decreasing=FALSE)


# ===============================================================
# In the two last exercises, we looked at
# the distribution for diamonds by cut.

# Run the code below in R Studio to generate
# the histogram as a reminder. (Solamente para recordar el histograma sacado arriba)
# ===============================================================

qplot(x = price, data = diamonds, 
      color = I('black'), fill = I('#099DD9') ) + 
  facet_wrap(~cut)

# ===============================================================

# In the last, exercise we looked at the summary statistics
# for diamond price by cut. If we look at the output table, the
# the median and quartiles are reasonably close to each other.

# diamonds$cut: Fair
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     337    2050    3282    4359    5206   18570 
# ------------------------------------------------------------------------ 
# diamonds$cut: Good
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     327    1145    3050    3929    5028   18790 
# ------------------------------------------------------------------------ 
# diamonds$cut: Very Good
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     336     912    2648    3982    5373   18820 
# ------------------------------------------------------------------------ 
# diamonds$cut: Premium
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     326    1046    3185    4584    6296   18820 
# ------------------------------------------------------------------------ 
# diamonds$cut: Ideal
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     326     878    1810    3458    4678   18810 

# This means the distributions should be somewhat similar,
# but the histograms we created don't show that.

# The 'Fair' and 'Good' diamonds appear to have 
# different distributions compared to the better
# cut diamonds. They seem somewhat uniform
# on the left with long tails on the right.

# Let's look in to this more.

# Look up the documentation for facet_wrap in R Studio.
# Then, scroll back up and add a parameter to facet_wrap so that
# the y-axis in the histograms is not fixed. You want the y-axis to
# be different for each histogram.

# If you want a hint, check out the Instructor Notes.

qplot(x = price, data = diamonds, 
      color = I('black'), fill = I('#099DD9') ) + 
  facet_wrap(~cut, scales = 'free_y')


# ===========================================================================
# Create a histogram of price per carat
# and facet it by cut. You can make adjustments
# to the code from the previous exercise to get
# started.

# Adjust the bin width and transform the scale
# of the x-axis using log10.

# Submit your final code when you are ready.

# ENTER YOUR CODE BELOW THIS LINE.
# =========================================================================== 
qplot(x = price/carat, data = diamonds,
      color = I('black'), fill = I('#099DD9') ) + 
  facet_wrap(~cut, scales = 'free_y') + 
  scale_x_log10()

# =================================================================
# Investigate the price of diamonds using box plots,
# numerical summaries, and one of the following categorical
# variables: cut, clarity, or color.

# There won't be a solution video for this
# exercise so go to the discussion thread for either
# BOXPLOTS BY CLARITY, BOXPLOT BY COLOR, or BOXPLOTS BY CUT
# to share you thoughts and to
# see what other people found.

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.
# =================================================================
## For boxplots, we use y parameter for the continuous data and the x 
## parameter for the categorical data.

qplot(x = cut, y = price, data = diamonds,
      binwidth = 50,
      geom = 'boxplot',
      xlab = 'Diamond Cut',
      fill = color) +
  scale_y_continuous( breaks = seq(0,20000,2500)) +
  coord_cartesian(ylim = c(0,15000))
# The above code present a series of box plots grouped by cut and colored by color. 
# You'll notice each groups' price has a nice positive slope related to the color.


# ===================================================================
# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.

# Go to the discussions to
# share your thoughts and to discover
# what other people found.

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.

# SUBMIT YOUR CODE BELOW THIS LINE
# ===================================================================
qplot(x = color, y = price/carat, data = diamonds,
      binwidth = 50,
      geom = 'boxplot',
      xlab = 'Diamond Cut',
      fill = color) +
  scale_y_continuous( breaks = seq(0,20000,2500)) +
  coord_cartesian(ylim = c(0,15000))

# ===================================================================
# Investigate the weight of the diamonds (carat) using a frequency polygon.
# Use different bin widths to see how the frequency polygon changes. 
# What carat size has a count greater than 2000?
# ===================================================================
qplot(x = carat, data = diamonds, 
      binwidth = 1, 
      geom = 'freqpoly')

# ====================================================================================
# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to download a data set of your choice
# and create 2-5 plots that make use of the techniques from Lesson 3.

# You might use a simple histogram, a boxplot split over a categorical variable,
# or a frequency polygon. The choice is yours!

# You can find a link to the Gapminder website in the Instructor Notes.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. any questions you answered, your observations, and summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots

# You can save images by using the ggsave() command.
# ggsave() will save the last plot created.
# For example...
#                  qplot(x = price, data = diamonds)
#                  ggsave('priceHistogram.png')

# ggsave currently recognises the extensions eps/ps, tex (pictex),
# pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Copy and paste all of the code that you used for
# your investigation, and submit it when you are ready.
# ====================================================================================
getwd()
setwd('C:/Users/Gorka/Dropbox/Proyecto_MSI/WORKSPACE/')
list.files()

wcc = read.csv("indicator colon and rectum female deaths.csv", sep = ',', header=TRUE)

# library(ggplot2)
# library(gridExtra)
# 
# wcc <- read.csv("womencoloncancerdeaths.csv", header=TRUE)
# summary(wcc)
# sapply(wcc, class)
# levels(wcc$Country)
# 
# # We need to reshape this data to give us multiple observations in rows.
# # Reshape to be Country, year (text), Deathper1000
# wcc_reshape <- reshape(wcc, idvar="Country", ids="Country", 
#                        times =names(wcc[,c(-1)]), timevar = "Year",
#                        varying = list(names(wcc[,c(-1)])),
#                        direction = "long"
# )
# names(wcc_reshape)[3] <- "Deathper1000"
# wcc_reshape$Year <- factor(wcc_reshape$Year)
# wcc_reshape <- wcc_reshape[order(wcc_reshape$Year, wcc_reshape$Country),]
# 
# 
# qhistplot <- qplot(data=wcc_reshape, x=Deathper1000, color=Country, fill=Country,
#                    xlab = "Death per 1000",
#                    ylab = "Count of Occurences of Death rate") +
#   labs(title = "Frequency of Death Per 1000 Women by Country (Years 1979-2002)")
# 
# ggsave("CDR_Hist.png")
# 
# qlineplot <- qplot(data=wcc_reshape, y=Deathper1000, color=Country,
#                    xlab = "Year",
#                    ylab = "Death Per 1000") +
#   geom_line() +
#   labs(title = "Death Per 1000 Women (Years 1979-2002)") +
#   scale_x_continuous(breaks=c(0,25,50,75,100), labels=c("1979","1985","1991","1997", "2002"))
# ggsave("CDR_Line.png")
# 
# qboxplot <- qplot(data=wcc_reshape, x=Country, y=Deathper1000, 
#                   ylab = "Death Per 1000",
#                   geom='boxplot') + 
#   labs(title = "Death Per 1000 Women (Years 1979-2002)")
# ggsave("CDR_Box.png")
# 
# grid.arrange(qhistplot, qlineplot, qboxplot, ncol=1)



#==================================================================
# REFERENCIAS:
#==================================================================
## Facet_wrap:
## -----------  
## http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/#free-scales
##
## Data Sets:
## ----------
## http://www.gapminder.org/data/
##
## Importar Fechas en R:
## ---------------------
## http://www.r-bloggers.com/date-formats-in-r/
