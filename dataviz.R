library(ggplot2)

# Take a look at our example dataset  
View(diamonds)

# Make a chart from scratch
x = ggplot() +
  layer(
    data = diamonds, mapping = aes(x=carat,y=price),
    stat='identity', position="identity", geom="point"
  ) +
  scale_x_continuous() +
  scale_y_continuous() +
  coord_cartesian()
x

# Show only diamonds with at least 4 carat
min4carat = diamonds[diamonds$carat >= 4,]
ggplot() +
  layer(
    data = min4carat, mapping = aes(x=carat,y=price),
    stat='identity', position="identity", geom="point"
  ) +
  scale_x_continuous() +
  scale_y_continuous() +
  coord_cartesian()

# TODO: Convert this into a jitter plot

# TODO: Is this a power curve?
# add 1. y <- ..., and x and y log scale
x + scale_x_log10() + scale_y_log10()


# Get rid of layer definitintions
ggplot(data = diamonds, mapping = aes(x=carat,y=price)) +
  geom_point()

ggplot(data = diamonds, mapping = aes(x=carat,y=price)) +
  geom_smooth()

# Add up 2 layers
ggplot(data = diamonds, mapping = aes(x=carat,y=price)) +
  geom_point() + geom_smooth()

#### 

# Switch to qplot 
qplot(carat,price,data=diamonds,geom=c('point','smooth'))

# set axis log='xy'
qplot(carat,price,data=diamonds,geom=c('point','smooth'),log='xy')

# Let's change the theme to bw
qplot(carat,price,data=diamonds,geom=c('point','smooth'),log='xy') +
  theme_bw()


# A FEW EXTRA FEATURES

# Strong defaults
qplot(price, data=diamonds)
qplot(price, data=diamonds, binwidth=100)

# Histogram by cut 
qplot(cut,data=diamonds)

# Histogram as barchart
ggplot(diamonds,aes(x="",fill=cut)) + geom_bar(width=1)

# Pie Chart
ggplot(diamonds,aes(x="",fill=cut)) + geom_bar(width=1) 
+ coord_polar(theta='y')

# Faceting
qplot(carat,price,data=diamonds,geom=c('point','smooth'),
      log='xy') + facet_grid(. ~ cut)

# Colors
qplot(carat,price,data=diamonds,geom='point', 
      color=cut, 
      size=clarity)

# Sizes
qplot(carat,price,data=diamonds[diamonds$carat >= 3,],geom='point', 
      color=clarity, 
      size=cut)

# Show point, jitter, boxplot, violin plot
qplot(x=cut, y=price, data=diamonds, geom='point')


# Exercises 
# Visualize these and write a sentence
# about what the chart tells yo about the data
###########

# - Create a histogram of "carat"

# - Set the bin width of the histogram to 0.01

# Make a scatterplot: carat vs price, set the color to clarity

# Make a scatterplot: carat vs price, set the color to clarity. Also add trendline to the plot

# Make a scatterplot: carat vs price, 
# Facet it by clarity.

# - Show carat vs cut, make a jitter, a violin and a boxplot. 
# What can you see?
