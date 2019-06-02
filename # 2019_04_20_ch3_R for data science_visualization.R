# 2019_04_20_ch3_R for data science_visualization 
# basic is the best strategic
# reference: https://r4ds.had.co.nz/introduction.html

library(tidyverse)
tidyverse_update()

head(mpg,3)
ggplot(data=mpg) + geom_point(mapping = aes(x=displ, y = hwy, color = class, size = class))

# The mapping argument is always paired with aes(), and the x and y
# arguments of aes() specify which variables to map to the x and y axes



# An aesthetic is a visual property of the objects in your plot.


''' basic strcuture of ggplot 
ggplot(data = <DATA>) + 
  <GEOM_FUNCTION>(
    mapping = aes(<MAPPINGS>),
    stat = <STAT>, 
    position = <POSITION>
  ) +
  <COORDINATE_FUNCTION> +
  <FACET_FUNCTION>
'''




# alpha, shape

ggplot(data=mpg) + geom_point(mapping = aes(x=displ, y = hwy, alpha= class))
ggplot(data=mpg) + geom_point(mapping = aes(x=displ, y = hwy, shape= class))

                            
# One way to add additional variables is with aesthetics. Another way, particularly useful for categorical variables, 
# is to split your plot into facets, subplots that each display one subset of the data.

ggplot(data=mpg) + geom_point(mapping = aes(x=displ, y = hwy)) + facet_wrap( ~ class, nrow=2)

colnames(mpg)                                                   

# two facet, discrete variables drv, cyl
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

# A geom is the geometrical object that a plot uses to represent data. People often
# describe plots by the type of geom that the plot uses.


# geom_point
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# geom_smooth
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

head(mpg,3) # drv ={4,f,r}

# additng linetype
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype =drv))


# additng linetype
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, color =drv))

# different graph on the sample graph

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) +
  geom_smooth(data = filter(mpg, class =="2seater"), se = FALSE)

# 3.7 Statistical transformations

ggplot(data = diamonds) + geom_bar(mapping = aes(x=cut))


# generating data 
demo <- tribble(
  ~cut, ~ freq,
  "Fair", 1610,
  "good", 2000,
  "very good", 20100,
  "premium", 12000,
  "ideal", 21000)

# count bar-chart
ggplot(data = demo) + geom_bar(mapping = aes( x=cut, y = freq), stat = "identity")

# proposting bar-chart
head(diamonds,1)
ggplot(data= diamonds) + geom_bar(mapping = aes(x= cut, y= ..prop.., group =1))

# sata_summary()

ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x=cut, y=depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median)

# 3.8 Position adjustments    

# fill

ggplot(data = diamonds) + geom_bar(mapping = aes(x=cut, colour =cut))
ggplot(data = diamonds) + geom_bar(mapping = aes(x=cut, fill =cut)) # fill the color in the box

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

# position = "fill" works like stacking, but makes each set of stacked bars the same height. This makes it easier to compare 
# proportions across groups.

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

# position = "dodge" places overlapping objects directly beside one another. This makes it easier to compare individual values.

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

# 3.9 Coordinate systems

# coord_flip() switches the x and y axes.

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) 
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot() + coord_flip()

# map data : coord_quickmap() sets the aspect ratio correctly for maps. 

nz <- map_data("nz")
head(nz)
summary(nz$group)

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# coord_polar() uses polar coordinates. Polar coordinates reveal an interesting connection between a bar chart and a Coxcomb 

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar

bar + coord_flip()
bar + coord_polar()


