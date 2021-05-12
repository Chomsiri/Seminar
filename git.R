#section1 packages####
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("datasauRus")
library(datasauRus)
install.packages("gganimate")
library(gganimate)
install.packages("gifski")
library(gifski)
install.packages('png')
library(png)
install.packages("usethis")
library(usethis)
use_git_config(user.name = "Chomsiri", user.email = "Chomsiri.to@gmail.com")

#participant_data####

participants_data <- 
  read.csv(url("https://raw.githubusercontent.com/CWWhitney/teaching_R/master/participants_data.csv"))

participants_barplot <- table(participants_data$academic_parents)
barplot(participants_barplot)
?barplot

colnames(participants_data)

library(ggplot2)
ggplot(data = participants_data, 
       aes(x = letters_in_first_name, 
           y = days_to_email_response)) + 
  geom_point()

ggplot(data = participants_data, 
       aes(x = letters_in_first_name, 
           y = days_to_email_response, 
           color = academic_parents, 
           size = working_hours_per_day)) + 
  geom_point()

#practice ggplot datavisuallization####
# Change the barplot by creating a table of gender
participants_barplot <- table(participants_data$gender)
barplot(participants_barplot)

# Create a scatterplot of days to email response (y) 
# as a function of the letters in your first name (x) 
colnames(participants_data)

ggplot(data = participants_data,
aes(x = letters_in_first_name,
    y=days_to_email_response)) +
  geom_point()

# Create a scatterplot of days to email response (y) 
# as a function of the letters in your first name (x) 
# with colors representing binary data 
# related to academic parents (color) 
# and working hours per day as bubble sizes (size).

ggplot(data = participants_data, 
       aes(x = letters_in_first_name, 
           y = days_to_email_response, 
           color = gender, 
           size = working_hours_per_day)) + 
  geom_point()

#Iris data####
# Create a scatterplot of iris petal length (y) 
# as a function of sepal length (x) 
# with colors representing iris species (color) 
# and petal width as bubble sizes (size).
?iris
ggplot(data = iris, 
       aes(x = Sepal.Length, 
           y = Petal.Length, 
           color = Species, 
           size = Petal.Width))+ 
  geom_point()

colnames(iris)



#diamond data####

# Create a plot with the diamonds data 
# of the carat (x) and the price (y)

ggplot(data = diamonds, aes(x=carat, y= price,
                           alpha = 0.2))+
  geom_point()
ggplot(data = diamonds, aes(x = carat, 
                            y = price,
                            alpha=0.2,
                            color = color)) + 
  geom_point()

# Create a smaller diamonds data set (top 100 rows), 
# create a scatterplot with carat on the x-axis 
# and price on the y-xis and 
# with the color of the diamond as the color of the points. 

dsmall <- top_n(diamonds, n = 100)

ggplot(data = dsmall, aes(x = carat, 
                          y = price, 
                          color = color)) + 
  geom_point()

ggplot(data = diamonds, aes(x = carat, 
                          y = price, 
                          color = color)) + 
  geom_point()

# Create a smaller diamonds data set (top 40 rows), 
# create a scatterplot with carat on the x-axis 
# and price on the y-xis and 
# with the cut of the diamond as the shapes for the points. 

dsmall <- top_n(diamonds, n = 40)
ggplot(data = dsmall,
       aes(x=carat,
           y=price,
           shape=cut))+
  geom_point()

# ggplot2 smooth ####
# Create a smaller data set of diamonds with 50 rows.
dsmall <- top_n(diamonds, n = 50)
ggplot(data = dsmall, 
       aes(x = carat, 
           y = price))+
  geom_point()+
  geom_smooth()

# Create a smaller data set of diamonds with 50 rows. 
# Create a scatterplot and smoothed conditional 
# means overlay with carat on the x-axis 
# and price on the y-axis.
# Use 'glm' as the option for the smoothing
dsmall <- top_n(diamonds, 
n = 50)

ggplot(data = dsmall, 
       aes(x = carat, 
           y = price))+ 
  geom_point()+ 
  geom_smooth(method = 'glm')
  

  
# Create a plot with the diamonds data 
# of the log of carat (x) 
# and the log of price (y)

ggplot(data=diamonds,
       aes(x=log(carat),
           y=log(price),
           alpha=0.2))+
  geom_point()

# ggplot box####

ggplot(data = diamonds, 
       aes(x = color, 
           y = carat)) + 
  geom_boxplot()

# Create a boxplot where the x-axis is cut and
#  the y-axis is price divided by carat

ggplot(data = diamonds, 
       aes(x = cut, 
           y = price/carat)) + 
  geom_boxplot()


# ggplot parameter####
# Create a plot of the diamonds data 
# with carat on the x-axis, price on the y-axis. 
# Use the inhibit function to set the alpha to 0.1 
# and color to blue
ggplot(data = diamonds, 
       aes(x = carat, 
           y = price, 
           alpha = I(0.1), 
           color = I("blue"))) + 
  geom_point()

#ggplot jitter plot####

ggplot(data = diamonds, 
       aes(x = color, 
           y = carat)) + 
  geom_boxplot()+ 
  geom_jitter()
# Change the jittered boxplot so that the x-axis is cut and
#  the y-axis is price divided by carat
ggplot(data = diamonds, 
       aes(x = cut, 
           y = price/carat)) + 
  geom_boxplot()+ 
  geom_jitter()

# ggplot adding alpha ####
ggplot(data = diamonds, 
       aes(x = color, 
           y = price/carat, 
           alpha = I(0.1))) + 
  geom_boxplot()+ 
  geom_jitter()

# Change the alpha to 0.4 to make 
# the scatter less transparent
ggplot(data = diamonds, 
       aes(x = color, 
           y = price/carat, 
           alpha = I(0.4))) + 
  geom_boxplot()+ 
  geom_jitter()

# ggplot geom_histogram ####

ggplot(data = diamonds, 
       aes(x = carat)) +
  geom_density()
# Change the density plot so that the x-axis is carat 
# and the color is the diamond color

ggplot(data = diamonds, 
       aes(x = carat,
           color = color)) +
  geom_density()

colnames(diamonds)

ggplot(data = diamonds, 
       aes(x = price, 
           color = cut, 
           alpha = I(0.5))) +
  geom_density()

# Change the density plot so that the x-axis is carat 
# the color is the diamond color
# and the alpha is set to 0.3 using the inhibit function
ggplot(data = diamonds, 
       aes(x = carat, 
           color = color, 
           alpha = I(0.3))) +
  geom_density()

# ggplot subset ####

?mpg

ggplot(data = mpg, 
       aes(x = displ, 
           y = hwy,  
           color = class)) + 
  geom_point() +
  geom_smooth(method = "glm")
# Create a plot of the mpg data with 
# manufacturer as the color and a linear model 'lm'
# as the smooth method
ggplot(data = mpg, 
       aes(x = displ, 
           y = hwy,  
           color = manufacturer)) + 
  geom_point() +
  geom_smooth(method = "lm")

# ggplot not slow example ####


Change the title and labels as you see fit

ggplot(mtcars, 
       aes(mpg, 
           y = hp, 
           col = gear)) +
  geom_point() +
  ggtitle("The Title") +
  labs(x = "xxx", 
       y = "yyy", 
       col = "degree")
?mtcars
# Change the title and labels as you see fit  
ggplot(data = mtcars) +
  aes(x = mpg) +
  labs(x = "oilyyy") +
  aes(y = hp) +
  labs(y = "Horsee heeeyah") +
  geom_point() +
  aes(col = gear) +
  labs(col = "gearr") +
  labs(title = "My Cool Title")

# subset the data to numeric only with select_if
part_data <- select_if(participants_data, 
                       is.numeric)
# use 'cor' to perform pearson correlation
# use 'round' to reduce correlation 
# results to 1 decimal
cormat <- round(cor(part_data), 
                digits = 1)
# use 'as.data.frame.table' to build a table
# with correlation values
melted_cormat <- as.data.frame.table(cormat, 
                                     responseName = "value")
# plot the result with 'geom-tile'
ggplot(data = melted_cormat, 
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile()

#export file####

png(file = "cortile.png", width = 7, height = 6, units = "in", res = 300)

ggplot(data = melted_cormat, 
       aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

#gganimate####

colnames(iris)

P <- ggplot(iris,
           aes(x=Petal.Width,
            y=Petal.Length))+
  geom_point()

plot(P)

anim <- P + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)

anim
animate(plot = anim)
animate(plot = anim, renderer = gifski_renderer())
# gganimate and datasauRus ---- 
library(datasauRus)
library(gganimate)

p <- ggplot(datasaurus_dozen,
       aes(x = x, 
           y = y)) + 
  geom_point() + 
  theme_minimal() + 
  transition_states(dataset, 3, 1) + 
  ease_aes('cubic-in-out')   

anim <- p + transition_states(dataset, 3, 1) + 
  ease_aes('cubic-in-out') 

animate(plot = anim, renderer = gifski_renderer())


ggplot(data = dsmall,
       aes(x = carat,
           y = price,
           cplor = color)) + 
  geom_line() + 
  transition_reveal(carat) + 
  ease_aes("linear") + 
  labs(title = 'Diamond carat: {frame_along}')



