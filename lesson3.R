#GGPLOT assignment, penguins database


library(tidyverse)
library(palmerpenguins)
library(ggthemes)
penguins
glimpse(penguins)
view(penguins)
?penguins

ggplot(  # version 1 has color = species global
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +   # creates point scatterplot
  geom_smooth(method = "lm")  # creates a smooth line with linear model (regression line)


ggplot(    # version 2 has color = species locally in scatterpoint
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +   #sets different shapes for each species point
  geom_smooth(method = "lm") +
  labs(                                           
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()


# Make a scatterplot of bill_depth_mm vs. bill_length_mm. That is, make a scatterplot with bill_depth_mm on the y-axis and bill_length_mm on the x-axis. 
# Describe the relationship between these two variables.


ggplot(
  data = penguins,
  mapping = aes(x =bill_length_mm, y = bill_depth_mm)
) +
  geom_point(mapping = aes(color = species, shape = species, na.rm = TRUE)) +
  scale_color_colorblind()


# What happens if you make a scatterplot of species vs. bill_depth_mm? What might be a better choice of geom?: boxplot

ggplot(
  data = penguins,
  mapping = aes(x =species, y = bill_depth_mm)
) +
  geom_boxplot(mapping = aes(color = species)) +
  labs(
    caption = "Data come from the palmerpenguins package.",
    title = "Penguin Beak depths"
  ) +
  scale_color_colorblind() 


# better syntax för plotting with pipes and data first, then mapping with aes

penguins |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

ggplot(penguins, aes(x = fct_infreq(species))) +    # bar chart if you only have f.ex. species. fct_infreq sorts falling
  geom_bar()

ggplot(penguins, aes(x = body_mass_g)) +     # histogram
  geom_histogram(binwidth = 200)

ggplot(penguins, aes(x = body_mass_g)) +    # density plot (works like a histogram but line
  geom_density()



# Make a bar plot of species of penguins, where you assign species to the y aesthetic. How is this plot different? sideways

penguins |> 
  ggplot(aes(y = species)) +
  geom_bar()


# Make a histogram of the carat variable in the diamonds dataset that is available when you load the tidyverse package. 
# Experiment with different binwidths. What binwidth reveals the most interesting patterns?

?diamonds
view(diamonds)
diamonds |> 
  ggplot(aes(x = carat)) +
  geom_histogram(binwidth = 0.3)

penguins |>    # filled density plot, alpha sets transparancy
  ggplot(aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)

ggplot(penguins, aes(x = island, fill = species)) +   # shows the number of species on each island as fill
  geom_bar()   #add this: (position = "fill") to get the bars to 100 and show % instead, use: labs(y = "proportion") to rename the y-axle

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +  # facet_wrap creates a subplot for each island (category)
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)


# Make a scatterplot of bill_depth_mm vs. bill_length_mm and color the points by species. 
# What does adding coloring by species reveal about the relationship between these two variables? What about faceting by species?

penguins |> 
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm, color = species)) +
  geom_point() +
  facet_wrap(~species)

ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm, 
    color = species, shape = species
  )
) +
  geom_point()
ggsave(filename = "penguin-plot.png")

?ggsave
