#     GGPLOT
library(tidyverse)
library(palmerpenguins)
library(ggthemes)
penguins
glimpse(penguins)
view(penguins)
?penguins

ggplot(  # version 1 har color = species globalt
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +   # skapar punkt scatterplot
  geom_smooth(method = "lm")  # skapar en smooth linje med metoden linear model


ggplot(    # version 2 har color = species lokalt i scatterpointen
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +   #sätter olika former för varje species punkt
  geom_smooth(method = "lm") +
  labs(                                           
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

# Make a scatterplot of bill_depth_mm vs. bill_length_mm. That is, make a scatterplot with bill_depth_mm on the y-axis and bill_length_mm on the x-axis. Describe the relationship between these two variables.

ggplot(
  data = penguins,
  mapping = aes(x =bill_length_mm, y = bill_depth_mm)
) +
  geom_point(mapping = aes(color = species, shape = species, na.rm = TRUE)) +
  scale_color_colorblind()

# What happens if you make a scatterplot of species vs. bill_depth_mm? What might be a better choice of geom? svar boxplot
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

#bättre syntax för plotting med pipes och data först, sedan mapping via aes
penguins |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

ggplot(penguins, aes(x = fct_infreq(species))) +    # bar chart om du bara har t.ex. species. fct_infreq sorterar fallande
  geom_bar()

ggplot(penguins, aes(x = body_mass_g)) +     # histogram
  geom_histogram(binwidth = 200)

ggplot(penguins, aes(x = body_mass_g)) +    # density plot fungerar som ett histogram fast linje
  geom_density()


#Make a bar plot of species of penguins, where you assign species to the y aesthetic. How is this plot different? sideways
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

penguins |>    # fylld density plot, alpha sätter genomskinligheten
  ggplot(aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)

ggplot(penguins, aes(x = island, fill = species)) +   # visar antalet species på varje ö som fill
  geom_bar()   #lägg till detta: (position = "fill") för att få barsen alla till 100 och visa % istället,  använd labs(y = "proportion") för att döpa om y-axeln

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +  # facet_wrap skapar en subplot för varje ö (kategori)
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