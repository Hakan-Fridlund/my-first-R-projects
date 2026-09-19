#   Övning: filtrera, gruppera, summera data.  DPLYR flights Tibble (dataframe)

pacman::p_load(nycflights13)
library(nycflights13)
library(tidyverse)
?flights
view(flights) #bäst, men finns även glimpse() och print(flights, width = Inf)
class(flights)

flights |> filter(month %in% c(1, 2)) # filtrerar rader som har månad 1 eller 2
flights |> arrange(year, month, day, dep_time) # arrangerar lägsta år, sedan månad, dag och dep time. för INVERSE use ARRANGE(dsc(year))
flights |> distinct()    #filtrerar bort duplicates
flights |> distinct(origin, dest)    # find unique origin and destination PAIRS
flights |> count(origin, dest, sort = TRUE)   # räknar unique origin and destination PAIRS instead and sorts desc

#ÖVNING 1 Had an arrival delay of two or more hours  2  Flew to Houston (IAH or HOU) 3 Were operated by United, American, or Delta
#4 Departed in summer (July, August, and September) 5 Arrived more than two hours late but didn’t leave late 6 Were delayed by at least an hour, but made up over 30 minutes in flight

x <- flights |> filter(arr_delay >= 120)
x
y <- flights |> filter(dest %in% c("IAH", "HOU"))
y
z <- flights |> filter(carrier %in% c("UA", "AA", "DL"))
z
a <- flights |> filter(month == 7 | month == 8 | month == 9) # annat sätt att skriva, omständigare än %in%
a                       
b <- flights |> filter(arr_delay > 120 & dep_delay == 0) 
b
c <- flights |> filter(dep_delay >= 60 & (dep_delay - arr_delay > 30))
c

flights |>             # mutate lägger till kolumner som beräknas av existerande .before = index lägger dom först istället för sist
  mutate(                  # alternativ   .keep = "used"    sparar bara dom som använts och skapats
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )

flights |> select(!year:day)  # visar allt utom year till dag
flights |> select(where(is.character))  # visar alla komumner som är bokstäver
# hjälpfunktioner till SELECT()  starts_with(), ends_with(), contains() num:range() matches(används med regex)
# starts_with("abc"): matches names that begin with “abc”, ends_with("xyz"): matches names that end with “xyz”
# contains("ijk"): matches names that contain “ijk” num_range("x", 1:3): matches x1, x2 and x3.

flights |> select(tail_num = tailnum)  # döper om en variabel med select
flights |> rename(tail_num = tailnum)  # döper om en variable när man inte använder select
flights |> relocate(year:dep_time, .after = time_hour) #flyttar variable, om .after eller .before inte används flyttas det först

flights |>           # en pipe som filtrerar på dest, muterar fram speed, väljer variabler och arrangerar desc på speed
  filter(dest == "IAH") |> 
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed))

flights |> group_by(month)  #grupperar månader


flights |>                      # summarize medelvärdet av dep_delay   na.rm = TRUE ignorerar N/A värden (na remove)
  group_by(month) |>            # n = n() räknar antalet i varje grupp
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    n = n()
  )

flights |>              # tar en (n antal) slice av gruppen, max av försenade flyg, flyttar dest till första kolumn
  group_by(dest) |>             
  slice_max(arr_delay, n = 1) |>    # slice_head(n = 1) takes the first row from each group, slice_tail, slice_min, slice_sample(tar en random)
  relocate(dest)

flights |>       #  .by istället för group_by
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = c(origin, dest)
  )

#Which carrier has the worst average delays? Challenge: can you disentangle the effects of bad airports vs. bad carriers? 
#Why/why not? (Hint: think about flights |> group_by(carrier, dest) |> summarize(n()))

x <- flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n=n(),
    .by = c(carrier, dest)) |> 
  arrange (desc(delay)) |> 
  filter(n>50)               # tar bort dom PAIRS av destinationer/carriers som flugits färre än 50 ggr
x

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
