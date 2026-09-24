# Assignment: filter, group, summerize data.  DPLYR flights Tibble (dataframe)
# trying out the nycflights database


pacman::p_load(nycflights13)
library(nycflights13)
library(tidyverse)

#exploring the dataframe in several ways
?flights
view(flights) # a couple of useful commands for viewing the datatable
glimpse(flights)
print(flights, width = Inf)
class(flights)

flights |> filter(month %in% c(1, 2)) # filter rows that has month 1 or 2
flights |> arrange(year, month, day, dep_time) # arranges lowest year, then month, day and dep time. for INVERSE use ARRANGE(dsc(year))
flights |> distinct()    #filter away duplicates
flights |> distinct(origin, dest)    # find unique origin and destination PAIRS
flights |> count(origin, dest, sort = TRUE)   #counts unique origin and destination PAIRS instead and sorts descending


# Assignment. Filter: 1 Had an arrival delay of two or more hours  
# 2  Flew to Houston (IAH or HOU) 
# 3 Were operated by United, American, or Delta
# 4 Departed in summer (July, August, and September) 
# 5 Arrived more than two hours late but didn’t leave late 
# 6 Were delayed by at least an hour, but made up over 30 minutes in flight


x <- flights |> filter(arr_delay >= 120)
x
y <- flights |> filter(dest %in% c("IAH", "HOU"))
y
z <- flights |> filter(carrier %in% c("UA", "AA", "DL"))
z
a <- flights |> filter(month == 7 | month == 8 | month == 9) # another way to write, more complicated than %in%
a                       
b <- flights |> filter(arr_delay > 120 & dep_delay == 0) 
b
c <- flights |> filter(dep_delay >= 60 & (dep_delay - arr_delay > 30))
c

flights |>             # mutate adds columns that is calculated of existing .before = index adds them first instead of last
  mutate(                  # alternative   .keep = "used"  saves only the used and created
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )

flights |> select(!year:day)  # shows all except year til day
flights |> select(where(is.character))  # shows all columns that is letters
# helpfunctions to SELECT()  starts_with(), ends_with(), contains() num:range() matches(used with regex)
# starts_with("abc"): matches names that begin with “abc”, ends_with("xyz"): matches names that end with “xyz”
# contains("ijk"): matches names that contain “ijk” num_range("x", 1:3): matches x1, x2 and x3.

flights |> select(tail_num = tailnum)  # renames a variable with select
flights |> rename(tail_num = tailnum)  # renames a variable not using select
flights |> relocate(year:dep_time, .after = time_hour) #moves a variable, if .after or .before is not used it will be located first

flights |>           # a pipe that filters on dest, mutates speed, chooses variables and arranges desc on speed
  filter(dest == "IAH") |> 
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed))

flights |> group_by(month)  #groups by month


flights |>                      # summarize mean value of dep_delay  na.rm = TRUE ignores N/A values (na remove)
  group_by(month) |>            # n = n() counts the number in each group
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    n = n()
  )

flights |>              # takes a (n number) slice of the group, max delayed flights, moves dest to first column
  group_by(dest) |>             
  slice_max(arr_delay, n = 1) |>    # slice_head(n = 1) takes the first row from each group, slice_tail, slice_min, slice_sample(takes one random)
  relocate(dest)

flights |>       #  .by instead of group_by
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = c(origin, dest)
  )

# ASSIGNMENT:
# Which carrier has the worst average delays? Challenge: can you disentangle the effects of bad airports vs. bad carriers? 
# Why/why not? (Hint: think about flights |> group_by(carrier, dest) |> summarize(n()))


x <- flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n=n(),
    .by = c(carrier, dest)) |> 
  arrange (desc(delay)) |> 
  filter(n>50)               # removes the PAIRS of destinations/carriers that was flown less than 50 times
x

