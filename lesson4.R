#Assignments
#EDA for billboard database, analyzes top 100 hits on billboard chart.
#focusing on tidyr(tidyverse) functions to reshape the database structure


library(tidyverse)
?billboard

bb <- billboard |>              #pivot_longer redoes the columns that starts with "wk" into rows
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week",     # names the new column where the earlier columns are moved to 
    values_to = "rank",     # names the second new column where values from wk1, wk2 et.c. are moved to
    values_drop_na = TRUE   # remove weeks with NA value, since some hits was not on top 100 for 72 full weeks 
  ) |> 
  mutate(week = parse_number(week))     # parse_number takes first number in a string, (removes wk from wk1 -> 1)

view(bb)

bb |>     #plot billboard songs.
  ggplot(aes(x = week, y = rank, group = track)) + 
  geom_line(alpha = 0.25) + 
  scale_y_reverse()


df <- tribble(
  ~id,  ~bp1, ~bp2,
  "A",  100,  120,
  "B",  140,  115,
  "C",  120,  125
)

df |> 
  pivot_longer(
    cols = bp1:bp2,              #adds 2 columns together, names_to renames the variable to measurements. the values is saved in a new column: value (values_to)
    names_to = "measurement",
    values_to = "value"
  )

who2

df <- who2 |> 
  pivot_longer(
    cols = !(country:year),                          #columns that is NOT country or year, (is formatted sp_f_014 with _ as a separator) 
    names_to = c("diagnosis", "gender", "age"), 
    names_sep = "_",                                #the separator
    values_to = "count"                             #moves the values to a column that is created with the name count 
  )

household
household |> 
  pivot_longer(
    cols = !family, 
    names_to = c(".value", "child"),  # splits the column names into two components: the first part determines the output column name (x or y), and the second part determines the value of the num column.
    names_sep = "_", 
    values_drop_na = TRUE
  )

# pivot wider
cms_patient_experience

cms_patient_experience |>    #  takes unique values from measure_cd and measure_title
  distinct(measure_cd, measure_title)

cms_patient_experience |>     #adds all lines with same org-ID and ORG-name together to one row
  pivot_wider(
    id_cols = starts_with("org"),
    names_from = measure_cd,
    values_from = prf_rate
  )

df |> 
  distinct(diagnosis) |>    # takes unique values for a pivot wider
  pull()

# ASSIGNMENT 
# DATA CLEANING

scores <- tibble::tribble(
  ~name,     ~math_2023, ~english_2023, ~math_2024, ~english_2024,
  "Anna",    78,          81,            82,          85,
  "Björn",   65,          70,            68,          72,
  "Cecilia", 90,          88,            93,          91
)

#Make the data long so you get: one column for year, one for subject and one for score
longer_scores <- scores |> 
  pivot_longer(
    cols = !name,
    names_to = c("subject", "year"),
    names_sep = "_",
    values_drop_na = TRUE
  )

#Assignment 2 — pivot_wider, When made long, remake it into wide-format, this time with year as columns and subject as rows
longer_scores |> 
  pivot_wider(
    names_from = year,
    values_from = value
  )

#  The syntax for linear regression using the function lm() is: model <- lm(formula, data, ...)
