"""
Assignments
EDA for billboard database, analyzes top 100 hits on billboard chart.
focusing on tidyr(tidyverse) functions to reshape the database structure
"""

library(tidyverse)
?billboard

bb <- billboard |>              #pivot_longer redoes the columns that starts with "wk" into rows
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week",     # namnger den nya kolumnen där de tidigare kolumnerna flyttas till 
    values_to = "rank",     # namnger den andra nya kolumnen där värdena från wk1, wk2 osv flyttas till
    values_drop_na = TRUE   # tar bort veckor med NA värde, vissa hits låg inte på top 100 i 72 veckor 
  ) |> 
  mutate(week = parse_number(week))     # parse_number tar första siffran i en string, det tar alltså bort wk från wk1 osv

view(bb)

bb |>     #plottar billboard låtarna.
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
    cols = bp1:bp2,              #slår ihop 2 kolumner, names_to döper om variabeln till measurements. värdena sparas i ny kolumn: value (values_to)
    names_to = "measurement",
    values_to = "value"
  )

who2

df <- who2 |> 
  pivot_longer(
    cols = !(country:year),                          #kolumner som INTE är country eller year, (är formaterade sp_f_014 med _ som separator) 
    names_to = c("diagnosis", "gender", "age"), 
    names_sep = "_",                                #separatorn
    values_to = "count"                             #flyttar värdena till en kolumn som skapas med namnet count 
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

cms_patient_experience |>    #  tar fram unika värden från measure_cd och measure_title
  distinct(measure_cd, measure_title)

cms_patient_experience |>     #slår ihop alla linjer med samma org-ID och ORG-name till en rad
  pivot_wider(
    id_cols = starts_with("org"),
    names_from = measure_cd,
    values_from = prf_rate
  )

df |> 
  distinct(diagnosis) |>    # tar fram unika värden för en pivot wider
  pull()

# övning DATA CLEANING
scores <- tibble::tribble(
  ~name,     ~math_2023, ~english_2023, ~math_2024, ~english_2024,
  "Anna",    78,          81,            82,          85,
  "Björn",   65,          70,            68,          72,
  "Cecilia", 90,          88,            93,          91
)

#Gör datan long så att du får:en kolumn för år,en kolumn för ämne,en kolumn för poäng
longer_scores <- scores |> 
  pivot_longer(
    cols = !name,
    names_to = c("subject", "year"),
    names_sep = "_",
    values_drop_na = TRUE
  )

#Övning 2 — pivot_wider, När du har gjort long-formatet:gör tillbaka till wide-format, men denna gång med år som kolumner och ämnen som rader
longer_scores |> 
  pivot_wider(
    names_from = year,
    values_from = value
  )

#  The syntax for linear regression using the function lm() is: model <- lm(formula, data, ...)
