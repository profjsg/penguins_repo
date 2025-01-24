library(tidyverse)
library(palmerpenguins??)

penguins_raw <- penguins_raw
glimpse(penguins_raw)

#select columns for analysis
select(penguins_raw, Species, Island, `Individual ID`)
penguins_select <- select(penguins_raw, Species, Island, `Individual ID`)

#create a new object from the raw data with a subset of vars
penguins_select <- penguins_raw |>
  select(Species, Island, `Flipper Length (mm)`, `Individual ID`)

#create the same object but using the tidyverse
penguins_select <- penguins_raw %>%
  select(Species, Island, `Flipper Length (mm)`, `Individual ID`)

#select all of the columns from varA to varX
penguins_raw %>%
  select(Species: `Individual ID`)

#remove Species
penguins_raw |>
  select(-Species)

#select columns: Species, and all variables with "length"
penguins_raw |>
  select(Species, matches("Length"))

#reorder columns with Species first and everything else to follow
penguins_raw %>%
  select(Species, everything())

penguins_raw_reduced <- penguins_raw %>%
  select(Species, matches("Length"))
glimpse(penguins_raw_reduced)

select(penguins_raw, Species, Island, `Individual ID`)

#renaming columns
penguins_raw %>%
  rename(id = `Individual ID`, flipper_length = `Flipper Length (mm)`)

penguins_raw %>%
  select(Species, `Individual ID`, matches ("Length")) %>%
  rename(id = `Individual ID`, flipper_length = `Flipper Length (mm)`)

#creating vars
penguins_raw %>%
  select(Species, matches("Length")) %>%
  rename(flipper_length = `Flipper Length (mm)`) %>%
  mutate(flipper_length_cm = flipper_length *0.1)
  culmen_length_cm = `Culmen length (mm)` *0.1)

penguins_raw %>%
  select(Species, matches("Length")) %>%
  rename(flipper_length = `Flipper Length (mm)`, culmen_length_cm = `Culmen Length (mm)`) %>%
  mutate(flipper_culmen_ratio = flipper_length/culmen_length_cm)

#Filter Rows
penguins_raw %>%
  filter(Species == "Adelie Penguin (Pygoscelis adeliae)")

penguins_raw %>%
  filter(Species != "Adelie Penguin (Pygoscelis adeliae)")

penguins_raw %>%
  filter(Island %in% c("Torgerson", "Biscoe"))

penguins_raw %>%
  filter(Island == "Torgerson" & Sex == "Male")

penguins_raw %>%
  filter(`Flipper Length (mm)` >= 200)

penguins_raw %>%
  filter(`Flipper Length (mm)` <= 200)

penguins_raw |> 
  select(Species, `Body Mass (g)`, matches("Length")) |> 
  rename(flipper_length = `Flipper Length (mm)`) |>
  mutate(flipper_cm = flipper_length * 0.1, 
         length_cm_mass_ratio = flipper_cm/`Body Mass (g)`)

penguins_raw %>%
  filter(`Clutch Completion` == "Yes")

penguins_raw %>%
  filter(`Clutch Completion` == "Yes" & Island == "Torgersen")

##Ordering rows
penguins_mass <- penguins_raw %>%
  arrange(`Body Mass (g)`)
  filter(Island == "Biscoe") %>%
    arrange(`Body Mass (g)`)
  
penguins_mass <- penguins_raw %>%
    filter(Island == "Biscoe") %>%
    arrange(`Body Mass (g)`)

#reverse order the rows
penguins_mass <- penguins_raw %>%
  filter(Island == "Biscoe") %>%
  arrange(-`Body Mass (g)`)

penguins_mass <- penguins_raw %>%
  filter(Island == "Biscoe") %>%
  arrange(Sex, `Body Mass (g)`)

#Summarizing Data

penguins_raw %>%
  summarize(mean_flipper_length = mean(`Flipper Length (mm)`),
            mean_body_mass = mean(`Body Mass (g)`))


penguins_raw %>%
  summarize(mean_flipper_length = mean(`Flipper Length (mm)`, na.rm = T),
            mean_body_mass = mean(`Body Mass (g)`, na.rm = T))


penguins_raw %>%
  filter(`Clutch Completion` == "Yes") %>%
  summarize(max_flipper_length = max(`Flipper Length (mm)`, na.rm = T),
            min_flipper_length = min(`Flipper Length (mm), na.rm = T),
            .by = Sex) %>%
  filter(is.na(Sex == F))
#this doesn't work and I don't know why

#recoding variables

penguins_raw |>
  count(Species)

chinstrap <- penguins_raw |>
  mutate(chinstrap = if_else(Species == "Chinstrap penguin (Pygoscelis antarctica)", "Chinstrap", "Other")) %>%
  count(chinstrap)

penguins_raw |>
  mutate(species = case_when(
    Species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    Species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
    Species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
    .default = "Other"
    )) |>
  count(species)

library(knitr)
library(kableExtra)

penguins_ratio_data <- penguins_raw |>
  select(Species, `Body Mass (g)`, matches("Length")) |>
  rename(flipper_length = `Flipper Length (mm)`) |>
  mutate(length_mass_ration = flipper_length/`Body Mass (g)`) |>
  arrange(length_mass_ration)

write_csv(penguins_ratio_data, file = "data/penguins_ratio_data.csv")  

library(janitor)
















