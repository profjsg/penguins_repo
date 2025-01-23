library(tidyverse)
chinstrap_data <- read_csv("data/chinstrap_data.csv")
adelie_data <- read_csv("data/adelie_data.csv")
penguin_data <- bind_rows(adelie_data, chinstrap_data)
gentoo_data <- read_csv("data/gentoo_data.csv")
penguin_data <- bind_rows(penguin_data, gentoo_data)
table(penguin_data$Species)
write_csv(penguin_data,"data/penguin_data.csv")

