
data.clean <- function(penguins_raw){ #Function for cleaning data
  penguins_raw %>%
    select(-starts_with("delta")) %>%
    select(-Comments) %>%
    clean_names()
}