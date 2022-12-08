#Penguins assignment R script for Michaelmas 2022

#Please set the working directory to the source file location using the 'session' button in RStudio
library(palmerpenguins)
library(ggplot2)
library(janitor)
library(dplyr)
library(ragg)

head(penguins_raw) #raw penguins data
write.csv(penguins_raw, paste0("data_raw/penguins_raw.csv")) #saving data as a csv

data.clean <- function(penguins_raw){ #Function for cleaning data
  penguins_raw %>%
    select(-starts_with("delta")) %>%
    select(-Comments) %>%
    clean_names()
}
#This function has been saved to the 'functions' folder in the working directory
penguins_clean <- data.clean(penguins_raw) #Create clean data
head(penguins_clean)
write.csv(penguins_clean, paste0("data_clean/penguins_clean.csv"))

##Question 4 - run a statistical test on the dataset and produce a figure to explain it
#I'll do a linear regression comparing flipper length with culmen (bill) length in gentoos.
#First I need to filter out gentoo data, then plot the graph and perform the analysis.

gentoo <- filter(penguins_clean, species == 'Gentoo penguin (Pygoscelis papua)')
gentoo #Filtered dataset

gentoo_plot <- ggplot(data = gentoo,
                      aes(x = flipper_length_mm, y = culmen_length_mm,)) +
  labs(y = "Culmen length (mm)", x = "Flipper length (mm)", title = "Flipper and culmen length in gentoo penguins") +
  geom_point(colour = 'blue') +
  geom_smooth(method='lm') +
  theme_bw()
gentoo_plot

gentoo_mod <- lm(formula = culmen_length_mm ~ flipper_length_mm, data = gentoo) #Linear regression
summary(gentoo_mod)

#Saving figure in the 'figures' folder
agg_png("figures/fig01_15x15.png", 
        width = 20, height = 15, units = "cm", res = 600, scaling = 1.4)
gentoo_plot
dev.off()


