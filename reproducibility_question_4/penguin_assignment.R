#Penguins assignment R script for Michaelmas 2022

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

##Question 2. Create a scatter plot of culmen length vs depth across the penguins
{bill_plot <- ggplot(data = penguins_clean,
       aes(x = culmen_length_mm,
           y = culmen_depth_mm)) +
  geom_point() +
  geom_smooth(method='lm')
bill_plot
#This is a poor graph. All the penguin species have been lumped together with no differential identifiers
 #between species and sex. There is no title, making it not apparent what the graph is about. The axis labels
 #are taken straight from the data so use only lower case and underscores instead of spaces, which is
 #unintuitive and unpleasant to read.
}

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


