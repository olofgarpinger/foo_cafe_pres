###### R-Code - R for Data Science. By Olof Rännbäck-Garpinger, 2018-05-03, Knightec AB ######


### Load the Tidyverse packages ###
library(tidyverse)


### Data frames with tibble ###

# Print original R data frame
iris

# Print tibble data frame
as.tibble(iris)


### Reading rectangular data using readr ###

# Load data from vasaloppet 2016 into a tibble, specifying
# the start group variable as an ordered factor
vl_2016 = read_csv("vl_2016_mod.csv", 
                    col_types = cols(start_group = col_factor(ordered = T, levels = 0:10)
                                     ))

# View the data frame
View(vl_2016)


### Data manipulation with dplyr ###

# select: pick columns with skiier id, age class, start group, and finish time in Mora 
vl_2016_selected = select(vl_2016, skiier_id, age_class, start_group, mora)

# filter: pick skiiers only in age class 70 
vl_2016_filtered = filter(vl_2016_selected, age_class == 70)

# arrange: sort rows after columns: start group 1st and finish time in Mora 2nd
vl_2016_arrange = arrange(vl_2016_filtered, start_group, mora)

# mutate: Transform time in Mora from seconds to hours
vl_2016_mutate = mutate(vl_2016_arrange, mora_hours = mora / 3600)

# group_by and summarise: group after start group, summarise 
# mora_hours average and number of skiiers after group
vl_2016_group_by = group_by(vl_2016_mutate, start_group)
vl_2016_summarise = summarise(vl_2016_group_by, 
                              mean_mora_hours = mean(mora_hours), 
                              count = n())


### The pipe %>%. Ctrl + Shift + M ###

vl_2016 %>% 
  select(skiier_id, age_class, start_group, mora) %>% 
  filter(age_class == 70) %>% 
  arrange(start_group, mora) %>% 
  mutate(mora_hours = mora / 3600) %>% 
  group_by(start_group) %>% 
  summarise(mean_mora_hours = mean(mora_hours), 
            count = n())


### Visualizations with ggplot2 ###

# Geometric objects - Scatter plot
vl_2016_ac70 = vl_2016 %>% filter(age_class == 70) 
ggplot(data = vl_2016_ac70) + 
  geom_point(mapping = aes(x = smagan, y = mora), 
             size = 2) +
  labs(x = "Time to Smågan (seconds)",
       y = "Time to Mora (seconds)")

# The same plot with alternative code
vl_2016 %>% 
  filter(age_class == 70) %>% 
  ggplot(aes(x = smagan, y = mora)) + 
  geom_point(size = 2) +
  labs(x = "Time to Smågan (seconds)",
       y = "Time to Mora (seconds)")

# Geometric objects - Aesthetic mappings
# Adding colors depending on start group
vl_2016_ac70 %>%
  ggplot(aes(x = smagan, y = mora)) + 
  geom_point(aes(color = start_group), 
             size = 2) +
  labs(x = "Time to Smågan (seconds)",
       y = "Time to Mora (seconds)")

# The same plot with Bengt annotated
# install.packages("ggrepel") needs to be run first!
bengt = vl_2016 %>% 
  filter(skiier_id == 4687) %>% 
  mutate(name = "Bengt")
p = vl_2016_ac70 %>%
  ggplot(aes(x = smagan, y = mora)) + 
  geom_point(aes(color = start_group), 
             size = 2) +
  labs(x = "Time to Smågan (seconds)",
       y = "Time to Mora (seconds)")
p + 
  ggrepel::geom_label_repel(   # ggrepel needs to be installed: install.packages("ggrepel")
    aes(label = name),
    data = bengt,
    point.padding = 0.3
    )

# Geometric objects - Smoothing
vl_2016_ac70 %>% 
  ggplot(aes(x = smagan, y = mora)) + 
  geom_point(aes(color = start_group), 
             size = 2) +
  geom_smooth() +
  labs(x = "Time to Smågan (seconds)",
       y = "Time to Mora (seconds)")

# Smoothing without start group 5
vl_wo_sg5 = vl_2016_ac70 %>% 
  filter(start_group != 5)
q = vl_2016_ac70 %>% 
  ggplot(aes(x = smagan, y = mora)) + 
  geom_point(aes(color = start_group), 
             size = 2) +
  geom_smooth(data = vl_wo_sg5) +
  labs(x = "Time to Smågan (seconds)",
       y = "Time to Mora (seconds)")

# With Bengt annotated
q +
  ggrepel::geom_label_repel(   # ggrepel needs to be installed: install.packages("ggrepel")
    aes(label = name),
    data = bengt,
    point.padding = 0.3
    )

# Geometric objects - Box plot
vl_2016 %>% 
  ggplot(aes(x = age_class, y = mora)) +
  geom_boxplot() +
  labs(x = "Age class",
       y = "Time to Mora (seconds)")

# Geometric objects - Histogram
vl_2016 %>% 
  ggplot(aes(x = mora)) +
  geom_histogram(binwidth = 900) +
  labs(x = "Time to Mora (seconds)",
       y = "Skiiers within 15 minute intervals")

# Geometric objects - Frequency polygons
vl_2016 %>% 
  ggplot(aes(x = mora)) +
  geom_freqpoly(aes(col = start_group), 
                binwidth = 900, 
                lwd = 1.5) +
  labs(x = "Time to Mora (seconds)",
       y = "Skiiers within 15 minute intervals") +
  geom_vline(xintercept = 32328, 
             lty = 2, 
             lwd = 1.5, 
             color = "black")

# Facets
vl_2016 %>% 
  ggplot(aes(x = mora)) +
  geom_histogram(binwidth = 900) +
  facet_wrap( ~ start_group) +
  labs(x = "Time to Mora (seconds)",
       y = "Skiiers within 15 minute intervals")

