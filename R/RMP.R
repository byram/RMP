
# Packages ----------------------------------------------------------------

library(data.table)
library(tidyverse)


# Import ------------------------------------------------------------------

dt_data <- fread("data/ratemyprofessors.csv", 
                 data.table = TRUE, stringsAsFactors = TRUE) %>% 
  rename_all(tolower) %>% 
  mutate(index = as.integer(row.names(.)))


# Exploratory -------------------------------------------------------------

glimpse(dt_data)
unique(dt_data$college)
cor(dt_data$`overall quality`, 
    jitter(as.numeric(dt_data$hot), factor = 0.01), 
    use = "pairwise.complete.obs")

library(ggplot2)

dt_data %>% 
  ggplot(aes(x = hot, y = `overall quality`, fill = hot)) + 
  geom_violin() + 
#  facet_wrap(.~college) + 
  labs(title = "Hot vs Not Hot", subtitle = "RateMyProfessor Ratings") + 
#  annotate("text", x = 1, 
#           y = mean(dt_data[dt_data$hot == "Hot", "overall quality"], na.rm = TRUE), 
#           label = paste("avg =", round(mean(dt_data[dt_data$hot == "Hot", "overall quality"], na.rm = TRUE), digits = 1))) + 
#  annotate("text", x = 2, 
#           y = mean(dt_data[dt_data$hot == "Not Hot", "overall quality"], na.rm = TRUE), 
#           label = paste("avg =", round(mean(dt_data[dt_data$hot == "Not Hot", "overall quality"], na.rm = TRUE), digits = 1))) + 
  ggthemes::theme_fivethirtyeight() + 
  theme(legend.position = "none")

mean(dt_data[dt_data$hot == "Hot", ]$`overall quality`, na.rm = TRUE)

dt_data %>% 
  mutate(`total ratings` = pmin(`total ratings`, 15L)) %>% 
  ggplot(aes(x = `total ratings`, y = `overall quality`)) + 
  geom_point(position = position_jitter(), color = "dodgerblue") + 
  labs(title = "Overall Rating vs # of Ratings", 
       x = "# of Ratings", y = "Overall Rating", 
       caption = "# of ratings capped at 15") + 
  ggthemes::theme_fivethirtyeight()
