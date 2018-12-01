library(tidyverse)
library(scales)
library(extrafont)


bridge_raw <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-11-27/baltimore_bridges.csv") 

#DataExplorer::create_report(bridge_raw)

bridge_raw$bridge_condition <- factor(bridge_raw$bridge_condition, 
                                      levels = c("Poor", "Fair", "Good"))
bridge_raw <- bridge_raw %>% 
  mutate(ID = row_number()) #add UniqueID for later comparison
  
#Filter only Poor and Fair bridges, arrange by Condition and then by daily traffic
bridge_processed <- bridge_raw %>% 
  filter(bridge_condition != "Good") %>%
  arrange(bridge_condition, -avg_daily_traffic) %>% 
  mutate(cumulative = cumsum(avg_daily_traffic)) 

adt_pareto_val <- 0.8 * sum(bridge_processed$avg_daily_traffic)

bridges_to_repair <- bridge_processed %>% 
  filter(cumulative <= adt_pareto_val)

number_of_bridges <- count(bridges_to_repair)
percentage_of_bridges <- round((number_of_bridges / count(bridge_processed))*100, 0)
percentage_of_bridges <- paste0(percentage_of_bridges,"%", collapse = "")

title <- paste('Pareto to Maryland: "Fix these', number_of_bridges, 'first"')
subtitle <- paste(percentage_of_bridges, "of bridges rated Poor or Fair condition carry\n80% of Poor and Fair combined traffic.")

bridges_to_repair %>% 
  ggplot(aes(x = long, y = lat, alpha = .5)) + geom_point(colour = "maroon") + 
  borders("county", "Maryland") +
  theme_void() +
  labs(title = title, 
       subtitle = subtitle,
       caption = "Notes:Lots of caveats :)\nData: Balitmore Sun Data Desk\nPlot:@oscar_b123") +
  theme(legend.position = "none", 
        plot.margin = unit(c(5,5,5,5), "mm"),
        text = element_text(family = "Merriweather"))


