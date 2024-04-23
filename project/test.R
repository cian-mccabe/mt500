install.packages("tidyverse")
library("tidyverse")
install.packages("ggplot2")
library(ggplot2)

unicef_indicator_22 <- read_csv("unicef_indicator_22.csv")

ggplot(unicef_indicator_22) +
  aes(x = time_period, y = time_period +
  geom_point() +
  
  ukraine2_data <- unicef_indicator_22 %>% +
  filter (country = ukraine)
    
 install.packages("dplyr")
library(dplyr)

ggplot(ukraine_data, aes(x = time_period, y = indicator)) +
  geom_point() +  # Create a scatter plot
  labs(
    title = "Indicator for Ukraine Over Time",
    x = "Time Period",
    y = "Indicator Value"
  ) +
  theme_minimal()

print(ukraine_data)

install.packages("gapminder")
library(gapminder)

ggplot(data = unicef_indicator_22)
ggplot(data = gapminder)
ggplot(data = unicef_indicator_22) +
  aes(x = time_period, y = obs_value, color = Continent) +
  geom_line() +
  theme_classic() +
labs( x = "Time Period",y = "Net Migration Rate Value", title = "Evolution of Net Migration rate per continent.") 
  
  geom_line(linetype = "dashed") +  # Customize line appearance
  geom_point(size = .1, shape = 19) +  # Add points with a specific shape and size
  labs(
    title = "Trend of Net Migration Rates Over Time",
    subtitle = "A comparison across continents")
  



theme_minimal() +  # Use a clean theme
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  # Bold title
    plot.subtitle = element_text(size = 14, hjust = 0.5),  # Centered subtitle
    plot.caption = element_text(size = 10, hjust = 0.95),  # Right-align caption
    legend.position = "bottom",  # Place legend at the bottom
    legend.title = element_text(face = "bold"),  # Bold legend title
    legend.text = element_text(size = 10),  # Adjust legend text size
    axis.text = element_text(size = 12),  # Customize axis text size
    axis.title = element_text(face = "bold") 
    facet_wrap(~ continent)

  # scatter plot 1
    ggplot(filtered_data) +
      aes(time_period, obs_value, color = country) +
     geom_point() +
      labs(
        x = "Net Migration Rate Value",
        y = "Time Period",
        title = "Ukraine and Kuwait and the impact of war on Net Migration Rate."
      ) +
      geom_smooth(method = "lm") +
      theme_classic()
    
       filtered_data <- unicef_indicator_22 %>%
      filter(country %in% c("Ukraine", "Kuwait"))
  
  #bar chart 1
       filtered_data <- unicef_indicator_22 %>%
         filter(country %in% c("Kuwait", "Oman", "Saudi Arabia", "United Arab Emirates") & 
                  time_period == 1989, 1990, 1991)
       ggplot(filtered_data) +
         aes(x = country, y = obs_value, fill = country) +  # Set fill to distinguish bars
         geom_bar(stat = "identity") +  # Bar chart with value specified by 'obs_value'
         labs(
           title = "Observation Values in 1990 for Kuwait and selected neighbouring countries",
           x = "Country",
           y = "Net Migration Rate"
         ) +
         theme_minimal() +
         geom_bar(stat = "identity") +
         geom_text(aes(label = round(obs_value, 1)), vjust = -0.5)
#bar chart 2
       filtered_data <- unicef_indicator_22 %>%
         filter(country %in% c("Ukraine", "Hungary", "Poland", "Romania", "Slovakia") & 
                  time_period == 2022)
       ggplot(filtered_data) +
         aes(x = country, y = obs_value, fill = country) +  # Set fill to distinguish bars
         geom_bar(stat = "identity") +  # Bar chart with value specified by 'obs_value'
         labs(
           title = "Observation Values in 2022 for Ukraine and selected neighbouring countries",
           x = "Country",
           y = "Net Migration Rate"
         ) +
         theme_minimal()+
         geom_bar(stat = "identity") +
         geom_text(aes(label = round(obs_value, 1)), vjust = -0.5)
         