library(ggplot2)
library(dplyr)
library(scales)  # for date formatting
library(readxl)
library(tidyverse)
library(plan)
library(tidyr)

# Define the timeline data
excel <- "C:/Users/03248355/Dropbox/OKM esikoulukokeilu/Seurantatutkimus/Timeline_2.xlsx"

timeline_data <- read_excel(excel)

timeline_data <- timeline_data %>% 
  mutate(start = as.Date(StartDate), end = as.Date(EndDate))

timeline_data1 <- timeline_data %>% 
  gather(key=date_type, value=date, -Event, -EventNumber, -Category, -StartDate, -EndDate)

timeline_data2 <- timeline_data1 %>%
  mutate(date = as.Date(date))

SD <- timeline_data2 %>%
  filter(Category=="SG"|Category=="S17"| Category=="S16")

gantt_chart <- ggplot() +
  geom_line(data = SD,
            mapping=aes(x = fct_rev(fct_inorder(Event)), y = date, color=Category), size=5) +
  coord_flip() +
  labs(title="Timeline",
       x = "Event",
       y = "Date",
       colour = "Category") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.minor = element_line(colour="white", size=0.5),
        legend.position="right",
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 5))


# Save the Gantt chart as a PDF file
ggsave("project_gantt_chart4.pdf", gantt_chart, width = 8, height = 3.5)