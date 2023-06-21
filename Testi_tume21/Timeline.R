library(ggplot2)
library(dplyr)
library(scales)  # for date formatting

# Define the timeline data
timeline_data <- data.frame(
  Event = c(
    "Act on two-year pre-primary education trial",
    "Randomization of small municipalities and sampling of large municipalities",
    "Randomization within large municipalities",
    "Baseline assessment of the 2016 cohort",
    "Group questionnaire for the 2016 cohort",
    "Randomization of the 2017 cohort",
    "Baseline assessment of the 2017 cohort",
    "First follow-up assessment of the 2016 cohort",
    "Group questionnaire for the 2017 cohort",
    "Group questionnaire for the 6 cohort",
    "First follow-up assessment of the 2017 cohort",
    "Second follow-up assessment of the 2016 cohort",
    "Group questionnaire for the 2017 cohort",
    "Teacher/school directors’ questionnaire",
    "Second follow-up assessment of the 2017 cohort"
  ),
  StartDate = as.Date(c(
    "2020-12-17", "2021-01-18", "2021-04-06", "2021-10-04", "2022-11-23",
    "2022-03-04", "2022-08-15", "2022-10-03", "2022-11-22", "2022-11-28",
    "2023-09-25", "2023-10-30", "2023-12-01", "2023-01-01", "2024-01-01"
  )),
  EndDate = as.Date(c(
    "2020-12-17", "2021-01-18", "2021-04-06", "2021-10-15", "2022-12-03",
    "2022-03-04", "2022-08-26", "2022-10-21", "2022-12-09", "2022-12-13",
    "2023-10-13", "2023-11-24", "2023-12-31", "2023-12-31", "2024-12-31"
  ))
)

# Assign unique numeric values to each event based on the start date
timeline_data <- timeline_data %>%
  arrange(StartDate) %>%
  mutate(EventNumber = row_number())

# Create the Gantt chart
gantt_chart <- ggplot(data = timeline_data) +
  geom_rect(
    aes(xmin = StartDate, xmax = EndDate, ymin = EventNumber - 0.3, ymax = EventNumber + 0.3),
    fill = "steelblue", color = "black"
  ) +
  annotate(
    "text",
    x = min(timeline_data$StartDate) - as.difftime(30, units = "days"),
    y = timeline_data$EventNumber,
    label = timeline_data$Event,
    hjust = 1,
    vjust = 0.5,
    size = 3,
    fontface = "bold"
  ) +
  scale_x_date(date_labels = "%d.%m.%Y") +
  scale_y_reverse(breaks = seq(0.5, 15.5, 2)) +  # Reverse the y-axis scale
  labs(x = "Timeline", y = NULL, title = "Project Timeline") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 8),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "grey95"),
    panel.grid = element_line(size = 0.5, color = "white")
  )

last_plot()

# Save the Gantt chart as a PDF file
ggsave("project_gantt_chart.pdf", gantt_chart, width = 8, height = 6)
