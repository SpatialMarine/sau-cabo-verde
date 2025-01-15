# Sea Around Us catch data for Cabo Verde
# Summarize reliability


# Load libraries
library(openxlsx)  # this is better than xlsx package to handle large datasets
library(dplyr)     # for data manipulation
library(ggplot2)   # for data visualization

# Set dataset path
# dataset prepared by Dr Gill Ainsworth
# Raw data: https://www.seaaroundus.org/data/#/eez/132?chart=catch-chart&dimension=gear&measure=value&limit=10
data_path <- "~/SML Dropbox/gitdata/sau-cabo-verde/SeaAroundUs_Cabo_Verde_fishing data.xlsx"

# Load dataset
data <- read.xlsx(data_path, sheet = 1)

# Calculate reliability per year
reliability_year <- data %>%
  group_by(year) %>%
  summarise(reliability = mean(uncertainty_score, na.rm = TRUE))

# plot reliability
p <- ggplot(reliability_year, aes(x = year, y = reliability)) +
  geom_line() +
  labs(
    x = "Year",
    y = "Reliability"
  ) +
  theme_classic(base_size=20) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="black"),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),  # Add space on the right
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
  )
