# Sea Around Us catch data for Cabo Verde
# Summarize data per country (top 10)


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

# Summarize dataset by country (fishing_entity)
summary_country <- data %>%
  group_by(fishing_entity) %>%
  summarise(landed_value = sum(landed_value, na.rm = TRUE))

# Select top 10 countries by landed value
top10_countries <- summary_country %>%
  arrange(desc(landed_value)) %>%
  head(10)

# Create new column in 'data' for country, keeping the name of the top10,
# and setting 'Other' for the rest
data$country <- ifelse(data$fishing_entity %in% top10_countries$fishing_entity,
                               data$fishing_entity, "Other")


# Summarize dataset by year and fishing_entity
summary_country_year <- data %>%
  group_by(year, country) %>%
  summarise(landed_value = sum(landed_value, na.rm = TRUE)/1e6)

# Reorder the 'country' column based on the total landed value
# reverse order to have the top country at the bottom of the plot
summary_country_year <- summary_country_year %>%
  mutate(country = factor(country, levels = rev(c(top10_countries$fishing_entity, "Other"))))

# Plot the stacked area chart
p <- ggplot(summary_country_year, aes(x = year, y = landed_value, fill = country)) +
  geom_area(alpha = 0.8) +
  labs(
    x = "Year",
    y = "Real 2019 value (Million US$)",
    fill = "Country"
  ) +
  theme_classic(base_size=20) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),  # Remove default legend title
    panel.grid.major.y = element_line( size=.1, color="black"),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),  # Add space on the right
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
  ) +
  scale_fill_brewer(palette = "Paired", direction = -1) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(1950, 2020),           # Set x-axis to start at 1950
    breaks = seq(1950, 2020, by = 10),  # Labels every 10 years
    expand = c(0, 0)
  ) +
  guides(fill = guide_legend(ncol = 5))

# Save the plot
ggsave("fig/catch_data_CV_country.png", p, width=12, height=8, dpi = 300, bg = "white")
