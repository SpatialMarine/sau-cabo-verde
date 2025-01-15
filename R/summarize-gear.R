# Sea Around Us catch data for Cabo Verde
# Summarize data per sector


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

# Calculate reported catch per year
reported_year <- data %>%
  filter(reporting_status == "Reported") %>%
  group_by(year) %>%
  summarise(landed_value = sum(landed_value, na.rm = TRUE)/1e6)

# Rename gear types
# keep: purse seine, longline, gillnets, bottom trawl, other, unkown
# the rest of gears group them as "small scale"
data$gear_type <- ifelse(data$gear_type %in% c("purse seine", "longline", "gillnet", "bottom trawl", "other", "unknown"),
                         data$gear_type, "small scale")

# Summarize dataset by gear
summary_gear <- data %>%
  group_by(gear_type) %>%
  summarise(landed_value = sum(landed_value, na.rm = TRUE)) %>%
  arrange(desc(landed_value))

# Summarize dataset by year and gear
summary_gear_year <- data %>%
  group_by(year, gear_type) %>%
  summarise(landed_value = sum(landed_value, na.rm = TRUE)/1e6)

# Reorder the 'fishing_sector' column based on the total landed value
# reverse order to have the top country at the bottom of the plot
summary_gear_year <- summary_gear_year %>%
  mutate(gear_type = factor(gear_type, levels = rev(c(summary_gear$gear_type))))

# Plot the stacked area chart
p <- ggplot(summary_gear_year, aes(x = year, y = landed_value, fill = gear_type)) +
  geom_area(alpha = 0.8) +
  labs(
    x = "Year",
    y = "Real 2019 value (Million US$)",
    fill = "Gear"
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

# add line for reported catch
p <- p +
  geom_line(
    data = reported_year,  # Use reported_year data for the line
    aes(x = year, y = landed_value),
    color = "black",       # Black line
    size = 1,               # Line thickness
    inherit.aes = FALSE    # Do not inherit aesthetics from ggplot()
  ) +
  # add vertical line at 1955 and up to 30
  geom_segment(
    aes(
      x = 1955, xend = 1955,  # Fixed x position
      y = 4.4, yend = 30  # Start and end points for y
    ),
    linetype = "solid",    # Solid line
    color = "black",         # Line color
    size = 1,
    inherit.aes = FALSE    # Do not inherit aesthetics from ggplot()
  ) +
  annotate("text", x = 1956, y = 32, label = "Reported catch", size = 6)

# Save the plot
ggsave("fig/catch_data_CV_gear.png", p, width=12, height=8, dpi = 300, bg = "white")
