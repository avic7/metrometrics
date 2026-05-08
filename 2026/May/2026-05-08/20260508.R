```{python}

import pandas as pd

# Load raw data
df = pd.read_csv("tmc_raw_data_2020_2029.csv")

# Target locations
target_locations = [
    'Spadina Ave / King St W', 
    'Spadina Ave / Queen St W',
    'Spadina Ave / Richmond St W',
    'Spadina Ave / Adelaide St W',
    'Spadina Ave / Front St W'
]

# Filter locations
df = df[df['location_name'].isin(target_locations)].copy()

# Clean street names
df['intersecting_street'] = df['location_name'].str.split(' / ').str[1].str.replace(' St W', '', regex=False)

# Process dates & filter years
df['count_date'] = pd.to_datetime(df['count_date'])
df['year'] = df['count_date'].dt.year
df = df[df['year'].isin([2022, 2023, 2024, 2025])].copy()

# Add the "Era" Column
era_map = {
    2022: '2022-2023 (Early Recovery)',
    2023: '2022-2023 (Early Recovery)',
    2024: '2024-2025 (The New Normal)',
    2025: '2024-2025 (The New Normal)'
}
df['Era'] = df['year'].map(era_map)

# Filter for Mid-week days
df['day_of_week'] = df['count_date'].dt.day_name()
df = df[df['day_of_week'].isin(['Tuesday', 'Wednesday', 'Thursday'])].copy()

# Create volumes
ped_cols = ['n_appr_peds', 's_appr_peds', 'e_appr_peds', 'w_appr_peds']
bike_cols = ['n_appr_bike', 's_appr_bike', 'e_appr_bike', 'w_appr_bike']
motor_cols = [col for col in df.columns if 'cars' in col or 'truck' in col or 'bus' in col]


df['active_vol'] = df[ped_cols + bike_cols].apply(pd.to_numeric, errors='coerce').sum(axis=1)
df['motor_vol'] = df[motor_cols].apply(pd.to_numeric, errors='coerce').sum(axis=1)

# Extract time 
df['time_of_day'] = pd.to_datetime(df['start_time']).dt.strftime('%H:%M')

# Define peak periods 
am_mask = (df['time_of_day'] >= '07:00') & (df['time_of_day'] < '09:00')
pm_mask = (df['time_of_day'] >= '16:00') & (df['time_of_day'] < '19:00')

# Combine
df_am = df[am_mask].copy()
df_am['peak_period'] = 'AM Peak'

df_pm = df[pm_mask].copy()
df_pm['peak_period'] = 'PM Peak'

daily_peaks = pd.concat([df_am, df_pm])

# Group and Sum
daily_peaks = daily_peaks.groupby(
    ['year', 'Era', 'count_date', 'day_of_week', 'location_name', 'intersecting_street', 'peak_period']
)[['active_vol', 'motor_vol']].sum().reset_index()


daily_peaks.to_csv("spadina_strict_twt_peaks.csv", index=False)

print(f"Success! Processed {len(daily_peaks)} peak period records.")

```




```{r}

library(tidyverse)
library(scales)

# Dataset
df_peaks <- read_csv("spadina_strict_twt_peaks.csv", show_col_types = FALSE)

# Aggregate data for BOTH Active and Motor Volumes
chart_data <- df_peaks %>%
  group_by(Era, intersecting_street, peak_period) %>%
  summarise(
    `Active Transportation` = mean(active_vol, na.rm = TRUE),
    `Motor Vehicles` = mean(motor_vol, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Pivot to make new columns
  pivot_longer(
    cols = c(`Active Transportation`, `Motor Vehicles`),
    names_to = "Mode",
    values_to = "Volume"
  ) %>%
  
  # Missing 2022 data
  complete(Era, intersecting_street, peak_period, Mode) %>%
  
  mutate(
    peak_period = factor(peak_period, 
                         levels = c("AM Peak", "PM Peak"),
                         labels = c("AM Peak (7 AM - 9 AM)", "PM Peak (4 PM - 7 PM)")),
    intersecting_street = factor(intersecting_street, 
                                 levels = rev(c("Queen", "Richmond", "Adelaide", "King", "Front"))),
    Mode = factor(Mode, levels = c("Active Transportation", "Motor Vehicles")),
    
    label_text = ifelse(is.na(Volume), "NA", comma(round(Volume))),
    label_y = ifelse(is.na(Volume), 0, Volume)
  )

# Hex colors 
era_colors <- c("2022-2023 (Early Recovery)" = "#2d6c38", 
                "2024-2025 (The New Normal)" = "#f47920")

# Plot
p <- ggplot(chart_data, aes(x = intersecting_street, y = Volume, fill = Era)) +
  
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, na.rm = TRUE) +
  
  geom_text(aes(y = label_y, label = label_text), 
            position = position_dodge(width = 0.8), 
            hjust = -0.15, fontface = "bold", size = 3.5, na.rm = TRUE) +
  
  coord_flip() + 
  
  # Facet for both peak periods 
  facet_grid(peak_period ~ Mode, switch = "y") + 
  
  scale_fill_manual(values = era_colors) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.02, 0.20))) +
  
  labs(
    title = "Intersection Volume at Spadina Ave: Active vs. Motorized",
    subtitle = "Total Weekday (Tuesday-Wednesday-Thursday) Peak Period Volume",
    x = NULL, 
    y = "Total Average Volume",
    fill = NULL,
    caption = "Data Source: City of Toronto Open Data (Turning Movement Counts) | Viz: Atharva Vichare"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title.position = "plot", 
    plot.title = element_text(hjust = 0, face = "bold", size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0, size = 13, color = "grey30", margin = margin(b = 15)),
    
    legend.position = "top",
    legend.justification = "left", 
    legend.margin = margin(l = 0, b = 10), 
    legend.text = element_text(size = 12),
    

    strip.placement = "inside",
    
    
    strip.text.y.left = element_text(
      face = "bold", 
      size = 12, 
      angle = 90, 
      color = "white", 
      margin = margin(r = 10, l = 10)
    ),
    strip.background = element_rect(fill = "#1d4255", color = NA),
    
    # Top strip styling (White Text inside the Dark Blue)
    strip.text.x.top = element_text(face = "bold", size = 13, color = "white", margin = margin(b = 10, t = 10)),
    
    axis.text.y = element_text(face = "bold", color = "black", size = 12, margin = margin(r = 20)),
    axis.text.x = element_text(color = "black"),
    
    # Caption styling
    plot.caption = element_text(face = "italic", color = "grey40", size = 10, margin = margin(t = 20)),
    
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(),
    
    panel.spacing = unit(1.5, "lines") 
  )


suppressWarnings(print(p))
