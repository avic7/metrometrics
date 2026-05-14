```{r ingestion, message=FALSE}
library(opendatatoronto)
library(dplyr)
library(reticulate) 


res_2026_list <- get_resource("4f09b38d-ed2c-4576-9c20-de40d7e6b813")
res_2024_list <- get_resource("031de918-d980-4b22-be36-0580692b9a82")

# Extract specific datsets 
feb_2026 <- res_2026_list[[2]]
feb_2024 <- res_2024_list[[2]]

```


### The Wrangling Chunk (Python)
Here, we access `feb_2024` and `feb_2026` using `r.`. We perform all the heavy logic (cancellation math and market segmentation) using Python's efficient `apply` and `groupby` methods.


```{python processing}
import pandas as pd

def run_pipeline(df_raw, year_label):
    df = df_raw.copy()
    
    # Rides Completion Math
    cols = ['reported_trips_started', 'driver_cancelled_trips', 'passenger_cancelled_trips']
    for col in cols:
        df[col] = pd.to_numeric(df[col], errors='coerce').fillna(0)
    
    df['completed_trips'] = df['reported_trips_started'] - (df['driver_cancelled_trips'] + df['passenger_cancelled_trips'])
    df['completed_trips'] = df['completed_trips'].clip(lower=0)
    
 
    df['hr'] = pd.to_datetime(df['hr'])
    df['hour'] = df['hr'].dt.hour
    df['day_name'] = df['hr'].dt.day_name()
    
    # Market Segmentation
    def categorize(row):
        h, d = row['hour'], row['day_name']
        is_wknd = d in ['Saturday', 'Sunday']
        
        if d in ['Friday', 'Saturday'] and h >= 19: return "Weekend Social"
        if not is_wknd and ((7 <= h < 10) or (16 <= h < 19)): return "Peak Rush"
        if 10 <= h < 16: return "Weekend Day" if is_wknd else "Weekday Day"
        if 19 <= h <= 23: return "Weekend Evening" if is_wknd else "Weekday Evening"
        if 0 <= h < 7: return "Weekend Late" if is_wknd else "Weeknight Late"
        return "Other"

    df['Market_Segment'] = df.apply(categorize, axis=1)
    
    # Double Aggregation
    city_hourly = df.groupby(['hr', 'Market_Segment'])['completed_trips'].sum().reset_index()
    segment_avg = city_hourly.groupby('Market_Segment')['completed_trips'].mean().reset_index()
    
    segment_avg['Year'] = year_label
    return segment_avg

# Process both datasets
summary_2024 = run_pipeline(r.feb_2024, "Feb 2024")
summary_2026 = run_pipeline(r.feb_2026, "Feb 2026")

# Combine for the R visualization chunk
full_data_py = pd.concat([summary_2024, summary_2026])

```


### The Visualization Chunk (R)
Finally, we pull the processed data back into R using `py$full_data_py` to create the high-impact plot.


```{r visualization}
library(ggplot2)
library(scales)

# 1. Access the Python object and format
plot_df <- py$full_data_py %>%
  filter(Market_Segment != "Other") %>%
  mutate(Year = factor(Year, levels = c("Feb 2026", "Feb 2024")))

# 2. Calculate comparison labels
pct_labels <- plot_df %>%
  group_by(Market_Segment) %>%
  summarise(
    v24 = sum(completed_trips[Year == "Feb 2024"]),
    v26 = sum(completed_trips[Year == "Feb 2026"]),
    max_y = max(completed_trips)
  ) %>%
  mutate(
    diff = (v26 - v24) / v24,
    label = paste0(if_else(diff > 0, "+", ""), percent(diff, accuracy = 1))
  )

# 3. Generate the Final Plot
ggplot(plot_df, aes(x = reorder(Market_Segment, completed_trips), y = completed_trips, fill = Year)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.5) +
  coord_flip() +
  # Value labels inside bars
  geom_text(aes(label = comma(completed_trips, accuracy = 1)), 
            position = position_dodge(width = 0.7), 
            hjust = 1.2, color = "white", fontface = "bold") +
  # Growth percentage labels
  geom_text(data = pct_labels, aes(x = Market_Segment, y = max_y + (max(max_y)*0.05), label = label),
            inherit.aes = FALSE, size = 4, fontface = "italic", color = "grey30") +
  scale_fill_manual(values = c("Feb 2024" = "#8FA3B5", "Feb 2026" = "#2C4C64")) +
  theme_minimal() +
  labs(
    title = "Market Segment Growth: 2024 vs 2026",
    subtitle = "Data Ingested via R | Processed in Python | Visualized via ggplot2",
    y = "Average Trips per Hour (City-Wide)", x = NULL
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )


```
