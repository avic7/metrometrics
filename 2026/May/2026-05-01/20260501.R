library(tidyverse)
library(janitor)
library(showtext)

font_add_google("Outfit", "outfit")
font_add_google("Merriweather", "merriweather")
showtext_auto()



dataset <- readr::read_csv("https://figure.nz/table/TSQ8lkuKnyzfERF3/download") %>%
  janitor::clean_names() %>%
  dplyr::select(-value_unit, -null_reason, -metadata_1)

write_csv(dataset, "NZ.csv")

theme_metrometrics <- function() {
  theme_minimal(base_family = "outfit") +
    theme(
      plot.background = element_rect(fill = "#FAF9F6", color = NA),
      panel.background = element_rect(fill = "#FAF9F6", color = NA),
      text = element_text(color = "#2C2C2C"),
      plot.title = element_text(family = "merriweather", face = "bold", size = 16, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, color = "#5A5A5A", margin = margin(b = 15)),
      axis.text = element_text(color = "#888888", size = 10),
      axis.title = element_text(color = "#5A5A5A", size = 11, face = "bold"),
      panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.key = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
}

cereal_palette <- c("Wheat" = "#FFCA3A", "Barley" = "#1D3557", "Oats" = "#FB8500", "Maize" = "#2A9D8F")

# Data wrangling 
df <- read_csv("NZ.csv", show_col_types = FALSE)

df_area <- df %>% filter(measure == "Total Area of Farms")

cereals <- c("Wheat", "Barley", "Oats", "Maize")

df_cereals <- df %>%
  filter(str_detect(measure, paste(cereals, collapse = "|"))) %>%
  mutate(
    Crop = str_extract(measure, "^[A-Za-z]+"),
    Type = case_when(
      str_detect(measure, "area sown") ~ "Area",
      str_detect(measure, "yield") ~ "Yield",
      TRUE ~ "Other"
    )
  ) %>%
  filter(Type %in% c("Area", "Yield")) %>%
  select(year_ended_june, Crop, Type, value)

df_gap <- df_cereals %>%
  pivot_wider(names_from = Type, values_from = value) %>%
  drop_na(Area, Yield) %>%
  filter(Area > 0) %>%
  mutate(yield_per_ha = Yield / Area)

max_yields <- df_gap %>%
  group_by(Crop) %>%
  summarize(max_yield_per_ha = max(yield_per_ha, na.rm = TRUE), .groups = 'drop')

df_gap_analysis <- df_gap %>%
  left_join(max_yields, by = "Crop") %>%
  mutate(potential_yield = Area * max_yield_per_ha) %>%
  group_by(year_ended_june) %>%
  summarize(
    actual_yield = sum(Yield, na.rm = TRUE),
    potential_yield = sum(potential_yield, na.rm = TRUE),
    .groups = 'drop'
  )


# Summary Block 
text <- str_wrap(
  "While the cultivated area for barley, wheat, maize, and oats has declined, total yields have skyrocketed. This structural pivot reveals that modern growth is powered entirely by agricultural technology, not territorial expansion.", 
  width = 135
)

p_text <- ggplot() +
  annotate("text", x = 0, y = 1, label = text, 
           hjust = 0, vjust = 1, family = "outfit", size = 5.5, color = "#2C2C2C", lineheight = 1.3) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FAF9F6", color = NA),
    panel.background = element_rect(fill = "#FAF9F6", color = NA),
    plot.margin = margin(10, 20, 0, 20)
  ) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1))

# Macro Area 
p1 <- ggplot(df_area, aes(x = year_ended_june, y = value / 1e6)) +
  geom_area(fill = "#D3D3D3", alpha = 0.5) +
  geom_line(color = "#5A5A5A", linewidth = 1) +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  labs(title = "Total Area of Farms", y = "Area Available (Hectares) ", x = NULL) +
  theme_metrometrics()

#  Sown Area 
p2 <- df_cereals %>% filter(Type == "Area") %>%
  ggplot(aes(x = year_ended_june, y = value / 1000, fill = Crop)) +
  geom_area(alpha = 0.9) +
  scale_fill_manual(values = cereal_palette) +
  scale_y_continuous(labels = label_number(suffix = "K")) +
  labs(title = "Area Sown by Crop Type", y = "Area Sown (Hectares)", x = NULL) +
  theme_metrometrics()

# Total Yield 
p3 <- df_cereals %>% filter(Type == "Yield") %>%
  ggplot(aes(x = year_ended_june, y = value / 1000, fill = Crop)) +
  geom_area(alpha = 0.9) +
  scale_fill_manual(values = cereal_palette) +
  scale_y_continuous(labels = label_number(suffix = "K")) +
  labs(title = "Total Yield by Crop Type", y = "Yield (Tonnes)", x = NULL) +
  theme_metrometrics() + theme(legend.position = "none")

# The Yield Gap 
p4 <- ggplot(df_gap_analysis, aes(x = year_ended_june)) +
  geom_ribbon(aes(ymin = actual_yield / 1e6, ymax = potential_yield / 1e6), fill = "#E63946", alpha = 0.15) +
  geom_line(aes(y = potential_yield / 1e6, color = "Peak Potential Yield"), linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = actual_yield / 1e6, color = "Actual Total Yield"), linewidth = 1.2) +
  scale_color_manual(values = c("Peak Potential Yield" = "#E63946", "Actual Total Yield" = "#1D3557")) +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  labs(title = "The Efficiency Gap", y = "Yield (Tonnes)", x = NULL) +
  theme_metrometrics() + theme(legend.title = element_blank())


dashboard <- p_text / (p1 + p2) / (p3 + p4) +
  plot_layout(heights = c(0.2, 1, 1)) + 
  plot_annotation(
    title = 'Beyond the Pasture: The Quiet Cereal Revolution',
    caption = 'Data: Stats NZ (TidyTuesday) | Viz: Atharva Vichare (Metrometrics)',
    theme = theme(
      plot.title = element_text(family = "merriweather", face = "bold", size = 28, color = "#2C2C2C", margin = margin(t = 10, b = 15)),
      plot.caption = element_text(family = "outfit", size = 12, color = "#888888"),
      plot.background = element_rect(fill = "#FAF9F6", color = NA),
      plot.margin = margin(20, 20, 20, 20)
    )
  )

print(dashboard)
