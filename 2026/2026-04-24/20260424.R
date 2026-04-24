---
title: "Open_data"
output: html_document
date: "2026-04-23"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown



```{r cars}
# Load required libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(ggtext)
library(showtext)
library(readr)

```


## TTC data 
```{r}

library(opendatatoronto)
library(dplyr)


package <- show_package("b68cb71b-44a7-4394-97e2-5d2f41462a5d")
package


resources <- list_package_resources("b68cb71b-44a7-4394-97e2-5d2f41462a5d")


datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))


data <- filter(datastore_resources, row_number()==1) %>% get_resource()
data

```


## Font style 

```{r}

# Load custom fonts
font_add_google("Outfit", "outfit")
font_add_google("Merriweather", "merriweather")
showtext_auto()


```




## TTC Delay


```{r}

#  mapping dictionary for TTC delay codes
code_dict <- read_csv("Code Descriptions.csv")

# Join the datasets
delay_summary <- data %>%
  left_join(code_dict, by = c("Code" = "CODE")) %>%
  

  mutate(
    # Regex 
    DESCRIPTION = str_replace_all(DESCRIPTION, "[^[:ascii:]]+", "-"),
    
    # Format the hyphens
    DESCRIPTION = str_replace_all(DESCRIPTION, "\\s*-\\s*", " - "),
    
    # Squish extra spaces and convert to Title Case
    DESCRIPTION = str_squish(DESCRIPTION),
    DESCRIPTION = str_to_title(DESCRIPTION)
  ) %>%

  
  # Group by the newly cleaned DESCRIPTION column
  group_by(DESCRIPTION) %>%
  summarise(Total_Delay_Mins = sum(`Min.Delay`, na.rm = TRUE)) %>%
  arrange(desc(Total_Delay_Mins)) %>%
  
  # Remove NA descriptions 
  filter(!is.na(DESCRIPTION)) %>%
  head(10) %>%
  
  # Prepare factors for plotting
  mutate(
    DESCRIPTION = reorder(DESCRIPTION, Total_Delay_Mins),
    is_top_reason = ifelse(row_number() == 1, "yes", "no")
  )

# Get the exact name of the top reason to use dynamically in the subtitle
top_reason <- as.character(delay_summary$DESCRIPTION[1])


```

```{r pressure, echo=FALSE}


p <- ggplot(delay_summary, aes(x = Total_Delay_Mins, y = DESCRIPTION, color = is_top_reason)) +
  geom_segment(aes(x = 0, xend = Total_Delay_Mins, y = DESCRIPTION, yend = DESCRIPTION), 
               linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = format(Total_Delay_Mins, big.mark = ",")),
            hjust = -0.4, family = "outfit", size = 4.5, fontface = "bold", show.legend = FALSE) +
  
  scale_color_manual(values = c("yes" = "#D9381E", "no" = "#8C92AC")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  

  annotate(
    geom = "label",
    x = Inf,             
    y = 2,                 
    label = "In the context of the TTC, 'On Diversion' means a streetcar had to abruptly\nchange its route or turn back early. Because streetcars are stuck on tracks,\nif there is a fire, a protest, or a massive collision blocking the intersection\nahead, they cannot just steer around it. They have to divert to a different\ntrack loop, causing massive delays for passengers waiting further down the line.",
    hjust = 1,             
    vjust = 0,
    family = "outfit",
    color = "#555555",
    fill = "#EFEFEF",      
    label.size = 0.5,        
    size = 5.5,
    fontface = "italic",
    lineheight = 1.2
  ) +

  
  #  Title/Subtitle
  labs(
    title = "What is Stopping Toronto's Streetcars?",
    subtitle = paste0("Cumulative delay minutes by incident type. <span style='color:#D9381E;'>**", 
                      top_reason, "**</span> dominates the downtime."),
    caption = "Data: Toronto Open Data (TTC) | Viz: Atharva Vichare",
    x = "Total Minutes Delayed",
    y = NULL 
  ) +
  
  # Theme
  theme_minimal(base_family = "outfit") +
  theme(
    plot.background = element_rect(fill = "#F9F9F6", color = NA),
    panel.background = element_rect(fill = "#F9F9F6", color = NA),
    plot.title = element_text(family = "merriweather", face = "bold", size = 20, margin = margin(b = 10)),
    plot.subtitle = element_markdown(size = 14, margin = margin(b = 20), color = "#4A4A4A"),
    plot.caption = element_text(size = 10, color = "#888888", hjust = 1, margin = margin(t = 20)),
    axis.text.y = element_text(size = 12, face = "bold", color = "#333333"),
    axis.text.x = element_blank(), 
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E0E0E0", linetype = "dotted"),
    legend.position = "none",
    plot.margin = margin(20, 30, 20, 20)
  )


print(p)
# ggsave("TTC_Delay_Lollipop.png", plot = p, width = 10, height = 8, dpi = 300)
