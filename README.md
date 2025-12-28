# Indicators-of-Anxiety-and-Depression-Based-on-Self-Reported-Symptoms

First, we need to import the data into the R environment via a CSV file.
```{r load-data, message=FALSE}
library(readr)
library(dplyr)

indicators <- Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days 
colSums(is.na(Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days))
```



<img width="3300" height="1200" alt="image" src="https://github.com/user-attachments/assets/1a18c7b5-eb84-444d-a5e3-1e4815f2ecfc" />

```
ggstatsplot::grouped_ggbetweenstats(
  data           = sex_data,
  x              = Subgroup,
  y              = Value,
  grouping.var   = Indicator,
  type            = "nonparametric",
  centrality.type = "nonparametric",
  digits         = 2,
  bf.message     = FALSE,
  plotgrid.args  = list(nrow = 1),
  ggplot.component = list(
    theme(
      legend.position = "none",
      axis.title.x    = element_blank(),  
      plot.subtitle = element_text(size = 7),
      strip.text    = element_text(size = 10),
      axis.text.x   = element_text(size = 9),
      axis.text.y   = element_text(size = 9),
      axis.title    = element_text(size = 10)
    )
  )
)

```
<img width="3000" height="1500" alt="image" src="https://github.com/user-attachments/assets/a5c182b6-ed31-4f54-a3e6-a320ba83ac60" />



```{r}
library(dplyr)
library(forcats)
library(ggplot2)
library(ggthemes)
library(stringr)
library(readr)

age_data <- indicators %>% 
  filter(
    Group == "By Age",
    Indicator %in% c(
      "Symptoms of Anxiety Disorder",
      "Symptoms of Depressive Disorder",
      "Symptoms of Anxiety Disorder or Depressive Disorder"
    )
  ) %>% 
  drop_na(Value)

age_summary <- age_data %>% 
  group_by(Age = Subgroup, Indicator) %>% 
  summarise(mean_value = mean(Value, na.rm = TRUE), .groups = "drop") %>% 
  mutate(
    
    age_start = readr::parse_number(Age),
    
    age_start = ifelse(
      str_detect(Age, regex("over|and over|older|\\+", ignore_case = TRUE)),
      Inf,
      age_start
    ),
   
    Age = fct_reorder(Age, age_start)
  ) %>% 
  select(-age_start)

ggplot(age_summary, aes(x = Age, y = mean_value, fill = Indicator)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x    = "",
    y    = "Average Percentage (%)",
    fill = "",   
    title    = "Comparison of Anxiety and Depression Symptom Prevalence Across Age Groups",
    subtitle = "Average Self-Reported Symptom Prevalence Over All Survey Waves"
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    expand = expansion(mult = c(0, 0.03))
  ) +
  scale_fill_manual(values = c(
    "Symptoms of Anxiety Disorder"                        = "grey70",
    "Symptoms of Depressive Disorder"                     = "grey40",
    "Symptoms of Anxiety Disorder or Depressive Disorder" = "lightblue"
  )) +
  theme_hc() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title  = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    plot.margin = margin(12, 24, 12, 12)
  )
```

<img width="2400" height="3000" alt="image" src="https://github.com/user-attachments/assets/6ee8a434-d104-495e-a764-9fb04d894754" />


```{r}
library(ggplot2)
library(ggridges)
library(viridis)
library(dplyr)
library(forcats)
library(tidyr)

state_data <- indicators %>% 
  filter(
    Group == "By State",
    Indicator == "Symptoms of Anxiety Disorder or Depressive Disorder"
  ) %>% 
  mutate(State = Subgroup) %>%
  drop_na(Value)

state_all_data <- state_data %>% 
  mutate(State = fct_reorder(State, Value, .fun = median))

ggplot(
  state_all_data,
  aes(
    x    = Value,
    y    = State,
    fill = after_stat(x)
  )
) +
  geom_density_ridges_gradient(
    scale          = 3,
    rel_min_height = 0.01
  ) +
  scale_fill_viridis(option = "C", guide = "none") +
  labs(
    title    = "Distribution of Anxiety/Depression Symptom Rates Across States",
    subtitle = "All states (By State, Symptoms of Anxiety or Depressive Disorder)",
    x        = "Percentage Reporting Symptoms (%)",
    y        = ""  
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.spacing = unit(0.15, "lines"),
    plot.margin   = margin(10, 30, 10, 10)  
  )

```
