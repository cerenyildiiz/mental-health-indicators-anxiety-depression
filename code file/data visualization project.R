install.packages("readr")
library(readr)
install.packages("dplyr")
library(dplyr)



indicators <- Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days 
colSums(is.na(Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days))





# First Graph 

library(dplyr)
library(ggstatsplot)
library(ggplot2)

sex_data <- indicators %>%
  filter(
    Group == "By Sex",
    Subgroup %in% c("Male", "Female"),
    Indicator %in% c(
      "Symptoms of Anxiety Disorder",
      "Symptoms of Depressive Disorder"
    )
  ) %>%
  mutate(
    Subgroup = factor(Subgroup, levels = c("Male", "Female"))
  )



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
















 # Second Graph 
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



# Third Graph

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
    title    = "State-Level Distribution of Anxiety and Depression Symptoms in the U.S.",
    subtitle = "Reported Symptom Prevalence Across States",
    x        = "Percentage of Adults Reporting Symptoms (%)",
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 10.5, margin = margin(b = 8)),
    panel.spacing = unit(0.15, "lines"),
    plot.margin   = margin(10, 30, 10, 10)
  )


# Fourth Graph
library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(grid)


plot_df <- indicators %>%
  filter(
    Group %in% c("By Education", "By Educational attainment"),   
    Indicator %in% c("Symptoms of Anxiety Disorder",
                     "Symptoms of Depressive Disorder"),
    !is.na(Value),
    !is.na(Subgroup)
  ) %>%
  mutate(Education = Subgroup) %>%
  group_by(Education, Indicator) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Education = factor(
      Education,
      levels = c(
        "Less than a high school diploma",
        "High school diploma or GED",
        "Some college/Associate's degree",
        "Bachelor's degree or higher"
      )
    )
  )

p <- ggplot(plot_df, aes(x = Education, y = Value, fill = Indicator)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.62) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = function(x) paste0(round(x, 0), "%"),
    expand = expansion(mult = c(0, 0.06))
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 28)) +
  scale_fill_manual(values = c(
    "Symptoms of Anxiety Disorder"    = "lightblue",
    "Symptoms of Depressive Disorder" = "grey40"
  )) +
  labs(
    x = "",
    y = "Average Percentage ",
    fill = "",
    title = "Anxiety and Depression Symptoms Across Education Levels",
    subtitle = "Average Self-Reported Symptom Prevalence Over  Survey Waves"
  ) +
  theme_hc(base_size = 14) +
  theme(
   
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 13),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),

   
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    legend.key.size = unit(1.0, "lines"),
    legend.box = "horizontal",
    legend.spacing.x = unit(0.8, "lines"),

   
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),

   
    plot.margin = unit(c(12, 26, 12, 12), "pt")
  ) +
  guides(fill = guide_legend(nrow = 1))

p


ggsave("education_anxiety_depression_poster.png", p, width = 12, height = 6, dpi = 400)

