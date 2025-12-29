# MENTAL HEALTH INDICATORS: ANXIETY & DEPRESSION



**Author:Ceren Yıldız**

**Institution: Eskisehir Technical University**

Mental illnesses are significant health concerns today, affecting individuals' daily lives, social relationships, and overall quality of life in multifaceted ways.Have you ever wondered which group you belong to in terms of the mental health challenges you experience in daily life?





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

CONCLUSION: When the impact of gender on anxiety and depression symptom levels is examined, it is
observed that females report higher symptom values than males for both disorders. In the context of anxiety,
the median value for females is approximately 33.85, whereas for males, it stands at around 26.20. Violin plots
indicate that the symptom distribution among female participants is both wider and extends toward higher
values, revealing greater variance and symptom intensity in women. Mann–Whitney test results strongly
corroborate this observation: a p-value of 1.77e-14 demonstrates that the diﬀerence is statistically highly
significant, while a rank-biserial correlation of –0.89 indicates a very large eﬀect size. Thus, gender creates a
distinct and robust separation in anxiety symptoms.A similar pattern is observed for depression symptoms,
although the eﬀect size is relatively smaller. The median for females is 26.00, compared to approximately
22.60 for males. The symptom distribution for females again spans a wider range and leans toward higher
values. While the Mann–Whitney test p-value of 3.34e-08 confirms the statistical significance of this diﬀerence,
the rank-biserial eﬀect of –0.64 suggests a medium-to-high level diﬀerence, albeit not as strong as in anxiety.
In conclusion, while females report higher symptom levels in both disorders, the gender disparity is notably
more pronounced in anxiety.









<img width="4200" height="1950" alt="image" src="https://github.com/user-attachments/assets/10ffccd9-315a-48fa-b688-95aad4b7c928" />



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

CONCLUSION: The graph compares the average prevalence rates of anxiety and depression across diﬀerent
age groups. Generally, the highest symptom levels are observed between ages 18 and 29. This cohort exhibits
a higher prevalence of symptoms than other age groups, and the composite index (“anxiety or depression”)
reaches its peak within this demographic. This indicates that young adults report mental health issues more
frequently.There is a steady decline in symptom rates as age increases. For instance, while rates remain
elevated in the 30–39 and 40–49 age groups, they are lower than in younger adults. In the 60-and-older
group, rates of both anxiety and depression decrease significantly, with the lowest rates seen in individuals
aged 80 and over. Furthermore, the combined rate of “anxiety or depression” is consistently higher than
the individual rates, suggesting that some individuals experience both symptoms simultaneously. Overall,
the data demonstrates that mental health symptoms are particularly prevalent in younger age groups and
diminish with age.








<img width="2400" height="3000" alt="image" src="https://github.com/user-attachments/assets/dbf2bd1b-c3e8-4ff3-97b8-8aaafd24e549" />

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

```

CONCLUSION: This ridgeline plot shows the trajectory of anxiety or current symptoms across all states
in the US. The graph represents a curve, representing the occurrence of symptoms observed in a given state
over time. The horizontal axis shows the percentage of people reporting symptoms, and the vertical axis
shows the states. The state names are not randomly listed, but rather are ordered according to the median
symptoms calculated for the state; states with lower medians appear lower on the list, and states with higher
medians appear higher. This listing makes the comparison between states more visually understandable.The
color scale also aids in interpreting the graph: purple tones represent lower percentages, while shades toward
yellow and orange represent higher percentages. In some states ( Louisiana, New Mexico, and Arizona), the
curves extend further to the right, with warmer colors (yellow and orange) covering a larger area, suggesting
that symptom rates are both higher and more frequent in these states. Conversely, in some states, such as
Minnesota, Wisconsin, and North Dakota, the distributions are narrower and centered around lower values,
suggesting that symptom rates are relatively lower and more stable in these states. Overall, the graph reveals
significant diﬀerences in both symptom levels and the breadth of the distribution across states, and that
mental health indicators have been quite geographically heterogeneous during the pandemic.




<img width="4800" height="2400" alt="image" src="https://github.com/user-attachments/assets/833b6d7a-fe5b-42b9-84df-734ca7c5ed69" />
```{r}
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
```





References:

https://catalog.data.gov/dataset/indicators-of-anxiety-or-depression-based-on-reported-frequency-of-symptoms-during-last-7-
