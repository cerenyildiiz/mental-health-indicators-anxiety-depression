# Indicators-of-Anxiety-and-Depression-Based-on-Self-Reported-Symptoms




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
