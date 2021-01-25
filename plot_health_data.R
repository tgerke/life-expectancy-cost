library(tidyverse)
library(ggtext)
library(ggbernie)

theme_set(
  firasans::theme_ipsum_fsc(
    axis_text_family = "Fira Sans Condensed",
    axis_text_size = 10, 
    axis_title_size = 14,
    axis_title_just = "cc") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_markdown(face = "plain"),
          plot.subtitle = element_markdown(),
          plot.caption = element_markdown(),
          plot.title.position = "plot")
)

health_data <- 
  read_csv(
    here::here("data/life-expectancy-vs-health-expenditure.csv"),
    guess_max = 2500
  ) %>%
  filter(
    Year ==  2015,
    !is.na(`Health Expenditure and Financing (per capita) (OECDstat (2017))`),
    !is.na(`Life expectancy at birth, total (years)`)
  ) %>%
  mutate(
    `Heath expenditure (per capita)` = as.numeric(`Health Expenditure and Financing (per capita) (OECDstat (2017))`)
  ) %>% 
  rename(
    Country = Entity,
    `Life expectancy` = `Life expectancy at birth, total (years)`
  ) %>%
  select(
    Country, `Heath expenditure (per capita)`, `Life expectancy`
  )

bernie_plot <- ggplot(
  data = health_data %>% filter(Country != "United States"),
  aes(
    x = `Heath expenditure (per capita)`,
    y = `Life expectancy`)
  ) + 
  geom_point() + 
  scale_x_continuous(labels = scales::dollar_format()) + 
  geom_bernie(
    data = health_data %>% filter(Country == "United States"),
    aes(
      x = `Heath expenditure (per capita)`,
      y = `Life expectancy`),
    bernie = "sitting"
  ) + 
  ggrepel::geom_label_repel(
    data = health_data %>% filter(Country != "United States"),
    aes(
      x = `Heath expenditure (per capita)`,
      y = `Life expectancy`,
      label = Country
    ),
    seed = 1234
  ) + 
  ggrepel::geom_label_repel(
    data = health_data %>% filter(Country == "United States"),
    aes(
      x = `Heath expenditure (per capita)`,
      y = `Life expectancy`,
      label = Country
    ),
    seed = 1234
  ) + 
  xlab(element_blank()) +
  ylab(element_blank()) + 
  labs(
    title = "Life expectancy and health expenditure (per capita)",
    caption = glue::glue(
      "Code: github.com/tgerke/life-expectancy-cost<br>",
      "Twitter: @travisgerke"
    )
  ) 

ggsave(
    here::here("life-expectancy-cost.png"),
    plot = bernie_plot,
    device = "png",
    width = 12,
    height = 9,
    units = "in",
    bg = "white"
  )
