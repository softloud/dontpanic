library(tidyverse)
daydat <- tibble::tribble(
  ~task, ~proportion, ~type,
  "coding", 0.5, "work",
  "writing", 0.2, "work",
  "administration", 0.1, "work",
  "editing", 0.2, "work",
  "vr fitness", 0.1, "leisure",
  "time with family", 0.6, "leisure",
  "housework", 0.1, "leisure",
  "gaming", 0.2, "leisure"
)
palette <- scales::gradient_n_pal(colours = c("#C2D6D6", "#394D00"))
shades <- palette((0:7)/7)


p <- daydat %>%
  mutate(task = fct_relevel(task, levels = daydat$task),
         type = fct_relevel(type, levels = c("work", "leisure"))) %>%
  ggplot(aes(x = type, y = proportion, fill = task)) +
  geom_col(alpha = 0.9) +
  labs(x = "Work or leisure", y = "Proportion of time") +
  scale_fill_manual(values = shades)
plot(p)
