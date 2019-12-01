### Data Scrapping ###
library(rvest)
library(magrittr)

url <- "https://en.wikipedia.org/wiki/List_of_largest_biomedical_companies_by_revenue"

revenue_table <- read_html(url) %>%
  html_node("table") %>%
  html_table(fill = TRUE)

colnames(revenue_table)[3:ncol(revenue_table)] <- c("company", 2019:2011)

revenue <- tidyr::pivot_longer(
  data = revenue_table[, 3:ncol(revenue_table)],
  cols = `2019`:`2011`,
  names_to = "year",
  names_ptypes = list(year = integer()),
  values_to = "revenue"
)

revenue <- within(revenue, {
  revenue[revenue %in% c("", "–")] <- NA

  revenue <- stringr::str_remove(
    string = revenue,
    pattern = "Q[1-3]\\[[0-9]+\\]|\\[[0-9]+\\]"
  ) %>% trimws() %>% as.numeric()

  company_name_end <<- stringr::str_locate(
    string = company,
    pattern = "Private|[[:upper:]]{3,}:"
  )[, 1]

  company <- substr(company, 1, company_name_end - 1L)
})

### Animation ###
library(dplyr)
library(ggplot2)
library(gganimate)

revenue_formatted <- revenue %>%
  group_by(year) %>%
  mutate(revenue_with_jitter = jitter(revenue),
         rank = rank(-revenue_with_jitter),
         label = paste0(" $", revenue, " Bn.")) %>%
  group_by(company) %>%
  filter(rank <= 20) %>%
  ungroup()

static_plot <- ggplot(revenue_formatted) +
  aes(rank, group = company, fill = company, color = company) +
  geom_tile(aes(y = revenue/2, height = revenue, width = 0.9),
            alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(company, " ")), vjust = 0.2, hjust = 1, size = 7) +
  geom_text(aes(y = revenue, label = label, hjust = 0), size = 7) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = .1, color = "grey"),
        panel.grid.minor.x = element_line(size = .1, color = "grey"),
        plot.title = element_text(size = 25, hjust = 0.5, face = "bold",
                                  colour = "grey", vjust = 0),
        plot.background = element_blank(),
        plot.margin = margin(2, 4, 2, 11, "cm"))

animation <- static_plot +
  transition_states(year, transition_length = 12, state_length = 8, wrap = FALSE) +
  view_follow(fixed_x = TRUE, fixed_y = TRUE)  +
  labs(title = "Year: {closest_state}")

animate(animation, renderer = av_renderer("pharma_revenue.mp4"), nframes = 200,
        fps = 20, end_pause = 32, width = 1200, height = 1000)
