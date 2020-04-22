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
write.csv(revenue, file = "top_biomedical_companies/data/biomedical_companies_revenue.csv",
          quote = FALSE, row.names = FALSE)

### Animation ###
library(dplyr)
library(ggplot2)
library(gganimate)

revenue_formatted <- revenue %>%
  group_by(year) %>%
  mutate(
    revenue_with_jitter = jitter(revenue),
    rank = rank(-revenue_with_jitter),
    company = reorder(company, revenue),
    label = paste0(" $", revenue, " Bn."),
    highlight = case_when(
      company == "Novartis" ~ "Novartis",
      company == "Roche" ~ "Roche",
      TRUE ~ "Other"
    )
  ) %>% 
  group_by(company) %>%
  filter(rank <= 10) %>%
  ungroup()

static_plot <- ggplot(revenue_formatted) +
  aes(rank, revenue, group = company, fill = highlight) +
  geom_col(width = 0.9, alpha = 0.8) +
  geom_text(aes(y = 0, label = paste(company, " ")), vjust = 0.2, hjust = 1, size = 7) +
  geom_text(aes(y = revenue, label = label, hjust = 0), size = 7) +
  coord_flip(clip = "off") +
  scale_x_reverse() +
  theme_classic() +
  ggeasy::easy_remove_axes() +
  theme(
    plot.title = element_text(
      size = 44,
      vjust = 0,
      hjust = 0,
      face = "bold",
      color = "black"
    ),
    plot.subtitle = element_text(size = 30, margin = margin(t = 10, b = 10)),
    legend.position = "none",
    plot.margin = margin(2, 4, 2, 8, "cm")
  ) +
  scale_fill_manual(values = c("Other" = "#cccccc", "Roche" = "#0066CC", "Novartis" = "#E8580F"))

animation <- static_plot +
  transition_states(year, transition_length = 12, state_length = 8, wrap = TRUE) +
  view_follow(fixed_x = TRUE, fixed_y = TRUE)  +
  labs(
    title = "Roche Surpassed Its Local Rival Novartis",
    subtitle = "Revenue in {closest_state}"
  )

animate(animation, renderer = gifski_renderer("pharma_revenue.gif"), nframes = 200,
        fps = 20, width = 1200, height = 1000)
