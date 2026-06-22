library(readr)
library(ggplot2)
library(dplyr)

sales_brygge <- read_delim("data/sales_brygge.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

hotel_nights = read_delim(here::here("data", "hotel_nights.csv"), 
                          locale = locale(encoding="latin1"),
                          delim = ";")

# hotel_nordland = read_delim(here::here("data", "hotel_nights_nordland.csv"),
#                           locale = locale(encoding = "latin1"),
#                           delim = ";")

hotel_long = hotel_nights |> 
  tidyr::pivot_longer(cols = -region, 
                      names_to = c("month", "year"), 
                      names_sep = "-",
                      values_to = "value") |> 
  dplyr::filter(value > 0)

hotel_year = hotel_long |> 
  dplyr::group_by(year, region) |> 
  dplyr::summarize(value = sum(value))

hotel_year$year = as.numeric(hotel_year$year)+2000

hotel_plot = ggplot(hotel_year |> 
                      dplyr::filter(year < 2024), 
                    aes(x = year,  
                        y = value, 
                        color = region)) + 
  geom_line(linewidth = 2) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks = seq(2009, 2023, by = 2)) +
  ylab("Guest nights in summer (on holiday)")

# ggsave(hotel_plot, 
#        filename = here::here("outputs", "hotel_plot.png"),
#        width = 10, 
#        height = 10, 
#        dpi = 300)

hotel_nord = hotel_year |> dplyr::filter(region == "18 Nordland - Nordlánnda")

scale_factor <- max(sales_brygge$Sales, na.rm = TRUE) /
  max(hotel_nord$value, na.rm = TRUE)

sales_plot = ggplot(sales_brygge, aes(x = Year, 
                                      y = Sales)) + 
  geom_vline(xintercept = 2013, 
             color = "#C0394B", 
             linetype = "dotted",
             linewidth = 1.3,
             alpha = 0.8) +
  geom_line(data = hotel_nord |> dplyr::filter(region == "18 Nordland - Nordlánnda"),
            aes(x = year, y = value * scale_factor),
            linetype = "dashed",
            linewidth = 1.3,
            alpha = 0.25) +
  geom_line(linewidth = 2, 
            color = "#2196A6") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, 30000000, by = 5000000),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(0, 25000000, by = 1000000),
                     sec.axis = sec_axis(
                       transform = ~ . / scale_factor,
                       labels = scales::comma,
                       breaks = seq(0, 600000, by = 100000),
                       name = "Guest nights in Nordland (summer)")) +
  theme(panel.grid = element_blank(),
        axis.title.y = element_text(size = 18, vjust = 2.5),
        axis.title.y.right = element_text(size = 18, vjust = 2.5),
        axis.title.x = element_text(size = 18, color = "black"),
        axis.text = element_text(size = 15, color = "black"),
        plot.margin = margin(r = 10,
                             t = 10,
                             b = 10,
                             l = 10)) +
  ylab("Total revenue (NOK)") +
  xlab("Year")

ggsave(sales_plot, filename = here::here("outputs", "sales_plot.png"), width = 10, height = 10, dpi = 300)
      