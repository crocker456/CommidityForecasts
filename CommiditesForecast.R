library(Quandl)
library(tidyverse)
library(tidyquant)
library(forecast)
library(timetk)
library(sweep)
list = c("CHRIS/CME_DA1", "CHRIS/ICE_KC1", "CHRIS/CME_C1","CHRIS/ICE_KC1", "CHRIS/ICE_CC1", "CHRIS/CME_RR1", "CHRIS/ICE_SB1", "CHRIS/ICE_OJ1", "CHRIS/CME_O1", "CHRIS/CME_LN1","CHRIS/CME_LC1")
quandl_api_key("iJVzgLaYsv3bWFfv1JTm")
mydata = tq_get(list, get = "quandl", from = "2015-01-01", to ="2018-10-10")
mydata = mydata %>% mutate(symbolNew = case_when(symbol == "CHRIS/CME_DA1" ~"CME Milk Futures",
                                                 symbol == "ODA/PBEEF_USD" ~ "Cattle, Beef (ODA)",
                                                 symbol == "CHRIS/CME_C1" ~ "CME Corn Futures",
                                                 symbol == "CHRIS/ICE_KC1" ~ "ICE Coffee Futures",
                                                 symbol == "CHRIS/ICE_CC1" ~ "ICE Cocoa Futures",
                                                 symbol == "CHRIS/CME_RR1" ~ "CME Rice Futures",
                                                 symbol == "CHRIS/ICE_SB1" ~ "ICE Sugar Futures",
                                                 symbol == "CHRIS/ICE_OJ1" ~ "CME Orange Juice Futures",
                                                 symbol == "CHRIS/CME_O1" ~ "CME Oats Futures",
                                                 symbol == "CHRIS/CME_LC1" ~ "CME Cattle Futures",
                                                 symbol == "CHRIS/CME_LN1" ~ "Pork Futures",
                                                 
                                                 TRUE ~ as.character(symbol)))
mydata = mydata %>% select(date, symbolNew, settle) %>%  group_by(Date=floor_date(date, "month"), symbolNew) %>%
  summarize(settle=mean(settle))
monthly_qty_by_cat2_nest <- mydata %>%
  group_by(symbolNew) %>%
  nest(.key = "data.tbl")
monthly_qty_by_cat2_ts <- monthly_qty_by_cat2_nest %>%
  mutate(data.ts = map(.x       = data.tbl, 
                       .f       = tk_ts, 
                       select   = -Date, 
                       start    = 2016,
                       freq     = 12))
monthly_qty_by_cat2_ts
monthly_qty_by_cat2_fit <- monthly_qty_by_cat2_ts %>%
  mutate(fit.ets = map(data.ts, auto.arima))

augment_fit_ets <- monthly_qty_by_cat2_fit %>%
  mutate(augment = map(fit.ets, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(augment, .drop = TRUE)
monthly_qty_by_cat2_fcast <- monthly_qty_by_cat2_fit %>%
  mutate(fcast.ets = map(fit.ets, forecast, h = 12))
monthly_qty_by_cat2_fcast_tidy <- monthly_qty_by_cat2_fcast %>%
  mutate(sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
  unnest(sweep)
fig = monthly_qty_by_cat2_fcast_tidy %>%
  ggplot(aes(x = index, y = settle, color = key, group = symbolNew)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  labs(title = "Commodity Forecasts",
       subtitle = "Using Arima Model",
       x = "", y = "Units") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap(~ symbolNew, scales = "free_y", ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig
ggsave("commodityForcast.png", fig, height = 8, width = 12)

