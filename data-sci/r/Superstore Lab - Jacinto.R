library(tidyverse)
glimpse(store)
store <- janitor::clean_names(store)
store <- store |> select(customer_name, category, department, discount, order_date, order_priority, order_quantity, profit, region, sales, ship_mode, shipping_cost, state)
glimpse(store)
store <- store |> mutate(
  expenses = sales-profit
)
glimpse(store)
store$large_discount <- ifelse(store$discount > 0.1, "Yes", "No")
glimpse(store)
install.packages("stringr")
library(dplyr)
library(stringr)
midname <- store |>
  filter(((str_count(customer_name, " ") + 1) >= 3))
glimpse(midname)

uniquemidname <- midname |>
  distinct(customer_name, .keep_all = TRUE)
uniquemidname

nomidname <- store |>
  filter(((str_count(customer_name, " ") + 1) < 3))
nomidname

relocatestore <- store |>
  relocate(customer_name, category, department, order_priority, region, ship_mode, state, large_discount)
glimpse(relocatestore)

eaststore <- store |>
  filter(region == "East")
eaststore
avgprofit <- store |>
  group_by(state, department) |>
  summarize(
    avg = mean(profit)
  )
avgprofit

store <- store |>
  mutate(
    month = str_sub(order_date, 6, 7))
totordermonth <- store |>
  group_by(month) |>
  summarize(
    tot = sum(order_quantity))
totordermonth <- totordermonth |>
  arrange(desc(tot))
totordermonth
dept <- store |>
  group_by(department) |>
  summarize(
    avg = mean(discount),
    min = min(discount),
    max = max(discount),
    count = length(discount)
  )
dept

library(ggplot2)
store <- store |>
  mutate(
    monthandyear = str_sub(order_date, 1, 7)
  )

graph1 <- store |>
  group_by(monthandyear, department) |>
  summarize(
    totalprofit = sum(profit)
  )

ggplot(graph1, aes(x = monthandyear, y = totalprofit, color = department, group = department)) +
  geom_line() +
  labs(title = "Total Profit over Time by Department", x = "Year and Month", y = "Total Profit")

idk <- store |>
  mutate(
    month = str_sub(order_date, 6, 7),
    numforpriority = case_when(
      order_priority == "Not Specified" ~ "Not Specified",
      .default = "Specified"
    )
  )

ggplot(idk, aes(x = month, fill = numforpriority )) +
  geom_bar(position = "fill", alpha = 0.7)

store$return <- ifelse(store$profit < 0, 1, 0)
store$january <- ifelse(store$month == "01", 1, 0)
january_returns <- sum(store$return[store$january == 1])
january_total <- sum(store$january == 1)

other_returns <- sum(store$return[store$january == 0])
other_total <- sum(store$january == 0)

prop.test(
  x = c(january_returns, other_returns),
  n = c(january_total, other_total),
  alternative = "greater",  # tests if January > other months
  correct = FALSE
)

