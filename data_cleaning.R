
# Loading libraries

library(tidyverse)
library(janitor)
library(ggplot2)
library(lubridate)
library(ggthemes)

# Loading data

classification <- read_csv("data/company_classification.csv") %>%
  select(
    coprd_company_code,
    company_name,
    coprd_date,
    product_name_mst,
    coprd_industry_code:owner_gp_name
  ) %>%
  rename(
    company_code = coprd_company_code,
    date = coprd_date,
    product_name = product_name_mst,
    industry_code = coprd_industry_code,
    owner_code = coprd_owner_code,
    owner_name = owner_gp_name
  )

financial <- read_csv("data/financial_statements.csv") %>%
  rename(company_code = sa_finance1_cocode,
         company_name = sa_company_name,
         date = sa_finance1_year) %>%
  select(company_code:sa_sales)

# Joining datasets

join_dat <- classification %>% 
  full_join(financial, by = c("company_code", "company_name", "date")) %>% 
  mutate(central_gov = ifelse(owner_name == "Central Govt. - Commercial Enterprises", 1, 0)) %>% 
  mutate(date = mdy(date),
         industry_code = as.factor(industry_code),
         company_code = as.factor(company_code))

n <- nrow(join_dat)

soe_dat <- join_dat %>%
  mutate(value = rep(1, n)) %>%
  group_by(company_code) %>% 
  mutate(privatized = ifelse(central_gov - lag(central_gov, order_by = date) == -1, 1, 0)) %>%
  ungroup() %>%
  group_by(coprd_industry_name) %>%
  mutate(industry_count = sum(value)) %>%
  drop_na(central_gov) %>% 
  mutate(industry_gov_perc = sum(central_gov) / industry_count) %>%
  select(-value) %>%
  mutate(central_gov = as.factor(central_gov)) %>%
  group_by(company_code, coprd_industry_name)

# Visualizing industry

industry_year_count_bar <- soe_dat %>% 
  drop_na(central_gov) %>% 
  ggplot(aes(x = reorder(coprd_industry_name, industry_count), fill = central_gov)) +
  geom_bar() +
  labs(x = "", y = "Count") +
  coord_flip() +
  scale_fill_manual(name = "", labels = c("Private", "Government"), values = c("indianred", "steelblue2")) +
  theme_clean()

industry_year_gov_perc_bar <- soe_dat %>% 
  ggplot(aes(x = reorder(coprd_industry_name, industry_gov_perc), fill = central_gov)) +
  geom_bar(position = "fill") +
  labs(x = "", y = "Percent") +
  coord_flip() +
  scale_fill_manual(name = "", labels = c("Private", "Government"), values = c("indianred", "steelblue2")) +
  theme_clean()





