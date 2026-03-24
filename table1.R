library(tidyverse)
library(eurostat)
library(gt)

## Load Eurostat datasets ------

datasets_eurostat <- c("prc_hicp_aind")

for (dataset in datasets_eurostat){
  assign(dataset, 
         get_eurostat(dataset, stringsAsFactors = F, cache = F) |>
           rename(date = TIME_PERIOD)
  )
}

flag_url <- function(code) {
  code <- tolower(code)
  dplyr::case_when(
    code == "ea20" ~ "https://flagcdn.com/eu.svg",  # euro area proxy
    TRUE ~ paste0("https://flagcdn.com/", code, ".svg")
  )
}

round_flag <- function(code) {
  sprintf(
    "<div style='width:22px;height:22px;border-radius:50%%;overflow:hidden;background:white;'>
       <img src='%s' style='width:100%%;height:100%%;object-fit:contain;'>
     </div>",
    flag_url(code)
  ) %>% gt::html()
}

coicop <- get_eurostat_dic("coicop", lang = "fr") %>%
  setNames(c("coicop", "Coicop"))


table1 <- prc_hicp_aind %>%
  filter(date == as.Date("2025-01-01"),
         geo %in% c("DE", "FR", "IT", "EA20"),
         unit == "RCH_A_AVG",
         coicop %in% c(paste0("CP0", 0:9), paste0("CP1", 0:3), "CP041", "CP0830")) %>%
  select_if(~ n_distinct(.) > 1) %>%
  left_join(coicop, by = "coicop") %>%
  spread(geo, values) %>%
  select(coicop, Coicop, FR, EA20, everything()) %>%
  gt() %>%
  cols_label(
    coicop = "Code",
    FR   = round_flag("FR"),
    DE   = round_flag("DE"),
    IT   = round_flag("IT"),
    EA20 = round_flag("EA20")
  ) %>%
  fmt_number(columns = c(FR, EA20, DE, IT), decimals = 1)

table1


gt::gtsave(table1, "table1.html")
gt::gtsave(table1, "table1.png")
gt::gtsave(table1, "table1.pdf")
