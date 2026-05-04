options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales)

source("functions-1.R")
source("rating_variables.R")
monthlist <- sprintf("%02d", 1:12)
y <- 2014

## Plan (Enrollment and Contract) data

load_month <- function(m, y) {
  c_path <- paste0("/home/salizad/econ470/a0/work/ma-data/ma/enrollment/Extracted Data/CPSC_Contract_Info_", y, "_", m, ".csv")
  e_path <- paste0("/home/salizad/econ470/a0/work/ma-data/ma/enrollment/Extracted Data/CPSC_Enrollment_Info_", y, "_", m, ".csv")
  
  contract.info <- read_contract(c_path) %>%
    distinct(contractid, planid, .keep_all = TRUE)   
  
  enroll.info <- read_enroll(e_path)
  
  contract.info %>%
    left_join(enroll.info, by = c("contractid","planid")) %>%
    mutate(month = as.integer(m), year = y)
}
plan.data <- map_dfr(monthlist, ~ load_month(.x, y)) %>%
  arrange(contractid, planid, state, county, month) %>%
  group_by(state, county) %>%
  fill(fips, .direction = "downup") %>%
  ungroup() %>%
  group_by(contractid, planid) %>%
  fill(plan_type, partd, snp, eghp, plan_name, .direction = "downup") %>%
  ungroup() %>%
  group_by(contractid) %>%
  fill(org_type, org_name, org_marketing_name, parent_org, .direction = "downup") %>%
  ungroup()

plan.data.dt <- as.data.table(plan.data)
setorder(plan.data.dt, contractid, planid, fips, year, month)

plan.year <- plan.data.dt[
  , {
    nonmiss <- !is.na(enrollment)
    n <- sum(nonmiss)
    list(
      n_nonmiss = n,
      avg_enrollment = if (n>0) mean(enrollment[nonmiss]) else NA_real_,
      sd_enrollment  = if (n>1) sd(enrollment[nonmiss]) else NA_real_,
      min_enrollment = if (n>0) min(enrollment[nonmiss]) else NA_real_,
      max_enrollment = if (n>0) max(enrollment[nonmiss]) else NA_real_,
      first_enrollment = if (n>0) enrollment[which(nonmiss)[1]] else NA_real_,
      last_enrollment  = if (n>0) enrollment[tail(which(nonmiss), 1)] else NA_real_,
      state  = tail(state, 1),
      county = tail(county, 1),
      org_type = tail(org_type, 1),
      plan_type = tail(plan_type, 1),
      partd = tail(partd, 1),
      snp   = tail(snp, 1),
      eghp  = tail(eghp, 1),
      org_name = tail(org_name, 1),
      org_marketing_name = tail(org_marketing_name, 1),
      plan_name = tail(plan_name, 1),
      parent_org = tail(parent_org, 1),
      contract_date = tail(contract_date, 1)
    )
  },
  by = .(contractid, planid, fips, year)
]

plan.data.2014 <- as_tibble(plan.year)

## Service Area data

load_month_sa <- function(m, y) {
  path <- paste0("/home/salizad/econ470/a0/work/ma-data/ma/service-area/Extracted Data/MA_Cnty_SA_",y, "_", m, ".csv")
  
  read_service_area(path) %>%
    mutate(month = as.integer(m), year = y)
}
service.year <- map_dfr(monthlist, ~ load_month_sa(.x, y))

service.year <- service.year %>%
  arrange(contractid, fips, state, county, month)

service.year <- service.year %>%
  group_by(state, county) %>%
  fill(fips, .direction = "downup") %>%
  ungroup() %>%
  group_by(contractid) %>%
  fill(plan_type, partial, eghp, org_type, org_name, .direction = "downup") %>%
  ungroup()

service.data.2014 <- service.year %>%
  group_by(contractid, fips, year) %>%
  arrange(month, .by_group = TRUE) %>%
  summarize(
    state     = last(state),
    county    = last(county),
    org_name  = last(org_name),
    org_type  = last(org_type),
    plan_type = last(plan_type),
    partial   = last(partial),
    eghp      = last(eghp),
    ssa       = last(ssa),
    notes     = last(notes),
    .groups = "drop"
  )

## Penetration data

load_month_pen <- function(m, y) {
  path <- paste0("/home/salizad/econ470/a0/work/ma-data/ma/penetration/Extracted Data/State_County_Penetration_MA_",y, "_", m, ".csv")
  
  read_penetration(path) %>%
    mutate(month = as.integer(m), year = y)
}
ma.penetration <- map_dfr(monthlist, ~ load_month_pen(.x, y)) %>%
  arrange(state, county, month) %>%
  group_by(state, county) %>%
  fill(fips, .direction = "downup") %>%
  ungroup()

pen.2014 <- ma.penetration %>%
  group_by(fips, state, county, year) %>%
  arrange(month, .by_group = TRUE) %>%
  summarize(
    n_elig  = sum(!is.na(eligibles)),
    n_enrol = sum(!is.na(enrolled)),
    
    avg_eligibles   = ifelse(n_elig  > 0, mean(eligibles, na.rm = TRUE), NA_real_),
    sd_eligibles    = ifelse(n_elig  > 1,  sd(eligibles,  na.rm = TRUE), NA_real_),
    min_eligibles   = ifelse(n_elig  > 0, min(eligibles,  na.rm = TRUE), NA_real_),
    max_eligibles   = ifelse(n_elig  > 0, max(eligibles,  na.rm = TRUE), NA_real_),
    first_eligibles = ifelse(n_elig  > 0, first(na.omit(eligibles)),     NA_real_),
    last_eligibles  = ifelse(n_elig  > 0,  last(na.omit(eligibles)),     NA_real_),
    
    avg_enrolled    = ifelse(n_enrol > 0, mean(enrolled,   na.rm = TRUE), NA_real_),
    sd_enrolled     = ifelse(n_enrol > 1,  sd(enrolled,    na.rm = TRUE), NA_real_),
    min_enrolled    = ifelse(n_enrol > 0, min(enrolled,    na.rm = TRUE), NA_real_),
    max_enrolled    = ifelse(n_enrol > 0, max(enrolled,    na.rm = TRUE), NA_real_),
    first_enrolled  = ifelse(n_enrol > 0, first(na.omit(enrolled)),       NA_real_),
    last_enrolled   = ifelse(n_enrol > 0,  last(na.omit(enrolled)),       NA_real_),
    
    ssa = last(ssa),
    .groups = "drop"
  )

## Star ratings data

# Import data -------------------------------------------------------------

ma.path.a <- "/home/salizad/econ470/a0/work/ma-data/ma/star-ratings/Extracted Star Ratings/Part C 2014 Fall/2014_Part_C_Report_Card_Master_Table_2013_10_17_stars.csv"
star.data.a <- read_csv(
  ma.path.a,
  skip = 3,
  col_names = rating.vars.2014,
  na = c("", "NA", "*")
) %>%
  mutate(across(
    -any_of(c("contractid","org_type","contract_name","org_marketing","org_parent")),
    ~ parse_number(as.character(.))
  ))



ma.path.b <- "/home/salizad/econ470/a0/work/ma-data/ma/star-ratings/Extracted Star Ratings/Part C 2014 Fall/2014_Part_C_Report_Card_Master_Table_2013_10_17_summary.csv"
star.data.b <- fread(
  ma.path.b,
  skip = 2,
  stringsAsFactors=FALSE, 
  select=c(1:9),
  col.names=c("contractid","org_type","org_marketing","contract_name", "org_parent","snp","sanction", "partc_score","partcd_score"),
  na = c("", "NA", "*")
) %>%
  mutate(
    new_contract=ifelse(partc_score=="Plan too new to be measured",1, ifelse(partcd_score=="Plan too new to be measured",1,0)),
    partc_score  = ifelse(new_contract == 1, NA_real_, parse_number(as.character(partc_score))),
    partcd_score = ifelse(new_contract == 1, NA_real_, parse_number(as.character(partcd_score)))
  ) %>%
  select(contractid, new_contract, partc_score, partcd_score)

star.data.2014 <- star.data.a %>%
  select(-contract_name, -org_type, -org_marketing) %>%  
  left_join(star.data.b, by=c("contractid")) %>%
  mutate(year=2014)

## Benchmark data

bench.data <- read_csv("/home/salizad/econ470/a0/work/ma-data/ma/benchmarks/ratebook2014/CountyRate2014.csv",
                       skip=2,
                       col_names=c("ssa","state","county_name","risk_star5",
                                   "risk_star45","risk_star4","risk_star35",
                                   "risk_star3","risk_star25","esrd_ab"))

bench.data.2014 <- bench.data %>%
  select(ssa,risk_star5,risk_star45,risk_star4,risk_star35,risk_star3,risk_star25) %>%
  mutate(aged_parta=NA_real_, aged_partb=NA_real_, risk_ab=NA_real_,
         risk_bonus5=NA_real_, risk_bonus35=NA_real_, risk_bonus0=NA_real_,
         year=2014)

## Merge data

ma.2014 <- plan.data.2014 %>%
  inner_join(service.data.2014 %>% select(contractid, fips), by = c("contractid", "fips")) %>%
  filter(!state %in% c("VI", "PR", "MP", "GU", "AS", ""),
         snp == "No",
         (planid < 800 | planid >= 900),
         !is.na(planid), !is.na(fips)) %>%
  left_join(pen.2014 %>% ungroup() %>%
              rename(state_long = state, county_long = county) %>%
              mutate(state_long = str_to_lower(state_long)) %>%
              group_by(fips) %>%
              mutate(ncount = n()) %>%
              filter(ncount == 1), by = c("fips")) %>%
  left_join(star.data.2014 %>%
              select(-any_of(c("contract_name", "org_type", "org_marketing"))),
            by = c("contractid")) %>%
  mutate(
    Star_Rating = case_when(
      partd == "No" ~ partc_score,
      partd == "Yes" & is.na(partcd_score) ~ partc_score,
      partd == "Yes" & !is.na(partcd_score) ~ partcd_score
    )
  ) %>%
  # Convert ssa to character before joining
  mutate(ssa = as.character(ssa)) %>%
  left_join(bench.data.2014 %>% filter(!is.na(ssa)) %>%
              mutate(ssa = as.character(ssa)), by = c("ssa")) %>%
  select(-starts_with("year.")) %>%
  mutate(year = y) %>%
  mutate(
    ma_rate = case_when(
      year < 2012 ~ risk_ab,
      year >= 2012 & year < 2015 & Star_Rating == 5 ~ risk_star5,
      year >= 2012 & year < 2015 & Star_Rating == 4.5 ~ risk_star45,
      year >= 2012 & year < 2015 & Star_Rating == 4 ~ risk_star4,
      year >= 2012 & year < 2015 & Star_Rating == 3.5 ~ risk_star35,
      year >= 2012 & year < 2015 & Star_Rating == 3 ~ risk_star3,
      year >= 2012 & year < 2015 & Star_Rating < 3 ~ risk_star25,
      year >= 2012 & year < 2015 & is.na(Star_Rating) ~ risk_star35,
      year >= 2015 & Star_Rating >= 4 ~ risk_bonus5,
      year >= 2015 & Star_Rating < 4 ~ risk_bonus0,
      year >= 2015 & is.na(Star_Rating) ~ risk_bonus35
    )
  )


# Save to CSV
output_path <- "../data/output/data-2014.csv"

# Create parent directories if they don't exist
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

# Write CSV
write_csv(ma.2014, output_path)

# Confirmation message
cat("Data saved to", output_path, "\n")