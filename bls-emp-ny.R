library(tidyverse)
library(fs)
library(blsAPI) # remotes::install_github("mikeasilva/blsAPI")
library(jsonlite)

# https://www.bls.gov/sae/additional-resources/state-and-area-ces-series-code-structure-under-naics.htm

# We'll use the BLS API to get the employment data by industry. 
# They use "series" code to identify the data, and this code is built of a few parts so we can fill in those details for our desired regions and industry types. 

bls_ind <- tribble(
  ~supersector_industry, ~bls_ind_name,
  "15000000", "Mining, Logging and Construction",
  "20000000", "Construction",
  "30000000", "Manufacturing",
  "40000000", "Trade, Transportation, and Utilities",
  "50000000", "Information" ,
  "55000000", "Financial Activities",
  "60000000", "Professional and Business Services",
  "65000000", "Education and Health Services",
  "70000000", "Leisure and Hospitality",
  "80000000", "Other Services",
  "90000000", "Government"
)

# Create a dataframe with all of the components of the series codes we will need
# to download
bls_series_info <- tibble(
  prefix = "SM", # State/Metro Data
  sa = "U", # Not Seasonally Adjusted
  state = "36", # NY
  area = "00000", # all of NY
  data_type = "01", # All Employees, In Thousands,
) %>% 
  bind_cols(bls_ind) %>% 
  mutate(series_code = str_c(prefix, sa, state, area, supersector_industry, data_type))

# Structure the series codes into the format required by {blsAPI}
bls_api_info <- list(
  seriesid = pull(bls_series_info, series_code),
  startyear = "2019",
  endyear = "2020"
)

# Send the api request and parse the json response
bls_resp <- blsAPI(bls_api_info) %>% fromJSON()

# Take take all the individual datasets of results and join them back with their
# series code and stack the results, keeping only the columns we need
bls_emp_long <- map2_dfr(
  bls_resp$Results$series$seriesID,
  bls_resp$Results$series$data,
  ~{
    .y %>% 
      transmute(
        year,
        month = str_sub(period, -2),
        value = as.numeric(value)*1000
      ) %>% 
      mutate(series_code = .x) %>% 
      as_tibble()
  }
)

# Keep only the most recent month of data (July) and reshape the data so we have
# separate 2020 and 2019 columns with the total employment level, then join back
# with the full series code components, then calculate the percent job loss by
# industry
bls_emp_clean <-  bls_emp_long %>% 
  filter(month == "11") %>%  # pre-covid, and most recent month available
  unite(year_month, year, month) %>% 
  pivot_wider(values_from = value, names_from = year_month, names_prefix = "emp_") %>% 
  left_join(bls_series_info, by = "series_code") %>% 
  mutate(unemp_chg_pct = (emp_2020_11 - emp_2019_11) / emp_2019_11) %>% 
  select(
    series_code,
    prefix,
    sa,
    state,
    area,
    supersector_industry,
    data_type,
    bls_ind_name,
    emp_2019_11,
    emp_2020_11,
    unemp_chg_pct
  )

bls_emp_clean %>% 
  write_csv(path("data", "bls_emp_ny.csv")) %>% 
  write_rds(path("data", "bls_emp_ny.rds"))

