# Using IPUMS "ind" codes, assign BLS industry groups
ind_to_bls <- function(.ind) {
  dplyr::case_when(
    # We don't have data on argiculture, so will use
    dplyr::between(.ind, 0170, 0490) ~ "15000000", # "Mining, Logging and Construction"
    .ind == 770                      ~ "20000000", # "Construction"
    dplyr::between(.ind, 1070, 3990) ~ "30000000", # "Manufacturing"
    dplyr::between(.ind, 4070, 4590) ~ "40000000", # "Trade, Transportation, and Utilities"
    dplyr::between(.ind, 4670, 5790) ~ "40000000", # "Trade, Transportation, and Utilities"
    dplyr::between(.ind, 6070, 6390) ~ "40000000", # "Trade, Transportation, and Utilities"
    dplyr::between(.ind, 0570, 0690) ~ "40000000", # "Trade, Transportation, and Utilities"
    dplyr::between(.ind, 6470, 6780) ~ "50000000", # "Information" 
    dplyr::between(.ind, 6870, 7190) ~ "55000000", # "Financial Activities"
    dplyr::between(.ind, 7270, 7790) ~ "60000000", # "Professional and Business Services"
    dplyr::between(.ind, 7860, 8470) ~ "65000000", # "Education and Health Services"
    dplyr::between(.ind, 8560, 8690) ~ "70000000", # "Leisure and Hospitality"
    dplyr::between(.ind, 8770, 9290) ~ "80000000", # "Other Services"
    dplyr::between(.ind, 9370, 9590) ~ "90000000"  # "Government"
    # No data on military (9670-9870)
  )
}