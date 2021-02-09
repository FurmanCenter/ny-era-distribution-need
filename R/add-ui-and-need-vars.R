
# This function takes the dataset created in "prep-data.Rmd" with an
# additional variable called `risk_group` added that identifies an individual
# as being affected by job loss, and then using that variable in conjunction
# with the general information above to create new variables related to income
# loss, UI and stimulus benefits, and rental assistance need. In this analysis
# we assign the job loss status randomly with a likelihood based on the
# industry-level job loss rates.

# As noted in "analysis.Rmd", when discussing the unemployment insurance recipiency rate, not
# everyone who loses their job will receive UI benefits for a variety of
# reasons. We use a single value for the UI recipiency rate, and
# randomly assign those who lost their jobs to UI receipt with this likelihood
# (adjusting for those who are deemed ineligible due to wage calculations).


add_ui_takeup <- function(.data, .ui_takeup_rate) {
  .data <- .data %>% 
    mutate(
      # If a person has no wages they are excluded from the 
      risk_group = if_else(is.na(inc_wages), NA, risk_group),
      risk_group_lab = if_else(risk_group, "More vulnerable", "Less vulnerable"),
      risk_wages = if_else(risk_group, inc_wages, NA_real_),
    )
  
  # We have a UI_TAKEUP_RATE that we use to determine who gets UI benefits, but
  # that number includes people who are ineligible. So we need to determine the
  # % of people who lost jobs who are ineligible and subtract that from the
  # UI_TAKEUP_RATE used to determine UI receipt among the eligible population
  # who lost jobs.
  ui_ineligible_pct <- .data %>% 
    filter(risk_group) %>% 
    summarise(
      jobs_lost = sum(perwt),
      ui_ineligible = sum(perwt * (ui_benefits_month_reg == 0)),
      ui_ineligible_pct = ui_ineligible / jobs_lost
    ) %>% 
    pull(ui_ineligible_pct)
  
  UI_TAKEUP_RATE_ADJ <- .ui_takeup_rate /(1 - ui_ineligible_pct)
  
  .data %>% 
    mutate(
      # Set UI benefits to 0 if haven't lost job
      ui_benefits_month_reg = if_else(!risk_group | is.na(risk_group), 0, ui_benefits_month_reg),
      ui_benefits_month_extra600 = if_else(!risk_group | is.na(risk_group), 0, ui_benefits_month_extra600),
      ui_benefits_month_extra300 = if_else(!risk_group | is.na(risk_group), 0, ui_benefits_month_extra300),
      
      # First apply the random assignment using the adjusted UI takeup rate
      ui_takeup = runif(n()) < (UI_TAKEUP_RATE_ADJ),
      # Then set all the people that are ineligible as not getting UI
      ui_takeup = if_else(ui_benefits_month_reg == 0, FALSE, ui_takeup),
      
      # Now the final UI takeup rate among those who lost there job should now
      # reflect the UI_TAKEUP_RATE
      
      # For those who don't takeup UI, set benefits to 0
      ui_benefits_month_reg = if_else(ui_takeup, ui_benefits_month_reg, 0),
      ui_benefits_month_extra600 = if_else(ui_takeup, ui_benefits_month_extra600, 0),
      ui_benefits_month_extra300 = if_else(ui_takeup, ui_benefits_month_extra300, 0)
      
    )
}

# For this analysis rental assistance need is defined as the amount of money
# required to bring households back to the rent-to-income ratio that they had
# before loss of income due to job loss, capped at 30% for those households that
# were previously below that level, and capped at 100%% of their gross rent.


# This function takes a dataframe of the cleaned IPUMS data, after it has been
# run through the above function to add the UI takeup variables

add_need_vars <- function(.data) {
  .data %>% 
    # Group by household and categorize households based or occupations of members
    group_by(serial) %>%
    mutate(
      
      # Number of household members estimated to have lost their job
      hh_risk_members_num = sum(risk_group, na.rm = TRUE),
      
      # Number of wage earners in household
      hh_wage_earners_num = sum(inc_wages > 0, na.rm = TRUE),
      
      # Household with at least one wage earner in a more vulnerable occupation
      
      # If there are no members with wages then NA, if there are any at-risk
      # people with wages then TRUE, if there are people with wages but none of
      # them are at risk then FALSE
      hh_any_risk = case_when(
        all(is.na(risk_group)) ~ NA, # no wage earners
        any(risk_group, na.rm = TRUE) ~ TRUE, # any wage earners are at risk
        all(!risk_group, na.rm = TRUE) ~ FALSE # all wage earners are at NOT at risk
      ),
      
      # The total wages for each household that come from vulnerable occupations
      hh_risk_wages = sum(risk_wages, na.rm = TRUE),
      
      # The percent of household income that comes from wages from vulnerable occupations
      hh_risk_wages_pct = sum(risk_wages, na.rm = TRUE) / na_if(hh_inc_nom, 0),
      
      # Sum up UI benefits by household
      hh_ui_benefits_month_reg = sum(ui_benefits_month_reg),
      hh_ui_benefits_month_extra600 = sum(ui_benefits_month_extra600),
      hh_ui_benefits_month_extra300 = sum(ui_benefits_month_extra300),
      hh_ui_benefits_month_all600 = hh_ui_benefits_month_reg + hh_ui_benefits_month_extra600,
      hh_ui_benefits_month_all300 = hh_ui_benefits_month_reg + hh_ui_benefits_month_extra300,
      
    ) %>%
    ungroup() %>%
    mutate(
      
      risk_burden = gross_rent_nom / ((hh_inc_nom - hh_risk_wages) / 12),
      risk_burden_ui_reg = gross_rent_nom / (((hh_inc_nom - hh_risk_wages) / 12) + hh_ui_benefits_month_reg),
      risk_burden_ui_all600 = gross_rent_nom / (((hh_inc_nom - hh_risk_wages) / 12) + hh_ui_benefits_month_all600),
      risk_burden_ui_all300 = gross_rent_nom / (((hh_inc_nom - hh_risk_wages) / 12) + hh_ui_benefits_month_all300),
      
      risk_rent_need = ((gross_rent_nom/risk_burden) - (gross_rent_nom/target_burden)),
      risk_rent_need_ui_reg = ((gross_rent_nom/risk_burden_ui_reg) - (gross_rent_nom/target_burden)),
      risk_rent_need_ui_all600 = ((gross_rent_nom/risk_burden_ui_all600) - (gross_rent_nom/target_burden)),
      risk_rent_need_ui_all300 = ((gross_rent_nom/risk_burden_ui_all300) - (gross_rent_nom/target_burden)),
      
    ) %>%
    # Make some adjustments to all the rent_need columns
    mutate_at(
      vars(matches("risk_.*rent_need.*")),
      ~{
        # note that rent need is expressed as a negative number at first, after this it is positive
        case_when(
          is.na(.) ~ 0, # missing change to zero (missing if no cash rent, etc.)
          . > 0    ~ 0, # positive value for need means they don't have need, set to zero
          -. > gross_rent_nom ~ gross_rent_nom, # cap needs at the full rent value
          TRUE ~ -. # change the negative numbers to positive
        )
      }
    )
}
