library(dplyr)
library(lookupr)
library(readr)
library(stringr)
library(survey)

# Export 34 from IPUMS
# "Method of travel to work, wages, occupation, class of worker, usual hours worked, and state/county fips (2005-2015) w/ CSV"

# Read IPUMS export
ipums_orig <- read_csv("data/usa_00034.csv", col_types="icciiiiidi")

# Filter to only full-time wageworkers (>= 35 hours per week, with wages, and not self-employed)
ipums <- ipums_orig %>%
  filter(INCWAGE > 0, INCWAGE < 999998, CLASSWKR == 2, UHRSWORK >= 35)

# Convert years to a factor
ipums$year <- as.factor(ipums$YEAR)

# Convert TRANWORK
ipums$commute <- NA

ipums$commute[ipums$TRANWORK >= 10 & ipums$TRANWORK <= 20] <- "Private vehicle"
ipums$commute[ipums$TRANWORK >= 30 & ipums$TRANWORK <= 36] <- "Public transit"
ipums$commute[ipums$TRANWORK == 40] <- "Bicycle"
ipums$commute[ipums$TRANWORK == 50] <- "Walked"
ipums$commute[ipums$TRANWORK == 60] <- "Other"
ipums$commute[ipums$TRANWORK == 70] <- "Worked at home"

ipums$commute <- factor(ipums$commute, level=c("Private vehicle", "Public transit", "Bicycle", "Walked", "Other", "Worked at home"))

# Recode OCC2010 to match ATUS OCC2 coding
ipums$jobs <- NA

ipums$jobs[ipums$OCC2010 >= 10 & ipums$OCC2010 <= 430] <- "Management occupations"
ipums$jobs[ipums$OCC2010 >= 500 & ipums$OCC2010 <= 950] <- "Business and financial operations occupations"
ipums$jobs[ipums$OCC2010 >= 1000 & ipums$OCC2010 <= 1240] <- "Computer and mathematical science occupations"
ipums$jobs[ipums$OCC2010 >= 1300 & ipums$OCC2010 <= 1560] <- "Architecture and engineering occupations"
ipums$jobs[ipums$OCC2010 >= 1600 & ipums$OCC2010 <= 1980] <- "Life, physical, and social science occupations"
ipums$jobs[ipums$OCC2010 >= 2000 & ipums$OCC2010 <= 2060] <- "Community and social service occupations"
ipums$jobs[ipums$OCC2010 >= 2100 & ipums$OCC2010 <= 2150] <- "Legal occupations"
ipums$jobs[ipums$OCC2010 >= 2200 & ipums$OCC2010 <= 2550] <- "Education, training, and library occupations"
ipums$jobs[ipums$OCC2010 >= 2600 & ipums$OCC2010 <= 2920] <- "Arts, design, entertainment, sports, and media occupations"
ipums$jobs[ipums$OCC2010 >= 3000 & ipums$OCC2010 <= 3540] <- "Healthcare practitioner and technical occupations"
ipums$jobs[ipums$OCC2010 >= 3600 & ipums$OCC2010 <= 3650] <- "Healthcare support occupations"
ipums$jobs[ipums$OCC2010 >= 3700 & ipums$OCC2010 <= 3950] <- "Protective service occupations"
ipums$jobs[ipums$OCC2010 >= 4000 & ipums$OCC2010 <= 4150] <- "Food preparation and serving related occupations"
ipums$jobs[ipums$OCC2010 >= 4200 & ipums$OCC2010 <= 4250] <- "Building and grounds cleaning and maintenance occupations"
ipums$jobs[ipums$OCC2010 >= 4300 & ipums$OCC2010 <= 4650] <- "Personal care and service occupations"
ipums$jobs[ipums$OCC2010 >= 4700 & ipums$OCC2010 <= 4965] <- "Sales and related occupations"
ipums$jobs[ipums$OCC2010 >= 5000 & ipums$OCC2010 <= 5940] <- "Office and administrative support occupations"
ipums$jobs[ipums$OCC2010 >= 6005 & ipums$OCC2010 <= 6130] <- "Farming, fishing, and forestry occupations"
ipums$jobs[ipums$OCC2010 >= 6200 & ipums$OCC2010 <= 6940] <- "Construction and extraction occupations"
ipums$jobs[ipums$OCC2010 >= 7000 & ipums$OCC2010 <= 7630] <- "Installation, maintenance, and repair occupations"
ipums$jobs[ipums$OCC2010 >= 7700 & ipums$OCC2010 <= 8965] <- "Production occupations"
ipums$jobs[ipums$OCC2010 >= 9000 & ipums$OCC2010 <= 9750] <- "Transportation and material moving occupations"

ipums$jobs <- as.factor(ipums$jobs)

# Add county FIPS codes
ipums$fips <- paste0(
  str_pad(ipums$STATEFIP, 2, pad = "0"),
  str_pad(ipums$COUNTYFIPS, 3, pad = "0")
)

# Add NCHS urban/rural classes
nchs <- from_lookup("fips", "nchs", version = "2013")

ipums <- merge(ipums, nchs, by = "fips", all.x = TRUE)

# Convert nchs codes
ipums$nchs[ipums$nchs == 1] <- "Large central metro"
ipums$nchs[ipums$nchs == 2] <- "Large fringe metro"
ipums$nchs[ipums$nchs == 3] <- "Medium metro"
ipums$nchs[ipums$nchs == 4] <- "Small metro"
ipums$nchs[ipums$nchs == 5] <- "Micropolitan"

# This is actually a list of rural counties that the census doesn't identify
# See: https://usa.ipums.org/usa-action/variables/COUNTYFIPS#codes_section
ipums$nchs[is.na(ipums$nchs)] <- "Non-core"

ipums$nchs <- factor(ipums$nchs, level=c("Large central metro", "Large fringe metro", "Medium metro", "Small metro", "Micropolitan", "Non-core"))

# Add "1" for survey sums
ipums$one <- 1

# Construct the simplified survey design
design = svydesign(
  ids = ~ 0,
  data = ipums,
  weights = ~ PERWT
)

nchs_totals <- svyby(~ one, ~ YEAR + nchs + commute, design, svytotal, multicore = TRUE)

nchs_share_of_commuters <- nchs_totals %>%
  rename(pop = one) %>%
  group_by(YEAR, nchs) %>%
  mutate(
    moe = (se * 1.96) / pop,
    share = pop / sum(pop)
  ) %>%
  ungroup()

write_csv(nchs_share_of_commuters, "results/nchs_share_of_commuters.csv")

programmers <- ipums %>%
  filter(jobs == "Computer and mathematical science occupations")

write_csv(nchs_share_of_commuters, "results/nchs_share_of_commuters.csv")

# Construct the simplified survey design
design = svydesign(
  ids = ~ 0,
  data = programmers,
  weights = ~ PERWT
)

programmers_nchs_totals <- svyby(~ one, ~ YEAR + nchs + commute, design, svytotal, multicore = TRUE)

programmers_nchs_totals$nchs_simple[programmers_nchs_totals$nchs == "Large central metro"] <- "Large metro"
programmers_nchs_totals$nchs_simple[programmers_nchs_totals$nchs == "Large fringe metro"] <- "Large metro"
programmers_nchs_totals$nchs_simple[programmers_nchs_totals$nchs == "Medium metro"] <- "Large metro"
programmers_nchs_totals$nchs_simple[programmers_nchs_totals$nchs == "Small metro"] <- "Small/non metro"
programmers_nchs_totals$nchs_simple[programmers_nchs_totals$nchs == "Micropolitan"] <- "Small/non metro"
programmers_nchs_totals$nchs_simple[programmers_nchs_totals$nchs == "Non-core"] <- "Small/non metro"
programmers_nchs_totals$nchs_simple <- factor(programmers_nchs_totals$nchs_simple)


programmers_nchs_share_of_commuters <- programmers_nchs_totals %>%
  rename(pop = one) %>%
  group_by(YEAR) %>%
  mutate(
    moe = (se * 1.96) / pop,
    share = pop / sum(pop)
  ) %>%
  ungroup()
  
write_csv(programmers_nchs_share_of_commuters, "results/programmers_nchs_share_of_commuters.csv")


