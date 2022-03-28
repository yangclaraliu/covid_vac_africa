#---------------------------------------------------------------------------------------------------
# FUNCTION: cov_VSL - returns country-specific VSL estimates
#
# Calculation according to base case recommendation in BCA reference case
# https://sites.sph.harvard.edu/bcaguidelines/
#
# "VSL extrapolated from a U.S. estimate to the target country using an income elasticity of
# 1.5. The starting point should be VSL-to-GNI per capita ratio of 160, based on a U.S. VSL
# of $9.4 million and U.S. GNI per capita of $57,900. If this approach yields a target
# country value of less than 20 times GNI per capita, then 20 times GNI per capita should
# be used instead given the expectation that VSL will exceed likely future income."
#
#---------------------------------------------------------------------------------------------------

cov_VSL <- function(
  GNIPC        # table of country-specific GNI per capita
){
  out <- as.data.table(GNIPC)[
    ,
    vsl := 9.4E6 * (GNIPC_2020_USD / 57900)^1.5
  ][
    ,
    vsl := ifelse(vsl < 20 * GNIPC_2020_USD, 20 * GNIPC_2020_USD, vsl)
  ][
    ,
    GNIPC_2020_USD := NULL
  ]
  
  return(out)
}

#---------------------------------------------------------------------------------------------------
# FUNCTION: cov_dLE - calculates discounted life expectancy for each of the
# CovidM age bands (equivalent to YLL per death)
#---------------------------------------------------------------------------------------------------

# qx = probability of dying between age x and x+1
# lx = number of 100,000 reference pop surviving to age x
# dx = instantaneous death rate = -ln{1-qx}
# Lx = person years lived between age x and x+1 = [l(x) + l(x+1)]/2 
# Tx = total person years lived above age x
# dLEx = discounted life expectancy at age x

cov_dLE <- function(
  LT,                            # data table with UN life tables,
  r               = 0,           # discount rate
  smr             = 1,           # SMRs adjustment for covid co-morbidities
  selectCountries = c("GBR"),    # vector of iso3 codes of countries to run
  selectTime      = "2020-2025", # which UN life-table time-period to use 
  selectSex       = "Total",     # which UN life-table sex to use
  weight_method   = "pop_ifr",   # weight method to average LE by age group: "lx" "lxqx" "equal" "pop_ifr"
  POP             = NULL         # data table of populations to be supplied if using weight_method=="pop_ifr"
){
  
  require(data.table)
  require(ISOcodes)
  
  # Covid age-specific IFR
  ifr_levin = function(age) exp(-7.56 + 0.121 * age) / (100 + exp(-7.56 + 0.121 * age))
  
  # age bands for output to match CovidM
  AgeBands = data.table(
    AgeBand = seq(1,16,1),
    start   = seq(0,75,5),
    end     = c(seq(4,74,5),100)
  )
  
  
  # UN M49 country code to ISO3 mapping
  UN_M.49_Countries <- as.data.table(UN_M.49_Countries)[, LocID := as.integer(Code)]
  
  LT <- LT[     # Convert UN M49 country code to ISO3 (also drops regions)           
    UN_M.49_Countries, 
    on = .(LocID == LocID)
  ][            # only life tables for selected time period, sex, and countries
    Time == selectTime & Sex == selectSex & ISO_Alpha_3 %in% selectCountries    
  ][
    ,
    .(         # keep only selected vars
      country = ISO_Alpha_3,
      AgeGrpStart,
      AgeGrpSpan,
      qx
    )
  ][            # Add AgeGrpEnd
    , AgeGrpEnd := AgeGrpStart + AgeGrpSpan
  ][AgeGrpStart == 100, AgeGrpEnd := 101]
  
  # expand age groups from 0 to maximum age
  out <- data.table(Age = seq(0, max(LT$AgeGrpStart)))  
  
  out <- LT[
    out, 
    on=.(AgeGrpStart <= Age, AgeGrpEnd > Age)
  ][
    order(country, AgeGrpStart)
  ][,AgeGrpEnd := NULL]
  
  setnames(out,"AgeGrpStart","Age")
  
  # convert age group qx to estimated yearly qx
  out[, qx := 1 - (1 - qx)^(1 / AgeGrpSpan)]
  
  # instantaneous death rate (Briggs et al)
  out[, dx := -log((1 - qx))]
  
  # lx
  out[Age == 0, lx := 100000] # starting age
  for (c in selectCountries){
    for (a in 1:max(out$Age)){
      out[
        country == c & Age == a,
        lx := out[country == c & Age == a - 1, lx] *        # alive at start of previous age group
          exp(-out[country == c & Age == a - 1, dx] * smr)  # deaths during previous age group
      ]
    }
  }
  
  # Lx
  for (c in selectCountries){
    for (a in 0:(max(out$Age) - 1)){
      out[
        country == c & Age == a,
        Lx :=  0.5 * (lx + out[country == c & Age == a + 1, lx])       
      ]
    }
  }
  out[Age == max(out$Age), Lx := lx] # final age
  
  # discounted Tx
  for (c in selectCountries){
    for (a in 0:max(out$Age)){
      out[
        country == c & Age == a,
        dTx := sum(out[country == c & Age >= a, Lx / (1 + r)^(Age - a)])
      ]
    }
  }
  
  # discounted LEx
  out[, dLEx := dTx/lx]
  
  # Age groups for output 
  out[, joinAge := Age]
  out <- out[AgeBands, on = .(joinAge >= start, joinAge <= end)]
  
  out <- out[Age != 100] # drop age 100
  
  # dLEx for age bands weighted by lx
  
  if (weight_method == "lx"){
    out <- out[
      ,
      .(
        disc.rate = r,
        SMR = smr,
        d_LEx = sum(lx * dLEx)/sum(lx)
      ),
      by = .(country, AgeBand)
    ][
      order(country, AgeBand)
    ]
  } else if (weight_method == "lxqx"){
    out <- out[
      , 
      .(
        disc.rate = r,
        SMR = smr,
        d_LEx = sum(lx * qx * dLEx)/sum(lx * qx)
      ),
      by = .(country, AgeBand)
    ][
      order(country, AgeBand)
    ]
  } else if (weight_method == "equal") {
    out <- out[
      , 
      .(
        disc.rate = r,
        SMR = smr,
        d_LEx = mean(dLEx)
      ),
      by=.(country, AgeBand)
    ][
      order(country, AgeBand)
    ]
  } else if (weight_method == "pop_ifr") {
    
    POP <- POP[     # Convert UN M49 country code to ISO3 (also drops regions)           
      UN_M.49_Countries, 
      on = .(LocID == LocID)
    ][
      , 
      country := ISO_Alpha_3
    ][ country %in% selectCountries & AgeGrp <= 99][
      , 
      IFR := ifr_levin(AgeGrp)
    ]
    
    out <- out[
      POP[, .(country = country, Age = AgeGrp, Pop = PopTotal, IFR = IFR)],
      on = .(Age == Age, country == country)
    ][
      order(country, AgeBand)
    ]
    
    out <- out[
      , 
      .(
        disc.rate = r,
        SMR = smr,
        d_LEx = sum(Pop * IFR * dLEx)/sum(Pop * IFR)
      ),
      by = .(country, AgeBand)
    ][
      order(country, AgeBand)
    ]
  } else {out <- "Error: Invalid argument for weight_method"}
  
  
  return(out[, c("disc.rate","SMR") := NULL]) # for now dropping discount rate & SMR from output as this is known implicitly
  
}

#---------------------------------------------------------------------------------------------------
# FUNCTION: cov_YLDs - calculates unit YLDs (per case / hospitalization) due to Covid morbidity
#---------------------------------------------------------------------------------------------------

cov_unit_ylds <- function (){
  
  weights <- list(
    acute        = 0.051,
    non_icu      = 0.133,
    icu          = 0.655,
    post_acute   = 0.219
  )
  
  unit_ylds <- list()
  
  # Acute episode: assume mean 5 days based on infectious duration in CovidM
  acute <- (5/365) * weights$acute 
  
  # For post-acute assume 20% of cases over 6 months
  # Sandmann et al. 16.8% reported physical symptoms at 6 months
  # Menges et al.   26% reporting symptoms at 6-8 months
  
  post_acute <- 0.2 * 183/365 * weights$post_acute
  
  # ylds per case
  unit_ylds$per_case <- acute + post_acute
  
  # ylds per hospitalised case 
  unit_ylds$per_non_icu_case <- (8/365)  * weights$non_icu # mean LOS 8 days from Davies et al.
  unit_ylds$per_icu_case     <- (10/365) * weights$icu     # mean LOS 10 days from Davies et al.
  
  return(unit_ylds)
  
} 


#---------------------------------------------------------------------------------------------------
# FUNCTION: cov_econ_outcomes 
# takes epi input data and calls other functions to get ylls, ylds, dalys, hc and vsl
#---------------------------------------------------------------------------------------------------

cov_econ_outcomes <- function(
  epi_deaths,   # data table of age-specific deaths
  epi_cases,    # data table of cases, non_icu and icu admission (not age-specific)
  econ_scens,   # econ scenarios specifying discount rate, smr
  LT,           # UNWPP life tables
  POP,          # UNWPP population estimates
  GDPPC,        # World Bank country GDP per capita 2020
  GNIPC         # World Bank country GNI per capita 2020
){
  
  require(data.table)
  
  # death related outcomes: YLLs, HC, and VSL
  
  dLE <- vector("list", length = nrow(econ_scens))
  for (s in 1:nrow(econ_scens)){
    # calculate discount life expectancy for given discount rate, smr and country
    dLE[[s]] <- cov_dLE(
      r   = econ_scens[[s,"discount_rate"]],
      smr = econ_scens[[s,"smr"]],
      selectCountries = unique(epi_deaths[,country]),
      LT = LT,
      POP = POP
    )
    dLE[[s]][, econ_id := s]
  }
  dLE <- rbindlist(dLE)
  
  # merge deaths and vsl data
  vsl <- cov_VSL(GNIPC = GNIPC)
  ylls <- vsl[  # merge deaths and vsl data
    epi_deaths, 
    on = .(country == country)
  ]
  
  # merge gdp per capita data
  ylls <- GDPPC[
    ylls,
    on = .(country == country)
  ]
  
  # merge dLE data and calculated ylls, vsl and hc 
  first_year <- 2021 # min(epi_deaths$year) # reference year for discounting
  ylls <- dLE[   
    ylls, 
    on = .(country = country, AgeBand = age), 
    allow.cartesian = TRUE
  ][               
    , 
    .(
      ylls = sum(deaths * d_LEx * 
                   1 / (1 + econ_scens[[s,"discount_rate"]])^(year - first_year)
      ),
      vsl = sum(deaths * vsl),
      human_capital = sum(d_LEx * GDPPC_2020_USD),
      GDPPC_2020_USD = mean(GDPPC_2020_USD)
    ),
    by = .(epi_id, econ_id, country) # collapse age bands
  ] 
  
  # ylds based on number of cases / hospitalizations
  unit_ylds <- cov_unit_ylds()
  first_year <- 2021
  ylds <- epi_cases[
    ,
    as.list(econ_scens[,c("econ_id","discount_rate")]), #  combine with different econ scenarios
    by=epi_cases
  ][
    ,
    .(
      ylds = sum(
        (
            (cases     * unit_ylds$per_case) +
            (non_icu  * unit_ylds$per_non_icu_case) +
            (icu      * unit_ylds$per_icu_case)
        ) * 1 / (1 + discount_rate)^(year - first_year) # discounting
      )
    ),
    by = .(epi_id, econ_id, country)
  ]
  
  # merge ylls with ylds and calculate dalys
  out <- ylds[
    ylls, 
    on = .(epi_id, econ_id, country)
  ][, dalys := ylls + ylds]
  
  return(out)
}

#---------------------------------------------------------------------------------------------------
# Use some fake data to test - TODO: agree format with Yang
#---------------------------------------------------------------------------------------------------

# ifr
ifr_levin = function(age) exp(-7.56 + 0.121 * age) / (100 + exp(-7.56 + 0.121 * age))

# econ scenarios
econ_scens <- as.data.table(cbind(
  econ_id       = c(1,2,3),
  discount_rate = c(0.03,0,0.03),
  smr           = c(1,1,1.5) 
))

# deaths
epi_deaths <- as.data.table(expand.grid(
  epi_id = c(1,2),
  country = c("ETH","COG","GHA","NGA","ZAF"),
  year = c(2020,2021),
  age  = seq(1,16,1)   #CovidM age groups
))
epi_deaths[, deaths := round(10^6 * ifr_levin(age * 5))]
epi_deaths <- epi_deaths[order(epi_id, country, age, year)]

# cases, non_icu, icu
epi_cases <- as.data.table(expand.grid(
  epi_id = c(1,2),
  country = c("ETH","COG","GHA","NGA","ZAF"),
  year = c(2020,2021)
))
epi_cases[, cases := 10^6]
epi_cases[, non_icu := round(cases * 0.1 * 0.7)]
epi_cases[, icu := round(cases * 0.1 * 0.3)]
epi_cases <- epi_cases[order(epi_id, country, year)]

# load UN population and life tables sourced from https://population.un.org/wpp/Download/Standard/CSV/
UNLT  <- fread(paste0(path_dropbox,
                      "WPP2019_Life_Table_Medium.csv"))[MidPeriod == 2023 & Sex == "Total"]  # only need combined sexes for 2025
UNPOP <- fread(paste0(path_dropbox,
                      "WPP2019_PopulationBySingleAgeSex_2020-2100.csv"))[Time == 2022]       # only need 2022 projections

# load world Bank GNI per capita current USD
GNIPC <- fread(
  paste0(path_dropbox,"API_NY.GNP.PCAP.CD_DS2_en_csv_v2_3732167.csv"), 
  skip = 4, 
  header = T
)[, .(country = `Country Code`, GNIPC_2020_USD = `2020`)] # select most recent (2020 values)

# load world Bank GNI per capita current USD
GDPPC <- fread(
  paste0(path_dropbox,"API_NY.GDP.PCAP.CD_DS2_en_csv_v2_3731360.csv"), 
  skip = 4, 
  header = T
)[, .(country = `Country Code`, GDPPC_2020_USD = `2020`)] # select most recent (2020 values)


# call function to calculate DALYs and VSL
results <- cov_econ_outcomes(
  epi_deaths = epi_deaths,
  epi_cases = epi_cases,
  econ_scens = econ_scens,
  LT = UNLT,
  POP = UNPOP,
  GDPPC = GDPPC,
  GNIPC = GNIPC
)

