# Optimizing the calculation of H Indices
# Nathan Quan
# March 28, 2021




msa_2017 <- readxl::read_excel("data/raw/CBSA_Aug_2017.xls")




## Cleaning MSA data----
str(msa_2017)

msa_2017 <- msa_2017 %>%
  head(-4)

msa_2017$FIPS64 <- str_c(msa_2017$`FIPS State Code`, msa_2017$`FIPS County Code`)

msa_2017 <- msa_2017 %>%
  filter(!is.na(`CBSA Title`)) %>%
  filter(`Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area") %>%
  select(`CBSA Code`, `CBSA Title`, `Metropolitan/Micropolitan Statistical Area`, `County/County Equivalent`, FIPS64, `FIPS State Code`, `FIPS County Code`)


## Load ACS variables----

vv <- tidycensus::load_variables(2017, 'acs5', cache = T)

###Define 5-year ACS variables of interest

## INCOME VARIABLE

# Define income vars
vars_inc <- c(
  
  ## TABLE B19001: Household Income categories - TOTAL POP
  "B19001_002", # Income <10k
  "B19001_003", # Income 10-15k
  "B19001_004", # Income 15-20k
  "B19001_005", # Income 20-25k
  "B19001_006", # Income 25-30k
  "B19001_007", # Income 30-35k
  "B19001_008", # Income 35-40k
  "B19001_009", # Income 40-45k
  "B19001_010", # Income 45-50k
  "B19001_011", # Income 50-60k
  "B19001_012", # Income 60-75k
  "B19001_013", # Income 75-100k
  "B19001_014", # Income 100-125k
  "B19001_015", # Income 125-150k
  "B19001_016", # Income 150-200k
  "B19001_017" # Income 200+
  
)



## Create a List of MSA objects----

MSA_lst <- lst() #create empty list

  
  for (cbsa_code in msa_2017$`CBSA Code`) {
    MSA_lst[[cbsa_code]] <- msa_2017 %>%
      filter(`CBSA Code` %in% cbsa_code)
  }

# Check
length(unique(as.numeric(msa_2017$`CBSA Code`)))



## Acquire Income Data----
cbsaNames <- names(MSA_lst) # create vector of CBSA names from MSA_lst

# loop through cbsaNames to retain CBSA code names in new list of income data
acs_inc <- lst() # create empty list


  for(name in cbsaNames){
    ## Obtain income data
    acs_inc[[name]] <- tidycensus::get_acs(geography = 'tract',
                                           state = MSA_lst[[name]]$`FIPS State Code`,
                                           county = MSA_lst[[name]]$`FIPS County Code`,
                                           variables = vars_inc,
                                           year = 2017,
                                           survey = 'acs5',
                                           geometry = F,
                                           key = keyring::key_get("U.S. Census API"))
    
    Sys.sleep(5) # Trying to get API call not to fail by building in 5-second pause
    
  }



### PREP DATA FOR RANKORDERSEG CALCULATION----

## rankorderseg() from OasisR package requires an object of class matrix
## where each column represents the distribution fo a group within spatial units
## i.e. columns = income categories; rows = tracts

## within the acs_inc list, each cbsa object has columns, GEOID, name, variable (income category), estimate, moe
## want to have columns be the income categories and the rows be the tracts


# loop through cbsaNames to retain CBSA code names in new list of income data in wide format
## Pivot so that columns = income categories; rows = tracts
acs_inc_wide <- lst() # create empty list
for(name in cbsaNames){
  acs_inc_wide[[name]] <- acs_inc[[name]] %>%
    select(-moe, -NAME) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    select(-GEOID)
  
  ## Removing duplicate income values due to collapsing tracts from pivot_wider
  for (incomecat in vars_inc) {
    
    for (i in 1:nrow(acs_inc_wide[[name]])) {
      acs_inc_wide[[name]][[incomecat]][[i]] <- acs_inc_wide[[name]][[incomecat]][[i]][!duplicated(acs_inc_wide[[name]][[incomecat]][[i]])]
    }
    
  }
  
  
}



## Calculate rankorder H indices----


# loop through cbsaNames to retain CBSA code names in new list of calculated rankorder segregation indices
msa_h <- lst() # create empty list
for(name in cbsaNames){
  msa_h[[name]] <- matrix(unlist(acs_inc_wide[[name]]),
                          ncol=length(acs_inc_wide[[name]]),
                          byrow=F) %>%
    rankorderseg(polorder = 4, pred = c(0.10, 0.90))
}


save(msa_h, file = "msa_h.RData")





## Extract H index values----

# Instantiate vectors for Index, H10, H90 to place extracted values in
Index <- vector()
H10 <- vector()
H90 <- vector()

# Overall H Index
for(name in cbsaNames){
  Index[[name]] <- unlist(msa_h[[name]]$Hr$Index)
  H10[[name]] <- unlist(msa_h[[name]]$Hr$Predict$fit[[1]])
  H90[[name]] <- unlist(msa_h[[name]]$Hr$Predict$fit[[2]])
}

msa_h_final <- bind_cols(list(cbsaNames, Index, H10, H90))


names(msa_h_final) <- c("CBSA.Code", "H.Index", "H10", "H90")

msa_h_final$CBSA.Code <- str_replace(msa_h_final$CBSA.Code,"cbsa", "")

save(msa_h_final, file = "msa_h_final.RData")