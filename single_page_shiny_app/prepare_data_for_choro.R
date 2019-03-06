library(readxl)
library(readr)
library(dplyr)
library(curl)
library(tibble)

#####################
#### Import Data ####
#####################


import_data <- NULL
import_data <- import_data %>% 
  bind_rows(read_excel("~/Desktop/telliscope/Export and Import Data/import_2013.xlsx")) %>%
  bind_rows(read_excel("~/Desktop/telliscope/Export and Import Data/import_2014.xlsx")) %>% 
  bind_rows(read_excel("~/Desktop/telliscope/Export and Import Data/import_2015.xlsx")) %>% 
  bind_rows(read_excel("~/Desktop/telliscope/Export and Import Data/import_2016.xlsx")) %>% 
  bind_rows(read_excel("~/Desktop/telliscope/Export and Import Data/import_2017.xlsx")) %>%
  add_column(trade_direction = "import")

export_data <- NULL
export_data <- export_data %>%
  bind_rows(read_excel("~/Desktop/telliscope/Export and Import Data/export_2013.xls")) %>%
  bind_rows(read_excel("~/Desktop/telliscope/Export and Import Data/export_2014.xlsx")) %>%
  bind_rows(read_excel("~/Desktop/telliscope/Export and Import Data/export_2015.xls")) %>%
  bind_rows(read_excel("~/Desktop/telliscope/Export and Import Data/export_2016.xlsx")) %>%
  bind_rows(read_excel("~/Desktop/telliscope/Export and Import Data/export_2017.xlsx")) %>%
  add_column(trade_direction = "export")


#import mapping btwn country names and ISO-3 compliant codes
iso_country_codes <- read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv",
                          col_types = cols_only(`alpha-3` = col_guess(),
                                                name = col_guess())) %>% rename(country_name = name, country_code = `alpha-3`)

#quick fix for Cote d'Ivoire since I can't figure out how to store "ô" in a tibble

for (i in 1:nrow(iso_country_codes)){
  if (grepl("Ivoire",iso_country_codes$country_name[i])){
    iso_country_codes$country_name[i] = "Cote d'Ivoire"
  }
}

###################
#### Functions ####
###################


#### Step 1: fix column names ####
###########################################################

rename_columns <- function(tbl){
  #This function changes the names of columns in the export and import data so they can be put into the
  #same tibble
  this_trade_direction <- unique(tbl$trade_direction)
  stopifnot(length(this_trade_direction) == 1)
  
  if (this_trade_direction == "export"){
    tbl <- tbl %>% rename(country_name = Destination, 
                          `Net Wt. (Kg)` = `Net.Wt. (Kg)`,
                          dollar_amount = `FOB Value (USD)`,
                          birr_amount = `FOB Value (ETB)`)
  }
  
  if (this_trade_direction == "import"){
    tbl <- tbl %>% rename(country_name = `Country (Origin)`,
                          dollar_amount = `CIF Value (USD)`,
                          birr_amount = `CIF Value (ETB)`)
    tbl <- tbl %>% select(-`Country (Consignment)`)
  }
  
  return(tbl)
} 

####Step 2: Fix faulty country names ####
#########################################

fix_country_name <- function(country_name){
  #INPUT: a country_name string
  #OUTPUT: a country_name compatible with ISO-3 country names and codes

  if (country_name == "United States"){return("United States of America")}
  if (country_name == "Korea, Republic of"){return("Korea (Republic of)")}
  if (country_name == "Democratic republic of the Congo"){return ("Congo (Democratic Republic of the)")}
  if (country_name == "Iran, Islamic Republic of"){return("Iran (Islamic Republic of)")}
  if (country_name == "United Kingdom"){return("United Kingdom of Great Britain and Northern Ireland")}
  if (country_name == "Czech Republic"){return("Czechia")}
  if (country_name == "Libyan Arab Jamahiriya"){return("Libya")}
  if (country_name == "Korea, Democratic People's Rep. of"){return("Korea (Democratic People's Republic of)")}
  if (country_name == "Swaziland"){return("Eswatini")}
  if (country_name == "United Republic of Tanzania"){return("Tanzania, United Republic of")}
  if (country_name == "Kazakstan"){return("Kazakhstan")}
  if (country_name == "Venezuela"){return("Venezuela (Bolivarian Republic of)")}
  if (country_name == "Palestine"){return("Palestine, State of")}
  if (country_name == "United States Virgin Islands"){return("Virgin Islands (U.S.)")}
  if (country_name == "Bolivia"){return("Bolivia (Plurinational State of)")}
  if (country_name == "Cape Verde"){return("Cabo Verde")}
  if (country_name == "Christmas Island[Australia]"){return("Christmas Island")}
  
  if (country_name == "Republic of Moldova"){return("Moldova (Republic of)")}
  if (country_name == "Reunion"){return("Réunion")}
  if (country_name == "Serbia and Montenegro"){return("Serbia")} #yikes I just applied s and m to just s
  if (country_name == "Yugoslavia"){return("Macedonia (the former Yugoslav Republic of)")}
  if (country_name == "Zaire"){return("Congo (Democratic Republic of the)")}
  if (country_name == "East Timor"){return("Timor-Leste")}
  if (country_name == "Holy See (Vatican)"){return("Holy See")}
  if (country_name == "The former Yugoslav Rep. Macedonia"){return("Macedonia (the former Yugoslav Republic of)")}
  if (country_name == "Netherlands Antilles"){return("Netherlands")}
  return(country_name)
}

fix_country_names <- function(tbl, country_col = "country_name"){
  
  tbl <- tbl %>% filter(!is.na(country_name))
  
  fixed_country_names <- sapply(tbl[country_col][[1]], fix_country_name)

  tbl[country_col] <- fixed_country_names

  return(tbl)
}


#### Step 3: Summarise trade data ####
######################################

get_yearly_values_per_country <- function(tbl){
  #INPUT: a tibble with individual trades as rows w. Year, 
  #OUTPUT: a tibble with countries as rows and their summed total trade amounts
  summarised_tbl <- tbl %>% 
    group_by(Year, country_name, trade_direction) %>% 
    summarise(total_trade = sum(dollar_amount))
  
  print(paste("TOTAL COuntryies: ", nrow(summarised_tbl)))
  return (summarised_tbl)
}



#### Step 4: Add countries with no yearly trade ####
####################################################

add_missing_countries <- function(tbl){
  #INPUT: tibble w/ a Year and country_code ccolumn
  #OUTPUT: tibble w/ a row for each missing country.
  print(paste("STARTING SIZE:", nrow(tbl)))
  tbl_with_missing_countries_added <- NULL #start w/empty and add empty countries every year
  for (y in unique(tbl$Year)){
    
    tbl_1_year <- tbl %>% filter(Year == y) #get tbl for this year
    
    print(paste(y, "SIZE:", (unique(tbl_1_year$Year))))
    
    missing_countries <- 
      iso_country_codes[
        which(iso_country_codes$country_name %in% setdiff(iso_country_codes$country_name,tbl_1_year$country_name)),] %>% 
      add_column(total_trade = 0, Year = unique(tbl_1_year$Year), trade_direction = unique(tbl_1_year$trade_direction))
    
    tbl_with_missing_countries_added <- tbl_with_missing_countries_added %>% bind_rows(tbl_1_year,missing_countries)
  }
  
  tbl_with_missing_countries_added
}

#### Step 5: Add country codes ####
###################################

add_country_codes <- function(tbl){
  #INPUT: a tibble with a column country_col
  #OUTPUT: a tibble with a new column called country_code
  country_code <- sapply(tbl$country_name, 
                         function(name){
    (iso_country_codes %>%filter(country_name == name) %>% pull(country_code))[1]})
  
  tbl["country_code"] = country_code
  return(tbl)
}

################
#### Script ####
################

process_trade_data <- function(trade_tbl){
  #INPUT: export or import trade data
  #OUPUT: processed data ready for choropleth went thru all above steps
  
  trade_tbl <- trade_tbl %>% 
    rename_columns %>% 
    fix_country_names %>% 
    get_yearly_values_per_country %>%
    add_missing_countries %>% 
    add_country_codes
  
  trade_tbl
}

trade_tbl <- bind_rows(process_trade_data(import_data), process_trade_data(export_data))

write_csv(trade_tbl, "~/Desktop/telliscope/import_export_clean.csv")
