


# spatial polygon data frames ---------------------------------------------

# Source: https://gadm.org/ 

path <- "input/gadm"

countries <- c("CHL", "PER", "MEX")
filenames <- paste0("gadm36_", countries, "_1_sp.rds")

for (i in 1:length(filenames)){
  spdf <- readRDS(file.path(path, filenames[i]))
  assign(paste0("spdf_", countries[i], "_sp"), spdf) 
  rm(spdf)
}

# as sf
spdf_MEX_sf <- sf::st_as_sf(spdf_MEX_sp)
spdf_PER_sf <- sf::st_as_sf(spdf_PER_sp)
spdf_CHL_sf <- sf::st_as_sf(spdf_CHL_sp)
spdf <- rbind(spdf_MEX_sf, spdf_PER_sf, spdf_CHL_sf)
rm(spdf_MEX_sp, spdf_PER_sp, spdf_CHL_sp, spdf_MEX_sf, spdf_PER_sf, spdf_CHL_sf)

# save spdf for panel extension
spdf_panel <- spdf


# ore extraction data -----------------------------------------------------

# Source: SNL Metals and Mining Database
# this data can only be provided at the aggregated regional level due to copyright restrictions

load("input/extraction_data.RData")

# select one year (cross-section)
snl_data_sf_agg <- snl_data_sf_agg_panel %>% dplyr::filter(year == 2010)

# merge aggregates of selected year into spdf (cross-section)
spdf <- spdf %>% dplyr::left_join(snl_data_sf_agg, by = "GID_1")

### extend to shp to panel and merge extraction data

# stack spdf data t (= 18 years) times
spdf_panel <- do.call("rbind", replicate(18, spdf_panel, simplify = FALSE))
spdf_panel <- spdf_panel %>% dplyr::mutate(year = rep(as.character(2000:2017), each=74))

# merge aggregates of selected year into spdf
spdf_panel <- spdf_panel %>% dplyr::left_join(snl_data_sf_agg_panel, by = c("GID_1", "year"))


# ports -------------------------------------------------------------------

# Source: https://www.searoutes.com/worldwide-ports 

ports <- read.csv("input/ports.csv", sep = ";")


# OECD --------------------------------------------------------------------

# region concordance and subset vector
dstruc <- OECD::get_data_structure("REGION_DEMOGR") 
reg <- dstruc$REG_ID
reg_ids <- c(paste0("ME0", c(1:9)), paste0("ME", c(10:32)), 
             paste0("PE0", c(1:9)), paste0("PE", c(10:25)), 
             paste0("CL0", c(1:9)), paste0("CL", c(10:15)))

# demography

if("oecd_demography.RData" %in% dir("input/oecd")){
  load("input/oecd/oecd_demography.RData")
} else{
  
  dstruc <- OECD::get_data_structure("REGION_DEMOGR") 
  var_desc <- dstruc$VAR_DESC # dstruc variable descriptions
  var <- dstruc$VAR # list of available variables
  filter_list <- list()
  filter_list[[1]] <- 2 # TL2
  filter_list[[2]] <- reg_ids # region
  filter_list[[3]] <- c("POP_DEN", # population density (pop. per km2)
                        "T") # population, all ages
  filter_list[[4]] <- "T" # sex (M+F)
  oecd_demography <- OECD::get_dataset("REGION_DEMOGR", filter = filter_list) %>% # download
    dplyr::select(-TL, -SEX, -POS) %>%
    dplyr::left_join(var, by = c("VAR" = "id"))
  save(oecd_demography, file = "input/oecd/oecd_demography.RData")
}

# economy

if("oecd_economy.RData" %in% dir("input/oecd")){
  load("input/oecd/oecd_economy.RData")
} else{
  
  dstruc <- OECD::get_data_structure("REGION_ECONOM") 
  var_desc <- dstruc$VAR_DESC
  var <- dstruc$VAR
  filter_list <- list()
  filter_list[[1]] <- 2 # TL2
  filter_list[[2]] <- reg_ids # region
  filter_list[[3]] <- "SNA_2008" # Last SNA classification (SNA 2008 or latest available)
  filter_list[[4]] <- c("GDP", # Regional GDP
                        "GVA_TOTAL", # Regional Gross Value Added, total activities
                        "GVA_IND_10_VA", # GVA in agriculture, forestry and fishing (ISIC rev4)
                        "GVA_IND_10_VB_E", # GVA in industry, including energy (ISIC rev4)
                        "GVA_IND_10_VC", # ..Of which: GVA in manufacturing (ISIC rev4)
                        "GVA_IND_10_VK") # GVA in financial and insurance activities (ISIC rev4)
  filter_list[[5]] <- "USD_PPP" # measured in Millions USD, current prices, current PPP
  filter_list[[6]] <- "ALL" # All regions
  oecd_economy <- OECD::get_dataset("REGION_ECONOM", filter = filter_list) %>% # download
    dplyr::select(-TL, -POS) %>%
    dplyr::left_join(var, by = c("VAR" = "id"))
  save(oecd_economy, file = "input/oecd/oecd_economy.RData")
}

suppressWarnings(rm(dstruc, filter_list, reg, var, var_desc))

# merge data --------------------------------------------------------------

oecd_data <- dplyr::bind_rows(oecd_demography, oecd_economy)
suppressWarnings(rm(oecd_demography, oecd_economy))

# merge to spatial data ---------------------------------------------------

# load and apply concordance
conc <- read.csv("input/concordance_regions.csv", sep = ";", stringsAsFactors = FALSE)
oecd_data <- oecd_data %>% dplyr::left_join(conc, by = c("REG_ID" = "oecd_id"))

# merge into spdf_panel (takes time!!)
if("panel.RData" %in% dir("input")){
  load("input/panel.RData")
} else{
  spdf_panel <- spdf_panel %>%
    dplyr::left_join(oecd_data , by = c("GID_1" = "gadm_id", "year" = "TIME")) %>%
    dplyr::select(GID_1, year, ore_extraction, VAR, UNIT, obsValue) %>%
    tidyr::unite("VAR", c("VAR","UNIT"), sep ="-")
  
  spdf_panel <- tidyr::spread(spdf_panel, VAR, obsValue, drop = TRUE)
  save(spdf_panel, file = "input/panel.RData")
}

# merge ports into data ---------------------------------------------------

# panel
spdf_panel <- spdf_panel %>% dplyr::left_join(ports %>% dplyr::select(GID_1, large_port), by = "GID_1")

# save full spatial data --------------------------------------------------

save(spdf_panel, file = "input/full_panel.RData")
