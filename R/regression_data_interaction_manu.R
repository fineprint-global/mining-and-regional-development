
load("input/full_panel.RData")

# neighbours --------------------------------------------------------------

# reason for excluding PER.7_1 and ER.15_1 see below

W_data <- spdf %>% 
  dplyr::filter(NAME_0 %in% c_vector1)

# neighbours (see https://cran.r-project.org/web/packages/spdep/vignettes/nb_sf.html)
coords_sf <-  sf::st_coordinates(sf::st_centroid(W_data) %>% dplyr::filter(!GID_1 %in% c("PER.7_1", "PER.15_1")))
W_k <- spdep::knearneigh(coords_sf, k = k_nn)
knear_nb <- spdep::knn2nb(W_k)
W_k <- spdep::nb2mat(knear_nb)

# structure of W must fit Y and X
W_str <- sf::st_centroid(W_data) %>% dplyr::filter(!GID_1 %in% c("PER.7_1", "PER.15_1")) %>%
  sf::st_set_geometry(NULL)

# subset panel ------------------------------------------------------------

data <- spdf_panel %>% 
  dplyr::filter(year %in% t_vector) %>%
  dplyr::filter(substr(GID_1, 1, 3) %in% c_vector2)

# # check regions
# unique(data$GID_1)
# test <- data %>%
#   `colnames<-`(gsub("-.*", "", colnames(data))) %>%
#   sf::st_set_geometry(NULL) %>%
#   dplyr::select("GID_1", "year", "GDP", "ore_extraction", "POP_DEN", "GVA_IND_10_VC", "GVA_IND_10_VK", "large_port") %>%
#   dplyr::filter(GID_1 %in% c("PER.16_1", "PER.15_1", "PER.7_1"))
# conclusio:
# PER.7_1 (Callao) can be excluded, since part of Lima and no individual data available
# PER.15_1 (Lima Province) can be excluded since data equals PER.16_1 (Lima), except extraction, which takes only place in PER.16_1

# select variables
X <- data %>%
  `colnames<-`(gsub("-.*", "", colnames(data))) %>%
  sf::st_set_geometry(NULL) %>% # drop geometry
  dplyr::select("GID_1", "year", "GDP", "ore_extraction", "POP_DEN", "GVA_IND_10_VC", "GVA_IND_10_VK", "large_port", "T") %>%
  dplyr::mutate(GVA_IND_10_VC = log(GVA_IND_10_VC)) %>%
  dplyr::mutate(GVA_IND_10_VK = log(GVA_IND_10_VK)) %>%
  dplyr::mutate(POP_DEN = log(POP_DEN)) %>%
  dplyr::mutate(ore_extraction = log(ore_extraction+1)) %>%
  replace(is.na(.), 0) %>%
  dplyr::select(-`T`)

# Dealing with NAs by setting = 0 is okay, since extraction is NA of there is none, 
# and other NAs drop later because it is 2014 and 2015 (Peru) data

# calculate growth (in %)
Y <- X %>%
  dplyr::group_by(GID_1) %>%
  dplyr::arrange(GID_1, year) %>%
  dplyr::mutate(g = log(GDP / lag(GDP,g_horizon)) / g_horizon * 100) %>%
  dplyr::mutate(g = lead(g, g_horizon)) %>%
  dplyr::select(GID_1, year, g) %>%
  dplyr::filter(! year %in% drop_horizon) %>%
  dplyr::filter(!GID_1 %in% c("PER.7_1", "PER.15_1")) %>%
  dplyr::arrange(year, factor(GID_1, levels = W_str$GID_1))

# calculate log initial income
vars <- c("int", "GDP", colnames(X)[! colnames(X) %in% "GDP"])
X <- X %>% 
  dplyr::mutate(GDP = log(GDP)) %>%
  dplyr::arrange(year, factor(GID_1, levels = W_str$GID_1)) %>% 
  dplyr::mutate(int = rep(1, nrow(X))) %>%
  dplyr::select(vars) %>%
  dplyr::filter(! year %in% drop_horizon) %>% 
  dplyr::filter(!GID_1 %in% c("PER.7_1", "PER.15_1"))
coefs <- vars[! vars %in% c("GID_1", "year")]

# interaction dummy ore extraction and country
X <- X %>%
  dplyr::mutate(ore_CHL = ifelse(substr(GID_1, 1, 3) == "CHL", ore_extraction ,0)) %>%
  dplyr::mutate(ore_MEX = ifelse(substr(GID_1, 1, 3) == "MEX", ore_extraction ,0)) %>%
  dplyr::mutate(ore_PER = ifelse(substr(GID_1, 1, 3) == "PER", ore_extraction ,0)) %>%
  dplyr::select(-ore_extraction)


# year dummies
D <- dummies::dummy(X$year, sep = "_")[,-1]

# country dummies
C <- X %>%
  dplyr::mutate(peru = ifelse(substr(GID_1, 1, 3) == "PER", 1, 0)) %>%
  dplyr::mutate(chile = ifelse(substr(GID_1, 1, 3) == "CHL", 1, 0)) %>%
  dplyr::select(peru, chile)

# remove year and transform to numeric matrices
Y <- as.numeric(Y$g)
X <- X %>% dplyr::select(-GID_1, -year)
X <- matrix(as.numeric(unlist(X)),nrow=nrow(X))
C <- matrix(as.numeric(unlist(C)),nrow=nrow(C))


