
t_vector <- c(2008:2015)
c_vector1 <- c("Mexico", "Peru", "Chile")
c_vector2 <- c("MEX", "PER", "CHL")
g_horizons <- c(2, 5)

# load data
load("input/full_panel.RData")

for (g_horizon in g_horizons){
drop_horizon <- c((max(t_vector)-g_horizon+1) : max(t_vector))

# prepare data
dat <- spdf_panel %>%
  sf::st_set_geometry(NULL) %>%
  dplyr::filter(year %in% t_vector, ore_extraction > 0) %>%
  dplyr::filter(!GID_1 %in% c("PER.7_1", "PER.15_1")) %>%
  dplyr::mutate(unid = paste(GID_1, year, sep = "_")) %>%
  dplyr::mutate(country = substr(GID_1, 1, 3))

colnames(dat) <-gsub("-.*", "", colnames(dat))

g <- dat %>%
  dplyr::group_by(GID_1) %>%
  dplyr::arrange(GID_1, year) %>%
  dplyr::mutate(g = log(GDP / lag(GDP,g_horizon)) / g_horizon * 100) %>%
  dplyr::mutate(g = lead(g, g_horizon)) %>%
  dplyr::select(GID_1, year, g) %>%
  dplyr::filter(! year %in% drop_horizon) %>%
  dplyr::filter(!GID_1 %in% c("PER.7_1", "PER.15_1")) %>%
  dplyr::mutate(unid = paste(GID_1, year, sep = "_")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-GID_1, -year)

dat <- left_join(dat, g, by = "unid") %>%
  dplyr::select(country, GID_1, year, ore_extraction, g) %>%
  dplyr::filter(!is.na(g))

# plot scatter
p <- dat %>% ggplot2::ggplot(aes(x = log(ore_extraction), y = g, group = country)) +
  ggplot2::geom_point(aes(shape=country, color=country), size = 4) +
  ggplot2::labs(x = "Ore extraction (kilotonnes, log)", y = paste0(g_horizon, "y avg. annual GDP growth rate"), title = NULL) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), labels=function(x) paste0(x,"%")) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  ggplot2::scale_color_manual(values = c("#1f78b4","#b2df8a","#33a02c"), labels = c("Chile", "Mexico", "Peru")) +
  ggplot2::scale_shape_manual(values = c(19, 17, 18), labels = c("Chile", "Mexico", "Peru")) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom", 
                 legend.title=element_blank(),
                 legend.text=element_text(size=16),
                 legend.spacing.x = unit(0.1, 'cm'),
                 panel.grid.minor =   element_blank(),
                 panel.grid.major =   element_blank(),
                 panel.border = element_blank(),
                 axis.line = element_line(colour = "black"),
                 axis.title.y = element_text(hjust = 1, size = 16),
                 axis.title.x = element_text(hjust = 1, size = 16),
                 axis.text.y = element_text(size = 14),
                 axis.text.x = element_text(size = 14))

# export
pdf(file=paste0("output/scatter_extraction_", g_horizon, "y_growth.pdf"), width = 8, height = 7)
grid.arrange(p)
dev.off()

}

