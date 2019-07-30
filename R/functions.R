
# map mines as points into country
map_mines <- function(spdf, reg, yr = "all"){
  
  dat <- snl_data %>% dplyr::filter(country %in% reg)
  if (yr != "all"){dat <- dat %>% dplyr::filter(year == yr)}
  
  zoom_bbox <- tibble::tribble(
    ~region,                    ~x_lim,            ~y_lim,
    "Chile",          c(-76.00, -67.00), c(-57.00, -14.00),
    "Latin America",  c( -88.00, -31.00), c(-58.00, 13.00),
    "Peru",           c(-82.00, -68.00), c(-19.00, 00.50),
    "Mexico",         c(-119.00, -84.00), c(13.00, 33.00)
  ) %>% 
    dplyr::mutate(geometry = lapply(seq_along(region), function(i) sf::st_multipoint(matrix(c(x_lim[[i]], y_lim[[i]]), nrow = 2))),
                  group = 1,
                  geometry = lapply(geometry, sf::st_bbox),
                  geometry = lapply(geometry, sf::st_as_sfc),
                  geometry = lapply(geometry, sf::st_geometrycollection),
                  geometry = sf::st_sfc(geometry)) %>% 
    sf::st_sf() %>% 
    sf::st_collection_extract()
  
  lim <- zoom_bbox %>% 
    dplyr::filter(region == reg)
  f_map <- fortify(spdf)
  gg_map <- ggplot2::ggplot(f_map, aes(x = long, y = lat, group = group)) + 
    ggplot2::geom_polygon(fill = "#e3e3e3") +
    ggthemes::theme_map() +
    ggplot2::coord_map(xlim = lim$x_lim[[1]], ylim = lim$y_lim[[1]]) + # For more acurate portion of the earth use ggplot2::coord_map (slow)
    ggplot2::geom_point(data = dat, mapping = aes(x = long, y = lat), colour = "#e31a1c", inherit.aes = FALSE, size = 1) +
    ggplot2::geom_path(data = f_map, mapping = aes(long, lat), colour = "#6f7072", size = 0.1)
  return(gg_map)
  
}


# Fast logdet approximation function
lndetPaceBarry <- function(W,qq = NULL,length.out = 200){
  require(matrixcalc)
  rmin=-1 # <---- CHANGE: perhaps rmin=1e-5, to produce results only for 0 < rho < 1
  rmax=1 # range of rho
  order=50
  iter=30 # <--- CHANGE: tuning parameters according to LeSage suggestions
  
  n=dim(W)[1]
  
  # Exact mom3ents from 1 to oexact
  td=matrix(c(0,sum(W^2)/2),length(c(0,sum(W^2)/2)),1)
  
  oexact=length(td)
  
  # stochastic moments
  mavmomi=matrix(0,order,iter)
  
  for(j in 1:iter)
  {
    u=matrix(rnorm(n,0,1),n,1)
    v=u
    utu=t(u)%*%u
    for (i in 1:order)
    {
      v=W%*%v
      mavmomi[i,j]=n*((t(u)%*%v)/(i*utu))
      
    }  
  }
  mavmomi[1:oexact,]=td[,matrix(1,iter,1)]
  
  # averages across iterations
  avmomi=as.matrix(rowMeans(mavmomi))
  
  # alpha matrix
  if (is.null(qq)) {
    alpha=seq(rmin,rmax,length.out = length.out) 
  } else {
    alpha=seq(rmin,rmax,qq) 
  }
  valpha=vandermonde.matrix(alpha,length(alpha))
  alomat=-valpha[,(2:(order+1))]
  
  # Estimated ln|I-aD| using mixture of exact, stochastic moments
  # exact from 1 to oexact, stochastic from (oexact+1) to order
  
  lndetmat=alomat%*%avmomi
  
  
  return(cbind(lndetmat,alpha))
}
