#' A function for creating an interactive map of local Moran's I statistics
#'
#' This function plots a map with the results of local Moran's I statistics, based on spatial polygons, a `listw` object, and a variable with an ID.
#' @param p A simple features `sf` object
#' @param listw A listw object (see spdep)
#' @param VAR A character string with the name of a variable
#' @param by A character string with the name of a key to join the local Moran's I statistics to the `sf` object
#' @keywords spatial
#' @export
#' @import dplyr
#' @examples
#' # Create a map of local Moran's I statistics for population density
#' #
#' # Obtain a listw object for the contiguities. First obtain the neighbors:
#' Hamilton_CT.nb <- spdep::poly2nb(as(Hamilton_CT, "Spatial"))
#' # Based on the neighbors, obtain a listw object:
#' Hamilton_CT.w <- spdep::nb2listw(Hamilton_CT.nb)
#'
#' localmoran.map(Hamilton_CT, Hamilton_CT.w, "POP_DENSITY", "TRACT")

localmoran.map <- function(p, listw, VAR, by){
  #require(tidyverse)
  #require(spdep)
  #require(plotly)

  df_msc <- p %>%
    rename(VAR = as.name(VAR),
           key = as.name(by)) %>%
    transmute(key,
              VAR,
              Z = (VAR - mean(VAR)) / var(VAR),
              SMA = lag.listw(listw, Z),
              Type = case_when(Z < 0 & SMA < 0 ~ "LL",
                               Z > 0 & SMA > 0 ~ "HH",
                               TRUE ~ "HL/LH"))

  local_I <- localmoran(df_msc$VAR, listw)

  colnames(local_I) <- c("Ii", "E.Ii", "Var.Ii", "Z.Ii", "p.val")

  df_msc <- left_join(df_msc,
                      data.frame(key = df_msc$key,
                                 local_I),
                      by = "key")

  plot_ly(df_msc) %>%
    add_sf(type = "scatter",
           split = ~(p.val < 0.05),
           color = ~Type,
           colors = c("red",
                      "khaki1",
                      "dodgerblue",
                      "dodgerblue4"))
}
