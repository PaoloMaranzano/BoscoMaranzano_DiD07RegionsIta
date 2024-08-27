#############################
########## Mapping ##########
#############################

##### Packages
library(sf)
library(ggplot2)
library(tidyverse)
library(eurostat)
library(haven)
library(ggpubr)

##### Setup
setwd("H:/Il mio Drive/RedditiComuniItaliani/Papers_PaoloBruno/Regioni")
linesize <- c("Treated" = 2,
              "Average non-treated" = 0.5)
coltype <- c("Treated" = "red",
             "Sicilia" = "red",
             "Abruzzo" = "green",
             "Campania" = "orange",
             "Lazio" = "blue",
             "Molise" = "forestgreen",
             "Average non-treated" = "black")

##### Data
Regioni <- read_dta("Regions_DID_data_Aug2024.dta")

##### Shapefile
shape <- get_eurostat_geospatial(output_class = "sf", resolution = "03", nuts_level = "2")
shape_it <- shape %>%
  filter(CNTR_CODE %in% c("IT"))

Regioni_sf <- Regioni %>%
  mutate(Region_Name = case_when(Region_Name == "Abbruzzo" ~ "Abruzzo",
                                 Region_Name == "Friuli" ~ "Friuli-Venezia Giulia",
                                 Region_Name == "Piemont" ~ "Piemonte",
                                 Region_Name == "Sardinia" ~ "Sardegna",
                                 Region_Name == "Sicily" ~ "Sicilia",
                                 Region_Name == "Vallée d'Aoste" ~ "Provincia Autonoma di Trento",
                                 Region_Name == "Trentino" ~ "Valle d'Aosta/Vallée d'Aoste",
                                 TRUE ~ Region_Name))

Regioni_sf <- left_join(x = Regioni_sf, y = shape_it, by = c("Region_Name" = "NUTS_NAME"))

Regioni_sf <- Regioni_sf %>%
  st_as_sf(crs = 4326)



##### Maps of observed data 1995-2021
vars <- c("RegGDPEuros","PerCapita_GDP_euros","Value_Added_per_Worker_euros","Self_Employed",
          "GROSS_CAPITAL_Euros","tot_employ","family_cosumption","indirecttaxcollection",
          "new_part_iva")
vars_label <- c("Regional GDP","Per-capita regional GDP",
                "Regional Value Added per regional worker",
                "Self-employment (2003-2020)",
                "Internal Fixed Gross Investment","Total regional Employment ",
                "Regional Family Consumption",
                "Indirect taxes collected within the region (1995-2020)",
                "New VAT Certificates (2001-2021)")
vars_legend <- c("Euros","Euros","Euros",
                "% of total \nemployment","Euros","Units",
                "Euros","Euros", "Units")
years <- c(1995,2000,2007,2015,2021)

rm(plt,v,y)
plt <- vector(mode = "list", length = length(vars_label))
plt_y <- vector(mode = "list", length = length(years))

for (v in 1:length(vars_label)) {
  data_temp <- Regioni_sf %>%
    filter(TIME %in% years) %>%
    mutate(Treat = case_when(d1_region == 1 ~ "Treated",
                             d1_region == 0 ~ "Non-treated"),
           Time = as.character(TIME)) %>%
    select(Region_Name,Time, Treat,variable = .data[[vars[v]]])
  
  data_temp_m <- data_temp %>%
    group_by(Region_Name,Treat) %>%
    summarise(across(c(variable),~ mean(.x,na.rm=T))) %>%
    mutate(Time = "Average 1995-2021")
  
  data_temp <- bind_rows(data_temp,data_temp_m)
  data_temp <- data_temp %>%
    mutate(Time = factor(Time, labels = c(years,"Average 1995-2021"), ordered = T))
  
  # cat(paste0(mean(data_temp$variable,na.rm=T),"\n"))
  
  plot_temp <- data_temp %>%
    ggplot() + 
    geom_sf(mapping = aes(fill = variable, linewidth = Treat)) + 
    facet_wrap(~ Time, ncol = length(years)+1) + 
    scale_fill_gradient2(vars_legend[v],low="green",high="red",mid = "yellow",
                         midpoint = mean(data_temp$variable,na.rm=T)
    ) +
    scale_linewidth_manual("",values = linesize) + 
    labs(title = vars_label[v])
  
  print(plot_temp)
  plt[[v]] <- plot_temp
}


map_comb <- ggarrange(plt[[1]],plt[[2]],plt[[3]],plt[[4]],plt[[5]],plt[[6]],
                      plt[[7]],plt[[8]],plt[[9]], nrow = 3)
ggpubr::ggexport(map_comb,width = 1800, height = 1200, res = 100, filename = "Observed.png")



##### Maps of observed differences data 2009-2006
plt_diffs <- vector(mode = "list", length = length(years))
y1 <- "2006"
y2 <- "2008"
for (v in 1:length(vars_label)) {
  plot_temp <- Regioni_sf %>%
    filter(TIME %in% c(y1,y2)) %>%
    mutate(Treat = case_when(d1_region == 1 ~ "Treated",
                             d1_region == 0 ~ "Non-treated")) %>%
    select(Time = TIME, Region_Name,Region_Code,Treat,variable = .data[[vars[v]]]) %>%
    pivot_wider(names_from = Time, values_from = variable) %>%
    mutate(Delta = .data[[y2]] - .data[[y1]]) %>%
    ggplot() + 
    geom_sf(mapping = aes(fill = Delta, linewidth = Treat)) + 
    scale_fill_gradient2(vars_legend[v],low="green",high="red",mid = "white",
                         midpoint = 0) + 
    scale_linewidth_manual(values = linesize) + 
    labs(title = vars_label[v])
  
  print(plot_temp)
  plt_diffs[[v]] <- plot_temp
}
map_comb <- ggarrange(plt_diffs[[1]],plt_diffs[[2]],plt_diffs[[3]],plt_diffs[[4]],plt_diffs[[5]],
                      plt_diffs[[6]], plt_diffs[[7]],plt_diffs[[8]],plt_diffs[[9]], nrow = 3, ncol = 3)
map_comb <- annotate_figure(p = map_comb,
                            top = text_grob(paste0("Observed difference between ",y1," and ",y2),
                                            size = 24,face = "bold"))
ggpubr::ggexport(map_comb,width = 1800, height = 1200, res = 100, filename = paste0("Differences_",y1,"_",y2,".png"))



##### Maps of observed times series data 2003-2013
plt_TS <- vector(mode = "list", length = length(years))
y1 <- "2003"
y2 <- "2013"
for (v in 1:length(vars_label)) {
  plot_temp <- Regioni_sf %>%
    mutate(Treat = case_when(d1_region == 1 ~ "Treated",
                             d1_region == 0 ~ "Average non-treated"),
           Regioni = case_when(d1_region == 1 ~ Region_Name,
                               d1_region == 0 ~ "Average non-treated")) %>%
    group_by(TIME,Regioni) %>%
    summarise(variable = mean(.data[[vars[v]]],na.rm = T)) %>%
    filter(TIME >= as.numeric(y1), TIME <= as.numeric(y2)) %>%
    ggplot() + 
    geom_line(mapping = aes(x = TIME, y = variable, col = Regioni), size = 1.2) + 
    geom_line(data = . %>% filter(Regioni == "Average non-treated"),
              mapping = aes(x = TIME, y = variable), size = 4) + 
    geom_vline(xintercept = 2007, col = "black", size = 2) + 
    scale_color_manual("",values = coltype) + 
    scale_x_continuous(breaks = c(2003,2005,2007,2009,2011,2013)) + 
    theme_bw() + 
    labs(title = vars_label[v], y = vars_legend[v], x = "")
  
  print(plot_temp)
  plt_TS[[v]] <- plot_temp
}
map_comb <- ggarrange(plt_TS[[1]],plt_TS[[2]],plt_TS[[3]],plt_TS[[4]],plt_TS[[5]],
                      plt_TS[[6]], plt_TS[[7]],plt_TS[[8]],plt_TS[[9]], nrow = 3, ncol = 3)
map_comb <- annotate_figure(p = map_comb,
                            top = text_grob(paste0("Observed time series between ",y1," and ",y2),
                                            size = 24,face = "bold"))
ggpubr::ggexport(map_comb,width = 1800, height = 1200, res = 125, filename = paste0("TS_",y1,"_",y2,".png"))



##### Index number with baseline 2006
plt_Index <- vector(mode = "list", length = length(years))
y0 <- 2006
y1 <- 2003
y2 <- 2013
for (v in 1:length(vars_label)) {
  plot_temp <- Regioni_sf %>%
    mutate(Treat = case_when(d1_region == 1 ~ "Treated",
                             d1_region == 0 ~ "Average non-treated"),
           Regioni = case_when(d1_region == 1 ~ Region_Name,
                               d1_region == 0 ~ "Average non-treated")) %>%
    group_by(TIME,Regioni) %>%
    summarise(variable = mean(.data[[vars[v]]],na.rm = T)) %>%
    ungroup() %>%
    group_by(Regioni) %>%
    mutate(Index = variable/variable[TIME == y0]*100) %>%
    filter(TIME >= as.numeric(y1), TIME <= as.numeric(y2)) %>%
    ggplot() + 
    geom_line(mapping = aes(x = TIME, y = Index, col = Regioni), size = 1.2) + 
    geom_line(data = . %>% filter(Regioni == "Average non-treated"),
              mapping = aes(x = TIME, y = Index), size = 4) + 
    geom_segment(mapping = aes(x = y0, y = min(Index), xend = y0, yend = 100), linetype = 2) +
    geom_segment(mapping = aes(x = 2003, y = 100, xend = y0, yend = 100), linetype = 2) +
    scale_color_manual("",values = coltype) + 
    scale_x_continuous(breaks = c(2003,2005,2007,2009,2011,2013)) + 
    theme_bw() + 
    labs(title = vars_label[v], y = paste0("Index: ",y0," = 100"), x = "")
  
  print(plot_temp)
  plt_Index[[v]] <- plot_temp
}
map_comb <- ggarrange(plt_Index[[1]],plt_Index[[2]],plt_Index[[3]],plt_Index[[4]],plt_Index[[5]],
                      plt_Index[[6]], plt_Index[[7]],plt_Index[[8]],plt_Index[[9]], nrow = 3, ncol = 3)
map_comb <- annotate_figure(p = map_comb,
                            top = text_grob(paste0("Index numbers with baseline ",y0),
                                            size = 24,face = "bold"))
ggpubr::ggexport(map_comb,width = 1800, height = 1200, res = 125, filename = paste0("Index_",y0,".png"))


