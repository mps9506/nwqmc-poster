## This script downloads data and recreates figures used in my poster
## hrbrthemes might have porblems loading fonts, if so, please comment
## out `theme_ipsum_rc` throughout

library(tidyverse)
library(echor)
library(gamm4)
library(modelr)
library(hrbrthemes)
library(ggrepel)
library(rnaturalearth)
library(sf)
library(ggspatial)
library(cowplot)
library(ggbeeswarm)

####################
## Some functions ##
####################

## Plot binomial smooths when exploring data in ggplot2
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}


############################################################
## Get simple features of permitted POTWs in Texas to map ##
############################################################
df.sf <- echoWaterGetFacilityInfo(output = "sf", verbose = T,
                               p_st = "tx", p_owop = "POTW")
## Clean the Dataframe to active permitted plants
df.sf <- df.sf %>%
  filter(CWPPermitStatusDesc != "Terminated")

## data exploration showed a few permits with either incorrect coordinates
## or incorrect counties. For the purpose of this excercise, I am just removing 
## those permits

## TX0058114
## TX0076856

df.sf <- df.sf %>%
  filter(SourceID != "TX0058114") %>%
  filter(SourceID != "TX0076856")

##########################################
## Get simple features of Texas to map ##
#########################################
txsf <- ne_states(country = "united states of america", returnclass = "sf") %>%
  filter(name == "Texas") %>%
  st_transform(3081)

## There are some locations outside of Texas, clean that up with spatial clip
df.sf.clip <- st_transform(df.sf, 3081) %>%
  st_intersection(txsf)

#########################################
## Get simple features of roads to map ##
########################################

roadssf <- ne_download(scale = 10, type = "roads", category = "cultural", returnclass = "sf")

roadssf <- st_transform(roadssf, 3081) %>%
  st_intersection(txsf)


###########################################
## Get simple features of oceans to map ##
##########################################

oceansf <- ne_download(scale = 10, type = "ocean", category = "physical", returnclass = "sf")

oceansf <- st_transform(oceansf, 3081)
####################################################
## Get simple features of populated places to map ##
####################################################

## connect to TNRIS.org and download zip file with
## texas municipal boundaries. Comes zipped

temp <- tempfile()
download.file("https://tnris-datadownload.s3.amazonaws.com/d/political-bnd/state/tx/political-bnd_tx.zip", temp)
tempdir <- tempdir()
utils::unzip(temp, exdir = tempdir)
sp_object <- rgdal::readOGR(file.path(tempdir, "txdot-cities"),
                            "txdot-2015-city-poly_tx", 
                            encoding = 'UTF-8', 
                            stringsAsFactors = FALSE, 
                            use_iconv = TRUE)
urbansf <- st_as_sf(sp_object)
urbansf <- st_transform(urbansf, 3081)


#################################################
## Explore and visualize discharge exceedances ##
#################################################


###############################################
## Get dataframe of permitted POTWs in Texas ##
###############################################
df <- echoWaterGetFacilityInfo(output = "df", verbose = T,
                               p_st = "tx", 
                               p_owop = "POTW")

## filter data to Houston Area counties

df_houston <- df %>%
  filter(CWPCounty %in% c("Harris", "Fort Bend", "Montgomery", "Brazoria", "Galveston", "Liberty",
                          "Waller", "Chambers", "Austin"))


df_houston <- df_houston %>%
  filter(SourceID != "TX0058114") %>%
  filter(SourceID != "TX0076856")


######################################################
## Make a map of POTWs in Texas with discharge size ##
######################################################

df.sf.clip <- df.sf.clip %>%
  filter(CWPCounty %in% c("Harris", "Fort Bend", "Montgomery", "Brazoria", "Galveston", "Liberty",
                          "Waller", "Chambers", "Austin")) %>%
  mutate(CWPTotalDesignFlowNmbr = as.numeric(CWPTotalDesignFlowNmbr))


box <- st_bbox(st_buffer(st_as_sfc(st_bbox(df.sf.clip)), 10000))

## Note that these figures are formatted for printting at large sizes.
## Printing to screen in RStudio will result in weird looking figures/fonts

a <- ggplot() +
  geom_sf(data = oceansf,
          fill = "royalblue4") +
  geom_sf(data = txsf %>%
            st_crop(box),
          lwd = 0.1, fill = "gray25", alpha = 0.75) +
  geom_sf(data = urbansf, alpha = 0.20, color = "gray60") +
  geom_text_repel(data = urbansf %>% 
                    filter(CITY_NM %in% c("Conroe", "Houston", "Galveston", "Baytown")) %>%
                    st_crop(st_bbox(df.sf.clip)) %>%
                    distinct(CITY_NM, .keep_all = TRUE) %>%
                    mutate(
                      CENTROID = map(geometry, st_centroid),
                      COORDS = map(CENTROID, st_coordinates),
                      COORDS_X = map_dbl(COORDS, 1),
                      COORDS_Y = map_dbl(COORDS, 2)
                    ), 
                  aes(x = COORDS_X,
                      y = COORDS_Y,
                      label = CITY_NM),
                  min.segment.length = 0,
                  point.padding = NA,
                  segment.color = "grey50",
                  family = "Roboto Condensed", fontface = "bold",
                  color = "white", size = 7, alpha = 0.8, seed = 1) +
  geom_sf(data = roadssf, alpha = 0.50, color = "wheat") +
  geom_sf(data = df.sf.clip %>% filter(!is.na(CWPTotalDesignFlowNmbr)),
          aes(size = as.numeric(CWPTotalDesignFlowNmbr), 
              color = as.numeric(CWPTotalDesignFlowNmbr)),
          alpha = 0.7, show.legend = 'point') +
  annotation_north_arrow(which_north = "true",
                         location = "bl",
                         style = north_arrow_minimal(text_family = "Roboto Condensed")) +
  annotation_scale(location = "br",
                   text_family = "Roboto Condensed", text_col = "white") +
  #geom_sf_label(data = df.sf.clip, aes(label = SourceID)) + ## Uncomment to include labels (helpful for data exploration)
  coord_sf(xlim = c(st_bbox(df.sf.clip)$xmin, st_bbox(df.sf.clip)$xmax), 
           ylim = c(st_bbox(df.sf.clip)$ymin, st_bbox(df.sf.clip)$ymax)) +
  scale_size(range = c(2,15),
             breaks = c(1,10,50,100,150,200)) +
  scale_color_viridis_c(option = "D", direction = -1, breaks = c(1,10,50,100,150,200)) +
  guides(color = guide_legend("Design Flow\n(MGD)"), size = guide_legend("Design Flow\n(MGD)")) +
  theme_ipsum_rc(base_size = 24,
                 axis_text_size = 14,
                 caption_size = 20,
                 plot_margin = margin(5, 5, 5, 5)) +
  theme(legend.position = "bottom",
        legend.box.margin = margin(5, 5, 5, 5),
        legend.box.spacing = unit(0.2, "in")) +
  labs(x = "", y = "")



b <- ggplot(df.sf.clip) +
  geom_boxplot(aes(x = CWPCounty, y = as.numeric(CWPTotalDesignFlowNmbr), 
                   fill = CWPCounty),
               alpha = 0.35, outlier.shape = NA) +
  geom_quasirandom(aes(x = CWPCounty, y = as.numeric(CWPTotalDesignFlowNmbr), 
                  color = CWPCounty), 
              varwidth = TRUE, alpha = 0.5) + 
  scale_y_log10() +
  theme_ipsum_rc(base_size = 24,
                 axis_text_size = 18,
                 axis_title_size = 24,
                 axis_title_just = "c",
                 caption_size = 20,
                 plot_margin = margin(5, 5, 5, 5)) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "County", y = "Design Flow (MGD)")

plot_grid(a, b, labels = "AUTO", label_fontfamily = "Roboto Condensed", nrow = 1)
##ggsave(here::here("figures/fig1.png"), width = 18, height = 10, units = "in", dpi = 300)

##############################################
## Download DMRs !!! This takes a while !!! ##
##############################################
df_reports <- downloadDMRs(df_houston, SourceID, start_date = "01/01/2010",
                           end_date = "12/30/2017", parameter_code = "50050")

df_reports <- df_reports %>%
  mutate(n = map(dmr, ~nrow(.))) %>%
  mutate(dmr = map(dmr, ~mutate(.x,
                                version_nmbr = as.character(version_nmbr),
                                monitoring_location_code = as.character(monitoring_location_code),
                                exceedence_pct = as.character(exceedence_pct),
                                rnc_detection_date = as.character(rnc_detection_date),
                                rnc_resolution_code = as.character(rnc_resolution_code),
                                rnc_resolution_date = as.character(rnc_resolution_date),
                                days_late = as.character(days_late),
                                nodi_code = as.character(nodi_code),
                                dmr_value_nmbr = as.numeric(dmr_value_nmbr),
                                dmr_value_standard_units = as.numeric(dmr_value_standard_units),
                                limit_value_nmbr = as.numeric(limit_value_nmbr),
                                limit_value_standard_units = as.numeric(limit_value_standard_units))))

df_reports <- df_reports %>%
  filter(n != 0)
df_reports <- tidyr::unnest(df_reports, dmr)  

## uncomment to save the data
##feather::write_feather(df_reports, here::here("data/df_reports.feather"))  

######################################################
## Fit a GAMM to average permitted flow exceedances ##
######################################################
df_reports_avgflow <-  df_reports %>%
  filter(parameter_code == "50050",
         statistical_base_code == "DB",
         perm_feature_nmbr != "101") %>%
  mutate(exceeded = case_when(
    dmr_value_nmbr > limit_value_nmbr ~ 1,
    dmr_value_nmbr <= limit_value_nmbr ~ 0),
    logDesignFlow = log(CWPTotalDesignFlowNmbr),
    month = lubridate::month(monitoring_period_end_date),
    year = lubridate::year(monitoring_period_end_date),
    dyear = lubridate::decimal_date(monitoring_period_end_date),
    block = paste0(SourceID, perm_feature_nmbr)) %>%
  filter(!is.na(exceeded))

ggplot(df_reports_avgflow, aes(dyear, exceeded)) +
  geom_jitter(height = 0.05, alpha = 0.25) +
  binomial_smooth(formula = y ~ splines::ns(x, 3))

ggplot(df_reports_avgflow, aes(year, exceeded)) +
  geom_jitter(height = 0.05, alpha = 0.25) +
  binomial_smooth(formula = y ~ splines::ns(x, 3))

ggplot(df_reports_avgflow, aes(CWPTotalDesignFlowNmbr, exceeded)) +
  geom_jitter(height = 0.05, alpha = 0.25) +
  binomial_smooth(formula = y ~ splines::ns(x, 3))

ggplot(df_reports_avgflow, aes(logDesignFlow, exceeded)) +
  geom_jitter(height = 0.05, alpha = 0.25) +
  binomial_smooth(formula = y ~ splines::ns(x, 3))

ggplot(df_reports_avgflow, aes(month, exceeded)) +
  geom_jitter(height = 0.05, alpha = 0.25) +
  binomial_smooth(formula = y ~ splines::ns(x, 3))

ggplot(df_reports_avgflow, aes(logDesignFlow, color = as.factor(exceeded))) +
  geom_line(stat = "density") 

ggplot(df_reports_avgflow, aes(dyear, color = as.factor(exceeded))) +
  geom_line(stat = "density")

m1 <- gamm4(exceeded ~ s(dyear) + s(month, bs = "cc") + s(logDesignFlow),
            random = ~(1|block),
            data = df_reports_avgflow,
            family = binomial(link = "logit"))

summary(m1$gam)
summary(m1$mer)

plot(m1$gam, all.terms = T)


#################################################
## Plot the predicted responses from the model ##
#################################################

## Create dataframe to make predictions from

newframe <- data_grid(df_reports_avgflow, 
                      monitoring_period_end_date = seq(as.Date("2010-02-15"), as.Date("2017-12-30"), by = "quarter"), 
                      CWPTotalDesignFlowNmbr = seq_range(CWPTotalDesignFlowNmbr, by = .25))

newframe <- newframe %>%
  mutate(dyear = lubridate::decimal_date(monitoring_period_end_date),
         month = lubridate::month(monitoring_period_end_date),
         logDesignFlow = log(CWPTotalDesignFlowNmbr))

## Make predictions

newframe$predict <- predict(m1$gam, newdata = newframe, type = "response", se.fit = F)

## Plot the fits
ggplot(data = newframe, aes(dyear, CWPTotalDesignFlowNmbr)) +
  geom_raster(aes(fill = predict)) +
  scale_fill_viridis_c(option = "C", name = "Probability of\nexceedance") +
  guides(fill = guide_colorbar(barheight = 20)) +
  theme_ipsum_rc(base_size = 20,
                 plot_title_size = 30,
                 subtitle_size = 24,
                 caption_size = 20,
                 axis_title_size = 24,
                 axis_title_just = "c") + 
  labs(x = "Date", y = "Plant Design Flow (mgd)")

## Uncomment to save

## ggsave(here::here("figures/fig2.png"), width = 18, height = 6, units = "in", dpi = 300)




#################################################
## Map illustrating errors in echo permit data ##
#################################################

df.errors <- echoWaterGetFacilityInfo(output = "sf", verbose = T,
                                  p_st = "tx", p_owop = "POTW")

statessf <- ne_states(country = "united states of america", returnclass = "sf") %>%
  filter(name %in% c("Texas", "Louisiana")) %>%
  st_transform(3081)

roadssf.errors <- ne_download(scale = 10, type = "roads", category = "cultural", returnclass = "sf") %>%
  st_transform(3081) %>%
  st_intersection(statessf)

## Clean the Dataframe to active permitted plants
df.errors <- df.errors %>%
  filter(CWPPermitStatusDesc != "Terminated") %>%
  filter(CWPCounty %in% c("Harris", "Fort Bend", "Montgomery", "Brazoria", "Galveston", "Liberty",
                          "Waller", "Chambers", "Austin")) %>%
  st_transform(3081)

box <- st_bbox(st_buffer(st_as_sfc(st_bbox(df.errors)), 30000))

ggplot() +
  geom_sf(data = statessf, aes(fill = name),
          lwd = 0.1, alpha = 0.75) +
  geom_sf(data = roadssf.errors, alpha = 0.50, color = "wheat") +
  geom_sf(data = df.errors,
          alpha = 0.7, size = 2) +
  annotation_north_arrow(which_north = "true",
                         location = "bl",
                         style = north_arrow_minimal(text_family = "Roboto Condensed")) +
  annotation_scale(location = "br",
                   text_family = "Roboto Condensed") +
  theme_ipsum_rc(base_size = 24,
                 axis_text_size = 14,
                 caption_size = 20,
                 plot_margin = margin(5, 5, 5, 5)) +
  theme(legend.position = "none") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  labs(x = "", y = "")

#ggsave(here::here("figures/fig3.png"), width = 12, height = 8, units = "in", dpi = 300)
