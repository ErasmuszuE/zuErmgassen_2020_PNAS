# Script to produce Figures from 
# ... "The origin, supply chain, and deforestation risk of Brazilian beef exports"
# Author: Erasmus zu Ermgassen
# Contact erasmus.zuermgassen@uclouvain.be with any questions


# Workstation set up ------------------------------------------------------


# Load packages
library(tidyverse)
library(sf)
library(stringi)
library(extrafont) 
library(scatterpie)
library(networkD3)
library(viridis)
library(scales)
library(ggrepel)
library(patchwork)


# Load data ---------------------------------------------------------------


# Load the .rdata file
# >>> Be sure to specifyhere the correct directory where the data are located
load("tmp/MANUSCRIPTS/PNAS/2020-07-17-BRA_BEEF_SEIPCS.rdata")


# The above loads the following data objects:
# abiec - data.frame with names of companies in Abiec, the Brazilian beef exporter association
# agro_pib_per_capita - data.frame with the agricultural GDP per capita, calculated from IBGE statistics
# anualpec - the nuiimber of cattle slaughtered per state from Anualpec
# biomes - sf object of Brazil biomas
# cd_all - trade data linked to logistic hubs (i.e. meat processing facilities, LINKED_CNPJ)
# data_ports - the volume of beef traded per port, per product (2015 & 2016)
# data_simple - sei-pcs results for brazilian beef exports 2015-2017
# geocode_key - dictionary of municipal geocodes to names
# gta_cleaning_summary_df - data.frame of counts of cleaned gtas used in the analysis
# gtas_per_year_df - data.frame of the number of GTAs per year used in the analysis
# mapbiomas - data.frame of Mapbiomas collection 4 municipal land use data
# markets_compared - data.frame containing Monte Carlo estimates of deforestation risk of export/domestic markets
# markets_compared_10_yr_amortized - as above, using 10-yr amortized deforestation data
# muni_biomes - a data.frame listing the dominant biome per municipality
# munis - sf object of municipalities in Brazil
# pasture_dfrs - data.frame of the intersection of deforestation and pasture expansion per municipality/year
# prod - data.frame of our estimate of municipal production, over a 5-yr lifecycle
# prod_dfrs_df - data.frame with the deforestation per ton per municipality/year (calculated over 5-yr lifecycle)
# shs - sf object with Trase beef 'logistics map' of the approximate location of suspected slaughter facilties in Brazil
# sifs_all - data.frame of all SIF-registered facilities in Brazil (data from 2018)
# sigsif - the proportion of SIF cattle slaughter per state which originates in each municipality
# slaughter_gtas - the number of cattle head slaughtered in the sample of GTAs used in this study
# slaughterhouse_sourcing_distances - the proportion of each slaughterhouses' supply per municipality, including TOTAL_LENGTH - the distance to the municipal centre
# slaughterhouse_sourcing_distances_dir - as above, direct suppliers only.
# soy_dfrs - soy deforestation as reported in zu Ermgassen et al. https://doi.org/10.1088/1748-9326/ab6497
# state_dic - a state name, code dictionary
# states - sf object of Brazil's states
# supply_sheds_df_dfrs - Monte Carlo estimates of the deforestation per ton of each logistic hub (LINKED_CNPJ)
# supply_sheds_df_dfrs_amortized - as above, but for 10-yr amortized data
# supply_sheds_df_sbst - the municipal supply sheds of logistic hubs identifed using GTAs
# total_ibge_dfrs - PRODES deforestation per municipality/year
# usa_data - SEI-PCS data for the USA, broken down per month


# Create folder for outputs -----------------------------------------------


# Specify where to save output
output_dir <- file.path(getwd(),"plots/MANUSCRIPTS/PNAS/")
if(!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("plots/MANUSCRIPTS/PNAS/ directory already exists")
}


# Pre-process data --------------------------------------------------------


# Extract coordinate range used for plotting maps
mapRange <- c(range(st_coordinates(states)[,1]),range(st_coordinates(states)[,2]))
mapRange[2] <- -35
mapRange[4] <- mapRange[4]+2


# Extract municipal centroids
munis_cent <- 
  st_centroid(munis) %>%
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2)) %>% 
  as.data.frame() %>%
  select(-geometry)


# subset bovine SIFs
sifs_bov <- 
  sifs_all %>%
  filter(grepl("AB1|AB2|AB3|AB4|AB5|MB1|MB2|MB3|MB4|MB5", SIF_CATEGORIES))


# Figure 1 - Russia -------------------------------------------------------


# Figure of Russia's supply chain


# (A) Sankey diagram


# Make summary df
# ... First I shorten some trader names to simplify the info presented on the Sankey
data_simple_russia <- 
  data_simple %>%
  filter(COUNTRY == "Russia", 
         YEAR == 2015) %>% 
  mutate(EXPORTER_GROUP = case_when(EXPORTER_GROUP == "Mataboi Alimentos" ~ "Prima Foods",
                                    EXPORTER_GROUP == "Cooperativa dos Produtores de Carne e Derivados de Gurupi" ~ "Cooperfrigo",
                                    EXPORTER_GROUP == "Irmaos Goncalves Comercio e Industria" ~ "Irmaos Goncalves",
                                    EXPORTER_GROUP == "Agra Agroindustrial de Alimentos" ~ "Agra Agro",
                                    EXPORTER_GROUP == "Jnj Comercial Importadora e Exportadora de Carnes" ~ "JNJ",
                                    EXPORTER_GROUP == "Sudambeef Industria Comercio Importacao e Exportacao" ~ "Sudambeef",
                                    EXPORTER_GROUP == "Vpr Brasil Importacoes e Exportacoes" ~ "VPR Brasil",
                                    TRUE ~ EXPORTER_GROUP)) %>%
  mutate(MUNI_ABBR = case_when(MUNICIPALITY == "AGUA CLARA" ~ "AGC",
                               MUNICIPALITY == "AGUA AZUL DO NORTE" ~ "AAN",
                               MUNICIPALITY == "MARABA" ~ "MAR",
                               MUNICIPALITY == "SAO FELIX DO XINGU" ~ "SFX",
                               MUNICIPALITY == "CHUPINGUAIA" ~ "CHU",
                               MUNICIPALITY == "NOVA CRIXAS" ~ "NCX",
                               MUNICIPALITY == "PORTO VELHO" ~ "PTV",
                               MUNICIPALITY == "ALTO ARAGUAIA" ~ "ARA",
                               MUNICIPALITY == "MINEIROS" ~ "MIN",
                               MUNICIPALITY == "CURIONOPOLIS" ~ "CUR",
                               MUNICIPALITY == "TORIXOREU" ~ "TOR",
                               TRUE ~ MUNICIPALITY)) %>%
  mutate(IMPORTER_GROUP = case_when(IMPORTER_GROUP == "Reserve Food Company Jsc" ~ "Reserve Food",
                                    IMPORTER_GROUP == "Marr Russia" ~ "Marr",
                                    IMPORTER_GROUP == "Miratorg Zapad" ~ "Miratorg",
                                    IMPORTER_GROUP == "Td Pervomaiskyi Khladocombinat" ~ "Pervomaiskyi",
                                    TRUE ~ IMPORTER_GROUP)) 


# Make list of top 10s for Russian market 
top_10_exporter_russia <- 
  data_simple_russia %>%
  group_by(EXPORTER_GROUP) %>% 
  summarise(SUM_FOB = sum(FOB)) %>%
  ungroup() %>% 
  arrange(desc(SUM_FOB)) %>% 
  top_n(wt = SUM_FOB, n = 10) %>% 
  pull(EXPORTER_GROUP)
top_10_importer_russia <- 
  data_simple_russia %>%
  group_by(IMPORTER_GROUP) %>% 
  summarise(SUM_FOB = sum(FOB)) %>%
  ungroup() %>% 
  arrange(desc(SUM_FOB)) %>% 
  top_n(wt = SUM_FOB, n = 10) %>% 
  pull(IMPORTER_GROUP)
top_10_muni_russia <- 
  data_simple %>% 
  filter(COUNTRY == "Russia", 
         !grepl("AGGREGATED", GEOCODE),
         YEAR == 2017) %>%
  group_by(GEOCODE) %>% 
  summarise(SUM_FOB = sum(FOB)) %>%
  ungroup() %>% 
  arrange(desc(SUM_FOB)) %>% 
  top_n(wt = SUM_FOB, n = 10) %>% 
  left_join(geocode_key, by = "GEOCODE") %>%
  mutate(MUNI_ABBR = case_when(MUNICIPALITY == "AGUA CLARA" ~ "AGC",
                               MUNICIPALITY == "AGUA AZUL DO NORTE" ~ "AAN",
                               MUNICIPALITY == "MARABA" ~ "MAR",
                               MUNICIPALITY == "SAO FELIX DO XINGU" ~ "SFX",
                               MUNICIPALITY == "CHUPINGUAIA" ~ "CHU",
                               MUNICIPALITY == "NOVA CRIXAS" ~ "NCX",
                               MUNICIPALITY == "PORTO VELHO" ~ "PTV",
                               MUNICIPALITY == "ALTO ARAGUAIA" ~ "ARA",
                               MUNICIPALITY == "MINEIROS" ~ "MIN",
                               MUNICIPALITY == "CURIONOPOLIS" ~ "CUR",
                               MUNICIPALITY == "TORIXOREU" ~ "TOR",
                               TRUE ~ MUNICIPALITY)) %>%
  pull(MUNI_ABBR)


# Make summary df with only from-to information, for Sankey
data_simple_russia1 <-
  data_simple_russia %>%
  mutate(
    EXPORTER_GROUP = ifelse(EXPORTER_GROUP %in% top_10_exporter_russia, EXPORTER_GROUP, "Other exporters"),
    IMPORTER_GROUP = ifelse(IMPORTER_GROUP %in% top_10_importer_russia, IMPORTER_GROUP, "Other importers"),
    MUNI_GROUP = ifelse(MUNI_ABBR %in% top_10_muni_russia, MUNI_ABBR, "Other municipalities")
  ) %>%
  group_by(MUNI_GROUP, EXPORTER_GROUP) %>%
  summarise(SUM_CWE = sum(CWE)) %>%
  ungroup() %>%
  rename(FROM = MUNI_GROUP,
         TO = EXPORTER_GROUP)
data_simple_russia2 <- 
  data_simple_russia %>%
  mutate(
    EXPORTER_GROUP = ifelse(EXPORTER_GROUP %in% top_10_exporter_russia, EXPORTER_GROUP, "Other exporters"),
    IMPORTER_GROUP = ifelse(IMPORTER_GROUP %in% top_10_importer_russia, IMPORTER_GROUP, "Other importers"),
    GEOCODE_GROUP = ifelse(GEOCODE %in% top_10_muni_russia, GEOCODE, "Other municipalities")
  ) %>%
  group_by(EXPORTER_GROUP, IMPORTER_GROUP) %>%
  summarise(SUM_CWE = sum(CWE)) %>%
  ungroup() %>%
  rename(FROM = EXPORTER_GROUP, 
         TO = IMPORTER_GROUP)
data_simple_russia3 <- 
  data_simple_russia %>%
  mutate(
    EXPORTER_GROUP = ifelse(EXPORTER_GROUP %in% top_10_exporter_russia, EXPORTER_GROUP, "Other exporters"),
    IMPORTER_GROUP = ifelse(IMPORTER_GROUP %in% top_10_importer_russia, IMPORTER_GROUP, "Other importers"),
    GEOCODE_GROUP = ifelse(GEOCODE %in% top_10_muni_russia, GEOCODE, "Other municipalities")
  ) %>%
  group_by(IMPORTER_GROUP, COUNTRY) %>%
  summarise(SUM_CWE = sum(CWE)) %>%
  ungroup() %>%
  rename(FROM = IMPORTER_GROUP, 
         TO = COUNTRY)
data_simple_russia <- 
  bind_rows(data_simple_russia1, data_simple_russia2, data_simple_russia3)


# tidy up
rm(data_simple_russia1, data_simple_russia2, data_simple_russia3)


# Give number ids to each municipality; required for sankeyNetwork
nodes <- data.frame(name = unique(c(data_simple_russia$FROM, data_simple_russia$TO)),
                    id = 0:(length(unique(c(data_simple_russia$FROM, data_simple_russia$TO)))-1)) %>%
  mutate(GROUP = case_when(name %in% c(top_10_exporter_russia, "Other exporters") ~ "EXPORTER",
                           name %in% c(top_10_importer_russia, "Other importers") ~ "IMPORTER",
                           name %in% c(top_10_muni_russia, "Other municipalities") ~ "MUNICIPALITY",
                           name == "Russia" ~ "COUNTRY",
                           TRUE ~ NA_character_))
if(any(is.na(nodes$GROUP))){stop("Check grouping")}


# Format the data so it can be called in sankeyNetwork
links_direct <- data_simple_russia %>%
  left_join(nodes, by = c('FROM' = 'name')) %>%
  rename(SOURCE_ID = id) %>%
  left_join(nodes, by = c('TO' = 'name')) %>%
  rename(TARGET_ID = id)  


# Give a color for each group:
# ... Add a 'group' column to the nodes data frame:
# ... https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html
# ... colors from Dark2 ColorBbrewer palette
my_color <- 'd3.scaleOrdinal() .domain(["MUNICIPALITY", "EXPORTER", "IMPORTER", "COUNTRY"]) .range(["#66A61E", "#E6AB02", "#A6761D", "#666666"])'


# plot Sankey 
sankeyNetwork(Links = links_direct, 
              Nodes = nodes, 
              Source = 'SOURCE_ID', 
              Target = 'TARGET_ID', 
              Value = 'SUM_CWE', 
              NodeID = 'name',
              fontSize = 16,
              fontFamily = "Avenir Medium",
              colourScale=my_color, 
              width = 1000,
              NodeGroup="GROUP", 
              iterations = 0 # Sets order of nodes to match order in df 
              )


# Note this image then needs to be manually saved, as "plots/MANUSCRIPTS/RUSSIA_SANKEY.png"
# ... this file is then loaded later and included in the multi-plot


# tidy up
rm(links_direct, nodes, data_simple_russia, my_color,
   top_10_exporter_russia, top_10_importer_russia, top_10_muni_russia)


# (B) slaughterhouse sourcing 


# (i) Map proportion of sourcing Russian sourcing per GEOCODE in 2017
munis_prop <-
  data_simple %>%
  filter(COUNTRY == "Russia", 
         YEAR == 2015) %>% 
  mutate(TOTAL_CWE = sum(CWE)) %>%
  group_by(GEOCODE) %>%
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CWE)) %>%
  ungroup() %>%
  inner_join(munis, ., by = "GEOCODE") 


# Select slaughterhouses exporting to Russia
shs_russia <- 
  data_simple %>%
  filter(COUNTRY == "Russia", 
         YEAR == 2015) %>%
  group_by(LH_CNPJ) %>% 
  summarise(SUM_CWE_TONS = sum(CWE) / 1000)
shs_to_plot <- 
  shs %>%
  inner_join(shs_russia, by = c("SH_CNPJ" = "LH_CNPJ")) %>%
  mutate(
    X = map_dbl(geometry, 1),
    Y = map_dbl(geometry, 2)
  ) %>%
  as.data.frame() %>%
  select(-geometry) %>% 
  distinct() 
rm(shs_russia)


# Plot
# ... Nb do I want state or biome borders?
# ... I can overlay scatterpie and map...
# https://www.rdocumentation.org/packages/cowplot/versions/0.9.2/topics/align_plots
p1 <- 
  ggplot() +
  geom_sf(
    data = munis_prop,
    aes(fill=PROP_CWE*100),
    color = NA, size = 0.25) +
  geom_sf(data = states, fill = NA, color = "darkgrey") +
  geom_point(data = shs_to_plot, aes(x = X, y = Y, size = SUM_CWE_TONS / 1000), 
             fill = "lightgrey", 
             alpha = 0.5) +
  scale_fill_viridis(option="magma",
                     name = bquote('Origin of\nsourcing (%)'),
                     na.value="white",
                     direction = -1) +
  theme_beef() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position = "left",
        legend.box.margin=margin(-10,-10,-10,-10)
  ) +
  coord_sf(datum = NA) +
  scale_size_continuous(name = "ktons per\nslaughterhouse") 


# tidy up
rm(munis_prop, shs_to_plot)


# (C) Deforestation per state, biome.


# Plot dfrs risk per biome
p3 <- 
  data_simple %>%
  filter(COUNTRY == "Russia") %>%
  group_by(YEAR, BIOME) %>%
  summarise(SUM_DFRS = sum(BEEF_DEF_5_YEAR_HA)) %>%
  ungroup() %>%
  filter(BIOME %in% c("Amazon", "Cerrado", "Atlantic Forest")) %>%
  ggplot(aes(YEAR, SUM_DFRS/1e03, fill = BIOME)) + 
  geom_bar(stat = "identity") + 
  theme_beef() + 
  theme(axis.title.x = element_blank(), 
        legend.title = element_blank(),
        axis.line = element_line(),
        legend.position = "bottom") + 
  ylab("Deforestation risk (ha)") +
  scale_fill_manual(values = biome_colors,
                    breaks = c("Cerrado", "Atlantic Forest", "Amazon"),
                    labels = c("Cerrado", "Atl. For.", "Amazon")) +
  scale_y_continuous(limits = c(0,11),
                     expand = c(0,0),
                     label = unit_format(unit = "K"))


# Assemble all panels of Figure 1
myImage <- png::readPNG("plots/MANUSCRIPTS/PNAS/2020-07-16-FIGURE_1A.png")
myImage <- grid::rasterGrob(myImage, interpolate = TRUE)
p2 <- cowplot::ggdraw(myImage) 
sankey_multiplot_sbst <- 
  cowplot::plot_grid(p1, p3,
                     labels = c("B","C"),
                     rel_widths = c(1, 1),
                     ncol = 2)
sankey_multiplot_stacked <- 
  cowplot::plot_grid(p2, sankey_multiplot_sbst, 
                     labels = c("A",""),
                     ncol = 1)


# Export
ggsave(paste0(output_dir, "/", Sys.Date(),"-FIGURE_1_ORDERED.tiff"),
       units = "cm", 
       width = 17.8, 
       height = 17.8,
       plot = sankey_multiplot_stacked)


# tidy up
rm(p1, p2, p3, sankey_multiplot_sbst, myImage,
   sankey_multiplot_stacked)


# Figure 2 - state data ---------------------------------------------------


# (A) Map export proportion per state


# Make map with state colours
states_to_plot <- states %>%
  mutate(STATE_ABBR = as_factor(STATE_ABBR),
         STATE_ABBR = fct_relevel(STATE_ABBR,
                                  levels = state_order))
map_range <- c(range(st_coordinates(states)[,1]),range(st_coordinates(states)[,2]))
p_STATES_BACKGROUND <- 
  ggplot() + 
  geom_sf(data = states_to_plot, 
          aes(fill = STATE_ABBR), col = "darkgrey") + 
  scale_fill_manual(values = state_colors) + 
  coord_sf(datum = NA,
           xlim = c(map_range[1],map_range[2]), 
           ylim = c(map_range[3],map_range[4])) +
  theme_beef() + 
  theme(legend.position = "none")
rm(states_to_plot)


# Sum the exported carcass per state
exp_per_state <- 
  data_simple %>%
  group_by(STATE, YEAR) %>% 
  summarise(EXPORTED_CWE_TONS = sum(CWE) / 1000) %>%
  ungroup()


# Calculate production per state
prod_per_state <- 
  prod %>% 
  mutate(TWO_DIGIT_CODE = str_sub(GEOCODE, start = 1, end = 2)) %>%
  left_join(state_dic, by = "TWO_DIGIT_CODE") %>%
  group_by(STATE_NAME, YEAR) %>%
  summarise(SUM_CW_PRODUCTION_TONS_5_YR = sum(CW_PRODUCTION_TONS_5_YR) ) %>%
  ungroup()


# Calculate the proportion exported per state
# ... take the mean per state/year for the plot
exp_prop_per_state <-
  left_join(prod_per_state, exp_per_state, by = c("STATE_NAME" = "STATE", "YEAR")) %>%
  filter(YEAR %in% c(2015:2017)) %>%
  mutate(EXPORTED_CWE_TONS = ifelse(is.na(EXPORTED_CWE_TONS), 0, EXPORTED_CWE_TONS),
         CWE_PRODUCTION_TONS = SUM_CW_PRODUCTION_TONS_5_YR / 5,
         DOMESTIC_CWE_TONS = CWE_PRODUCTION_TONS - EXPORTED_CWE_TONS) %>%
  group_by(STATE_NAME) %>% 
  summarise_at(vars(EXPORTED_CWE_TONS, DOMESTIC_CWE_TONS, CWE_PRODUCTION_TONS), 
               .funs = mean) %>%
  ungroup()


# Add coordinates to the export proportion
states_cent <- st_centroid(states) %>%
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2))
exp_prop_per_state <- 
  left_join(exp_prop_per_state, states_cent, by = "STATE_NAME") 
if(any(is.na(exp_prop_per_state))){stop("Check join")}


# Plot map of pie charts
p_EXP_SHARE <- 
  ggplot() +
  geom_scatterpie(aes(x=X, y=Y, group=STATE_NAME, r=CWE_PRODUCTION_TONS/5e05),
                  data=exp_prop_per_state, 
                  cols=c("EXPORTED_CWE_TONS", "DOMESTIC_CWE_TONS"), 
                  color="black", 
                  alpha=.8) +
  geom_scatterpie_legend(exp_prop_per_state$CWE_PRODUCTION_TONS/5e05, 
                         x=-38, 
                         y=-29,
                         n = 3,
                         labeller = function(x) x*5e05 / 1e06) +
  annotate("text",
           x = -34.5,
           y = -24,
           label = "Mton/year", 
           family = "Avenir Medium", 
           size = 4) +
  # theme_beef() +
  theme_minimal() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.19),
        text= element_text(size = 14, family="Avenir Medium"),
        legend.text = element_text(margin = margin(l = -0.2, r = -0.2, unit = 'cm'))
  ) +
  scale_fill_manual(values = trase_colors,
                    labels = c("Domestic market","Exports")) +
  coord_sf(datum = NA,
           xlim = c(map_range[1],map_range[2]), 
           ylim = c(map_range[3],map_range[4])) +
  NULL


# Align the two plots
# ... Add missing state colours
# ... Add legend title (Production (ktons))
p_ALIGNED <- cowplot::align_plots(p_STATES_BACKGROUND, p_EXP_SHARE,
                                  align = "hv")
p_EXP_SHARE_ALIGNED <- cowplot::ggdraw(p_ALIGNED[[1]]) + cowplot::draw_plot(p_ALIGNED[[2]])


# Make df with state origin of exports
df_origin_exports <- 
  data_simple %>%
  group_by(COUNTRY_GROUP) %>%
  mutate(TOTAL_CWE = sum(CWE)) %>%
  group_by(COUNTRY_GROUP, STATE) %>%
  summarise(PROP_CWE = sum(CWE) / unique(TOTAL_CWE)) %>%
  ungroup() %>%
  left_join(state_dic, by = c("STATE" = "STATE_NAME"))


# Identity state origin of domestic market
# ... i.e. domestic = production - exports
df_origin_domestic <- 
  exp_prop_per_state %>%
  mutate(TOTAL_DOMESTIC_CWE_TONS = sum(DOMESTIC_CWE_TONS)) %>%
  group_by(STATE_ABBR) %>%
  summarise(PROP_CWE = sum(DOMESTIC_CWE_TONS) / unique(TOTAL_DOMESTIC_CWE_TONS)) %>%
  ungroup() %>%
  mutate(COUNTRY_GROUP = "Domestic\nmarket")


# Plot the state origin of exports & domestic market's beef
top_10_cntry_grp_abbreviated <- 
  c("Domestic\nmarket","China", "Egypt", "Russia", "Iran", "USA","Chile","Venezuela",
    "Italy","Netherlands","UK","Other EU", "Other halal","Other")
p_origin <- 
  bind_rows(df_origin_domestic, df_origin_exports) %>%
  mutate(
    STATE_ABBR = ifelse(is.na(STATE_ABBR), "UN", STATE_ABBR),
    COUNTRY_GROUP = case_when(COUNTRY_GROUP == "United States" ~ "USA",
                              COUNTRY_GROUP == "United Kingdom" ~ "UK",
                              COUNTRY_GROUP == "Other EU markets" ~ "Other EU",
                              COUNTRY_GROUP == "Other halal markets" ~ "Other halal",
                              COUNTRY_GROUP == "Other markets" ~ "Other",
                              TRUE ~ COUNTRY_GROUP),
    COUNTRY_GROUP = as_factor(COUNTRY_GROUP),
    COUNTRY_GROUP = fct_relevel(COUNTRY_GROUP,
                                levels = rev(top_10_cntry_grp_abbreviated)),
    STATE_ABBR = as_factor(STATE_ABBR),
    STATE_ABBR = fct_relevel(STATE_ABBR,
                             levels = state_order)
  ) %>%
  ggplot(aes(COUNTRY_GROUP, PROP_CWE * 100, fill = STATE_ABBR)) +
  geom_bar(stat = "identity") + 
  theme_beef_with_axes() + 
  coord_flip() + 
  ylab("State of origin of Brazilian beef exports (%)") + 
  theme(axis.title.y = element_blank(), 
        legend.position = "none", 
        legend.title = element_blank(),
        text = element_text(size = 10)) +
  scale_fill_manual(values = state_colors) + 
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = guide_legend(ncol = 9), byrow = TRUE)


# Extract and add legend to map
my_legend <- cowplot::get_legend(p_origin + 
                                   theme(legend.position = "bottom", 
                                         legend.text = element_text(margin = margin(l = -0.2, r = -0.2, unit = 'cm'))))
p_map_with_legend <- 
  cowplot::plot_grid(
    p_EXP_SHARE_ALIGNED,
    my_legend,
    nrow = 2,
    rel_heights = c(1,0.2))


p_sc <- 
  cowplot::plot_grid(
    p_map_with_legend, 
    p_origin,
    rel_widths = c(0.9,1), 
    labels = c("A","B"),
    align = "h",
    ncol = 2)


# Export
ggsave(paste0(output_dir, "/", Sys.Date(),"-FIGURE_2.tiff"), 
       units = "cm", 
       width = 17.8, 
       height = 12,
       plot = p_sc)


# tidy up
rm(p_sc, p_map_with_legend, 
   my_legend,
   p_origin, 
   df_origin_domestic, 
   df_origin_exports, 
   exp_per_state, prod_per_state, 
   exp_prop_per_state, 
   fn_plot_export_prop,
   p_STATES_BACKGROUND, p_EXP_SHARE,
   p_EXP_SHARE_ALIGNED, p_ALIGNED)


# Figure 3 - US risks -----------------------------------------------------


# Plot of fluctuations in US sourcing over time
# ... NB there are some (about 10%) US flows where the month was not retained during the data processing.
# ... I also do not plot the deforestation risk linked to 'UNKNOWN' flows, for clarity.


# Specify the order of the HS4 codes in plotting
hs4_order <- rev(c("Fresh/frozen beef", "Processed beef"))


# Plot US product volumes/state
# ... NB that VOL is in kg, so VOL/1e06 -> kton
p_us_a <- 
  usa_data %>%
  mutate(STATE_ABBR = ifelse(STATE_ABBR == "UNKNOWN", "UN", STATE_ABBR),
         STATE_ABBR = as_factor(STATE_ABBR),
         STATE_ABBR = fct_relevel(STATE_ABBR, state_order)) %>%
  mutate(HS4_DESC = as_factor(HS4_DESC),
         HS4_DESC = fct_relevel(HS4_DESC, hs4_order)) %>%
  ggplot(aes(CD_DATE, TOTAL_VOL/1e06, fill = STATE_ABBR)) + 
  facet_wrap(~HS4_DESC) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = state_colors) + 
  geom_vline(xintercept = as.Date("2016-08-01"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2017-07-01"), linetype = "dashed") + 
  ylab("US imports of\nBrazilian beef (kton)") + 
  theme_beef_with_axes() + 
  theme(axis.title.x = element_blank(), 
        legend.position = "none",
        strip.text = element_text(size = 12)) + 
  scale_y_continuous(limits = c(0,7.5)) + 
  NULL


# Plot deforestation risk data 
# ... focus on fresh/frozen beef and processed beef
# ... and don't plot deforestation which is linked to flows of 'Unknown' origin
# ... (these are linked to national average defotestation rates, and blur the relationship)
# ... this is explained in the figure caption.
p_us_b <- 
  usa_dfrs %>% 
  filter(HS4_DESC %in% hs4_order, 
         STATE_ABBR != "UN") %>%
  mutate(STATE_ABBR = ifelse(is.na(STATE_ABBR), "UN", STATE_ABBR)) %>%
  mutate(STATE_ABBR = as_factor(STATE_ABBR),
         STATE_ABBR = fct_relevel(STATE_ABBR, state_order)) %>%
  mutate(HS4_DESC = as_factor(HS4_DESC),
         HS4_DESC = fct_relevel(HS4_DESC, hs4_order)) %>%
  ggplot(aes(x = CD_DATE, y = SUM_DFRS , fill = STATE_ABBR)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ HS4_DESC) +
  theme_beef_with_axes() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(), 
        legend.position = "none",
        strip.text = element_text(size = 12)) +  
  scale_fill_manual(values = state_colors) + 
  geom_vline(xintercept = as.Date("2016-08-01"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2017-07-01"), linetype = "dashed") + 
  ylab("Deforestation risk\nof US imports (ha/month)") +
  scale_y_continuous(limits = c(0,150))


p_for_legend <- 
  usa_data %>%
  mutate(STATE_ABBR = ifelse(STATE_ABBR == "UNKNOWN", "UN", STATE_ABBR),
         STATE_ABBR = as_factor(STATE_ABBR),
         STATE_ABBR = fct_relevel(STATE_ABBR, state_order)) %>%
  mutate(HS4_DESC = as_factor(HS4_DESC),
         HS4_DESC = fct_relevel(HS4_DESC, hs4_order)) %>%
  ggplot(aes(CD_DATE, TOTAL_VOL/1e06, fill = STATE_ABBR)) +
  facet_wrap(~HS4_DESC) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = state_colors) +
  geom_vline(xintercept = as.Date("2016-08-01"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2017-07-01"), linetype = "dashed") +
  theme_beef_with_axes() +
  theme(axis.title.x = element_blank(),
        legend.position = "right",
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank())


my_legend <- cowplot::get_legend(p_for_legend)
two_plots <- cowplot::plot_grid(
  p_us_a,
  p_us_b,
  nrow = 2,
  labels = c("A","B"),
  align = "vh")
two_plots_plus_legend <- cowplot::plot_grid(two_plots,
                                            my_legend,
                                            ncol = 2, 
                                            rel_widths = c(1, .2)
)


# Export
ggsave(paste0(output_dir, "/", Sys.Date(), "-FIGURE_3.tiff"),
       units = "cm", 
       width = 17.8, 
       height = 12,
       plot = two_plots_plus_legend)


# tidy up
rm(usa_dfrs, fn_extract_us_dfrs, p_us_dfrs, usa_data, 
   p_for_legend, my_legend, two_plots_plus_legend, two_plots,
   p_cds, p_us_dfrs)


# Figure 4 - multiplot ----------------------------------------------------


# A plot with:
# (A) bar chart of deforestation risk/major exporter
# (B) scatter plot of deforestation risk/ major market
# (C) Bar chart of deforestation risk/country per biome.
# (D) Bar chart of deforestation risk/product
# {E} The deforestation risk of all exports vs the domestic market
# {F} The relative deforestation risk of all exports vs the domestic market


# (A) Bar chart of relative deforestation risk/exporter


# Relative deforestation risk
# ... Note Mataboi Alimentos has rebranded as Prima Foods
short_name_exporters <- top_10_exporter
short_name_exporters[short_name_exporters == "Mataboi Alimentos"] <- "Prima Foods"
short_name_exporters[short_name_exporters == "Meat Snack Partners do Brasil"] <- "Meat Snack Partners"
short_name_exporters[short_name_exporters == "Irmaos Goncalves Comercio e Industria"] <- "Irmaos Goncalves"
short_name_exporters[short_name_exporters == "Frisa Frigorifico Rio Doce"] <- "Frisa"
market_average_risk <- 
  cd_all %>% 
  filter(YEAR == 2017) %>%
  group_by(YEAR, LINKED_CNPJ) %>%
  summarise(SUM_CWE_PER_FLOW = sum(CWE_PER_FLOW)) %>%
  left_join(supply_sheds_df_dfrs, by = c("LINKED_CNPJ", "YEAR")) %>%
  unnest() %>%
  mutate(DFRS_HA = SUM_DFRS_PER_TON * SUM_CWE_PER_FLOW / 1000) %>%
  group_by(NUM_RANDOMISATION) %>%
  summarise(SUM_DFRS_HA = sum(DFRS_HA),
            DFRS_HA_PER_TON = SUM_DFRS_HA / (sum(SUM_CWE_PER_FLOW)/1000)) %>%
  ungroup() %>%
  summarise(DFRS_HA_PER_KTON_MEAN = mean(DFRS_HA_PER_TON) * 1000) %>% 
  ungroup()
(p_exporter_rel_dfrs_risk <- 
    cd_all %>% 
    mutate(EXPORTER = case_when(EXPORTER == "Mataboi Alimentos" ~ "Prima Foods",
                                EXPORTER == "Meat Snack Partners do Brasil" ~ "Meat Snack Partners",
                                EXPORTER == "Irmaos Goncalves Comercio e Industria" ~ "Irmaos Goncalves",
                                EXPORTER == "Frisa Frigorifico Rio Doce" ~ "Frisa",
                                TRUE ~ EXPORTER),
           EXPORTER_GROUP = case_when(EXPORTER %in% short_name_exporters ~ EXPORTER, 
                                      TRUE ~ "Other exporters")) %>%
    filter(YEAR == 2017) %>%
    group_by(YEAR, EXPORTER_GROUP, LINKED_CNPJ) %>%
    summarise(SUM_CWE_PER_FLOW = sum(CWE_PER_FLOW)) %>%
    left_join(supply_sheds_df_dfrs, by = c("LINKED_CNPJ", "YEAR")) %>%
    unnest() %>%
    mutate(DFRS_HA = SUM_DFRS_PER_TON * SUM_CWE_PER_FLOW / 1000) %>%
    group_by(EXPORTER_GROUP, NUM_RANDOMISATION) %>%
    summarise(SUM_DFRS_HA = sum(DFRS_HA),
              DFRS_HA_PER_TON = SUM_DFRS_HA / (sum(SUM_CWE_PER_FLOW)/1000)) %>%
    ungroup() %>%
    group_by(EXPORTER_GROUP) %>%
    summarise(DFRS_HA_PER_KTON_MEAN = mean(DFRS_HA_PER_TON) * 1000,
              DFRS_HA_PER_KTON_05 = quantile(DFRS_HA_PER_TON, probs = c(0.05)) * 1000,
              DFRS_HA_PER_KTON_95 = quantile(DFRS_HA_PER_TON, probs = c(0.95)) * 1000,
              DFRS_HA_PER_KTON_33 = quantile(DFRS_HA_PER_TON, probs = c(0.33)) * 1000,
              DFRS_HA_PER_KTON_66 = quantile(DFRS_HA_PER_TON, probs = c(0.66)) * 1000
    ) %>% 
    ungroup() %>%
    mutate(EXPORTER_GROUP = as_factor(EXPORTER_GROUP), 
           EXPORTER_GROUP = fct_relevel(EXPORTER_GROUP, levels = rev(c(short_name_exporters, "Other exporters")))) %>%
    ggplot(aes(EXPORTER_GROUP, DFRS_HA_PER_KTON_MEAN)) +
    geom_bar(stat = "identity", fill = year_colors[1]) + 
    geom_errorbar(aes(ymin = DFRS_HA_PER_KTON_05, ymax = DFRS_HA_PER_KTON_95), 
                  size = 0.5, width = 0) + 
    theme_beef_with_axes() + 
    coord_flip() +
    theme(axis.title.y = element_blank(),
          # axis.text.y = element_blank(),
          legend.title = element_blank(), 
          legend.position = "none") + 
    ylab("Relative deforestation risk\n(ha/kton)") + 
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, 90)) +
    geom_hline(yintercept = market_average_risk$DFRS_HA_PER_KTON_MEAN, 
               linetype = "dashed") +
    NULL
)


# (B) scatter plot of deforestation risk per country


# Select countries to highlight in plot 
# ... i.e. countries who are in the top 10 for volume or deforestation risk
# ... NB I remove Netherlands from the list because otherwise the coutnry names overlap in the plot
top_10_countries <-
  data_simple %>%
  mutate(
    COUNTRY = case_when(
      COUNTRY == "Hong Kong" ~ "China (Hong Kong)",
      COUNTRY == "China" ~ "China (mainland)",
      COUNTRY == "United States" ~ "USA",
      COUNTRY == "United Kingdom" ~ "UK",
      COUNTRY == "Other EU markets" ~ "Other EU",
      COUNTRY == "Other halal markets" ~ "Other halal",
      COUNTRY == "Other markets" ~ "Other",
      TRUE ~ COUNTRY
    )
  ) %>%
  group_by(COUNTRY) %>%
  summarise(SUM_FOB = sum(FOB)) %>%
  ungroup() %>%
  top_n(n = 10, wt = SUM_FOB) %>%
  pull(COUNTRY)
top_10_countries <- top_10_countries[top_10_countries != "Netherlands"]


# Make summary data.frame with extra column for names of countries to highlight
cntry_summary <- 
  data_simple %>%
  mutate(
    COUNTRY = case_when(
      COUNTRY == "Hong Kong" ~ "China (Hong Kong)",
      COUNTRY == "China" ~ "China (mainland)",
      COUNTRY == "United States" ~ "USA",
      COUNTRY == "United Kingdom" ~ "UK",
      COUNTRY == "Other EU markets" ~ "Other EU",
      COUNTRY == "Other halal markets" ~ "Other halal",
      COUNTRY == "Other markets" ~ "Other",
      TRUE ~ COUNTRY
    )
  ) %>%
  mutate(COUNTRY_AGG = ifelse(COUNTRY %in% top_10_countries, COUNTRY, ""),
         TOTAL_CWE_TONS = sum(CWE) / 1000,
         TOTAL_FOB = sum(FOB),
         TOTAL_DFRS_RISK_HA = sum(BEEF_DEF_5_YEAR_HA)) %>%
  group_by(COUNTRY, COUNTRY_AGG) %>%
  summarise(SUM_FOB = sum(FOB),
            PERC_CWE = sum(CWE)/ 1000 / unique(TOTAL_CWE_TONS) * 100,
            PERC_FOB = sum(FOB)/ unique(TOTAL_FOB) * 100,
            PERC_DFRS_RISK = sum(BEEF_DEF_5_YEAR_HA)/ unique(TOTAL_DFRS_RISK_HA) * 100
  )


# Plot scatterplot
p_cntry_scatter <- 
  cntry_summary %>%
  ggplot(aes(PERC_CWE, PERC_DFRS_RISK)) +
  geom_point(size = 2) +
  theme_beef_with_axes() +
  theme(legend.position = c(0.15, 0.85)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_text_repel(aes(label = COUNTRY_AGG), family = "Avenir Medium") + 
  labs(x = "Exports (% carcass weight)",
       y = "\nDeforestation risk (%)") +
  NULL


# tidy up
rm(top_10_countries, cntry_summary)


# (C) Bar chart of deforestation risk/country per biome.
order_3c <- c("China", "Egypt", "Russia","Iran", "USA", "Chile","Venezuela",
              "Italy", "Netherlands", "UK", "Other EU markets", "Other halal markets", "Other markets")
(p_cntry_biome_risk <- 
    data_simple %>%
    group_by(COUNTRY_GROUP, BIOME) %>%
    summarise(DFRS_KHA = sum(BEEF_DEF_5_YEAR_HA)/1000) %>%
    ungroup() %>%
    filter(BIOME %in% c("Amazon","Cerrado","Atlantic Forest")) %>%
    mutate(COUNTRY_GROUP = case_when(COUNTRY_GROUP == "United States" ~ "USA",
                                     COUNTRY_GROUP == "United Kingdom" ~ "UK",
                                     TRUE ~ COUNTRY_GROUP),
           COUNTRY_GROUP = as_factor(COUNTRY_GROUP),
           COUNTRY_GROUP = fct_relevel(COUNTRY_GROUP,
                                       levels = rev(order_3c))) %>%
    ggplot(aes(COUNTRY_GROUP, DFRS_KHA, fill = BIOME)) +
    geom_bar(stat = "identity") + 
    theme_beef_with_axes() +
    coord_flip() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(), 
          legend.position = c(0.55,0.5)) + 
    ylab("Deforestation risk (ha)") + 
    scale_fill_manual(values = biome_colors,
                      breaks = c("Cerrado", "Atlantic Forest", "Amazon"),
                      labels = c("Cerrado", "Atl. Forest", "Amazon")) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, 55),
                       label = unit_format(unit = "k"),) +
    NULL
)


# (D) Bar chart of deforestation risk/product
(p_product_rel_risk <- 
    cd_all %>% 
    mutate(HS4 = str_sub(HS6_CODE, start = 1, end = 4),
           HS4_DESC = case_when(HS4 == "0102" ~ "Live cattle",
                                HS4 == "0201" ~ "Beef cuts",
                                HS4 == "0202" ~ "Beef cuts",
                                HS4 == "1602" ~ "Processed beef\nproducts",
                                HS4 == "0206" ~ "Offals", 
                                HS4 == "0210" ~ "Processed beef\nproducts", 
                                TRUE ~ NA_character_)) %>%
    group_by(YEAR, HS4_DESC, LINKED_CNPJ) %>%
    summarise(SUM_CWE_PER_FLOW = sum(CWE_PER_FLOW)) %>%
    left_join(supply_sheds_df_dfrs, by = c("LINKED_CNPJ", "YEAR")) %>%
    unnest() %>%
    mutate(DFRS_HA = SUM_DFRS_PER_TON * SUM_CWE_PER_FLOW / 1000) %>%
    group_by(HS4_DESC, YEAR, NUM_RANDOMISATION) %>%
    summarise(SUM_DFRS_HA = sum(DFRS_HA),
              DFRS_HA_PER_TON = SUM_DFRS_HA / (sum(SUM_CWE_PER_FLOW) / 1000)) %>%
    ungroup() %>%
    group_by(HS4_DESC, YEAR) %>%
    summarise(DFRS_HA_PER_KTON_MEAN = mean(DFRS_HA_PER_TON) * 1000,
              DFRS_HA_PER_KTON_05 = quantile(DFRS_HA_PER_TON, probs = c(0.05)) * 1000,
              DFRS_HA_PER_KTON_95 = quantile(DFRS_HA_PER_TON, probs = c(0.95)) * 1000,
              DFRS_HA_PER_KTON_33 = quantile(DFRS_HA_PER_TON, probs = c(0.33)) * 1000,
              DFRS_HA_PER_KTON_66 = quantile(DFRS_HA_PER_TON, probs = c(0.66)) * 1000
    ) %>% 
    ungroup() %>%
    mutate(YEAR = as.character(YEAR), 
           YEAR = as_factor(YEAR), 
           YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015")),
           HS4_DESC = as.factor(HS4_DESC),
           HS4_DESC = fct_relevel(HS4_DESC, 
                                  levels = c("Processed beef\nproducts","Live cattle","Offals","Beef cuts"))) %>%
    ggplot(aes(HS4_DESC, DFRS_HA_PER_KTON_MEAN, fill = YEAR)) +
    geom_bar(stat = "identity", 
             position=position_dodge(width=1)) + 
    geom_errorbar(aes(ymin = DFRS_HA_PER_KTON_05, ymax = DFRS_HA_PER_KTON_95), 
                  size = 0.5, width = 0, 
                  position=position_dodge(width=1)) + 
    theme_beef_with_axes() +
    coord_flip() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(), 
          legend.position = c(0.6,0.8)) + 
    ylab("Relative deforestation risk\n(ha/kton)") + 
    scale_fill_manual(values = year_colors, 
                      breaks = c("2015", "2016", "2017")) +
    scale_y_continuous(expand = c(0,0), 
                       limits = c(0, 170)) +
    geom_hline(yintercept = market_average_risk$DFRS_HA_PER_KTON_MEAN, 
               linetype = "dashed") +
    NULL
)


# (E) Total domestic vs export total deforestation risk
pD_tot <-
  markets_compared %>%
  mutate(MARKET = ifelse(MARKET == "DOMESTIC", "Domestic market", "Export markets")) %>%
  group_by(MARKET, YEAR) %>%
  summarise(SUM_DFRS_MEAN = mean(SUM_DFRS_HA),
            SUM_DFRS_05 = quantile(SUM_DFRS_HA, probs = c(0.05)),
            SUM_DFRS_95 = quantile(SUM_DFRS_HA, probs = c(0.95)),
            SUM_DFRS_33 = quantile(SUM_DFRS_HA, probs = c(0.33)),
            SUM_DFRS_66 = quantile(SUM_DFRS_HA, probs = c(0.66))
  ) %>% 
  ungroup() %>%
  mutate(YEAR = as.character(YEAR), 
         YEAR = as_factor(YEAR), 
         YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015"))) %>%
  ggplot(aes(MARKET, SUM_DFRS_MEAN, fill = as.factor(YEAR))) + 
  geom_bar(stat = "identity", 
           position=position_dodge(width=1)) + 
  geom_errorbar(aes(ymin = SUM_DFRS_05, ymax = SUM_DFRS_95), 
                size = 0.5, width = 0, 
                position=position_dodge(width=1)) + 
  theme_beef_with_axes() +
  coord_flip() +
  theme(axis.title.y = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "none",
        axis.line.x = element_line()) + 
  ylab("Deforestation risk (ha)") + 
  scale_fill_manual(values = year_colors, 
                    breaks = c("2015", "2016", "2017")) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 550e03),
                     label = unit_format(unit = "k",  scale = 1e-3)) +
  NULL


# (F) Relative domestic vs export total deforestation risk
pD_rel <- markets_compared %>%
  mutate(MARKET = ifelse(MARKET == "DOMESTIC", "Domestic market", "Export markets")) %>%
  group_by(MARKET, YEAR) %>%
  summarise(SUM_DFRS_MEAN = mean(DFRS_HA_PER_TON) * 1000,
            SUM_DFRS_05 = quantile(DFRS_HA_PER_TON, probs = c(0.05)) * 1000,
            SUM_DFRS_95 = quantile(DFRS_HA_PER_TON, probs = c(0.95)) * 1000,
            SUM_DFRS_33 = quantile(DFRS_HA_PER_TON, probs = c(0.33)) * 1000,
            SUM_DFRS_66 = quantile(DFRS_HA_PER_TON, probs = c(0.66)) * 1000
  ) %>% 
  ungroup() %>%
  mutate(YEAR = as.character(YEAR), 
         YEAR = as_factor(YEAR), 
         YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015"))) %>%
  ggplot(aes(MARKET, SUM_DFRS_MEAN, fill = as.factor(YEAR))) +  
  geom_bar(stat = "identity", 
           position=position_dodge(width=1)) + 
  geom_errorbar(aes(ymin = SUM_DFRS_05, ymax = SUM_DFRS_95), 
                size = 0.5, width = 0, 
                position=position_dodge(width=1)) + 
  theme_beef_with_axes() + 
  coord_flip() +
  theme(axis.title.y = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "none",
        axis.line.x = element_line()) + 
  ylab("Relative deforestation risk\n(ha/kton)") + 
  scale_fill_manual(values = year_colors, 
                    breaks = c("2015", "2016", "2017")) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max_dfrs_risk)) +
  NULL


# Multiplot
p_dfrs_multi_ab <- 
  cowplot::plot_grid(
    p_exporter_rel_dfrs_risk,
    p_cntry_scatter,
    labels = c("A","B"),
    nrow = 1,
    align = "h")
p_dfrs_multi_cdef <- 
  cowplot::plot_grid(
    p_cntry_biome_risk,
    p_product_rel_risk,
    pD_tot,
    pD_rel,
    labels = c("C","D","E","F"),
    rel_heights = c(1,0.65),
    rows = 2,
    align = "hv")
p_dfrs_multi_all <- 
  cowplot::plot_grid(
    p_dfrs_multi_ab, 
    p_dfrs_multi_cdef,
    nrow = 2,
    rel_heights = c(1,1.65)
  )


# Export
ggplot2::ggsave(paste0(output_dir, "/", Sys.Date(),"-FIGURE_4.tiff"), 
                units = "cm", 
                width = 17.8,
                height = 17.8,
                plot = p_dfrs_multi_all)


# tidy up
rm(p_dfrs_multi,
   p_exporter_rel_dfrs_risk,
   p_cntry_scatter,
   p_cntry_biome_risk,
   p_product_rel_risk,
   pD_rel, 
   pD_tot,
   order_3c)


# Figure 5 - China --------------------------------------------------------


# Disaggregation of China's sourcing


# Select data to plot
munis_to_plot <- 
  data_simple %>% 
  filter(COUNTRY_GROUP == "China") %>%
  mutate(COUNTRY = ifelse(COUNTRY == "China","Exports to\nChina (Mainland)", "Exports to\nChina (Hong Kong)"),
         COUNTRY = as_factor(COUNTRY), 
         COUNTRY = fct_relevel(COUNTRY, 
                               levels = c("Exports to\nChina (Mainland)", "Exports to\nChina (Hong Kong)"))) %>%
  group_by(COUNTRY) %>%
  mutate(TOTAL_CWE = sum(CWE)) %>%
  group_by(COUNTRY, GEOCODE) %>%
  summarise(SUM_CWE = sum(CWE),
            PERC_CWE = SUM_CWE / median(TOTAL_CWE) * 100) %>%
  ungroup() %>%
  inner_join(munis, ., by = "GEOCODE")
shs_to_plot <- 
  data_simple %>% 
  filter(COUNTRY_GROUP == "China") %>%
  mutate(COUNTRY = ifelse(COUNTRY == "China","Exports to\nChina (Mainland)", "Exports to\nChina (Hong Kong)"),
         COUNTRY = as_factor(COUNTRY), 
         COUNTRY = fct_relevel(COUNTRY, 
                               levels = c("Exports to\nChina (Mainland)", "Exports to\nChina (Hong Kong)"))) %>%
  group_by(COUNTRY, LH_CNPJ) %>%
  summarise(SUM_CWE_TONS  = sum(CWE) / 1000) %>%
  ungroup() %>%
  inner_join(shs, by = c("LH_CNPJ" = "SH_CNPJ")) %>%
  mutate(X = map_dbl(geometry, 1),
         Y = map_dbl(geometry, 2)) %>%
  select(-geometry) %>%
  as.data.frame() %>%
  unique()


# Figure A
# ... Map of China exports
p_china_muni <-
  ggplot() +
  geom_sf(data = munis_to_plot, col = NA, aes(fill = PERC_CWE)) +
  geom_sf(data = biomes, fill = NA) +
  geom_point(data = shs_to_plot, aes(x = X, y = Y, size = SUM_CWE_TONS / 1000), 
             fill = "lightgrey", 
             alpha = 0.5) +
  coord_sf(datum=NA) +
  theme_beef() +
  theme(legend.position = "left",
        axis.title = element_blank(),
        legend.box.margin=margin(-10,-10,-10,0),
        legend.spacing.y = unit(0, "cm"),
        strip.text = element_text(size = 12)) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 3.5)) + 
  facet_wrap(~COUNTRY) +
  scale_fill_viridis(option="magma",
                     name = bquote('Origin of\nsourcing (%)'),
                     na.value="white",
                     direction = -1) +
  scale_size_continuous(name = "ktons per\nslaughterhouse", 
                        breaks = c(40, 80, 120)) 


# Figure B
# ... barchart of sourcing per state
p_china_state <- 
  data_simple %>%
  filter(COUNTRY_GROUP == "China") %>%
  mutate(COUNTRY = ifelse(COUNTRY == "China","China (Mainland)", "China (Hong Kong)")) %>%
  group_by(COUNTRY) %>%
  mutate(TOTAL_CWE = sum(CWE)) %>%
  group_by(COUNTRY, STATE) %>%
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CWE)) %>%
  ungroup() %>%
  left_join(state_dic, by = c("STATE" = "STATE_NAME")) %>%
  mutate(
    STATE_ABBR = ifelse(is.na(STATE_ABBR), "UN", STATE_ABBR),
    STATE_ABBR = as_factor(STATE_ABBR),
    STATE_ABBR = fct_relevel(STATE_ABBR,
                             levels = state_order)
  ) %>%
  ggplot(aes(COUNTRY, PROP_CWE * 100, fill = STATE_ABBR)) +
  geom_bar(stat = "identity") + 
  theme_beef() + 
  coord_flip() + 
  ylab("State of origin of Brazilian beef exports (%)") + 
  theme(axis.title.y = element_blank(), 
        legend.position = "none", 
        legend.title = element_blank()) +
  scale_fill_manual(values = state_colors) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  guides(fill = guide_legend(ncol = 9), byrow = TRUE)


# Figure C
# ... slaughterhouse risks


# (A) Extract deforestation risk per SH for China/HK
ch_summary <- 
  data_simple %>%
  filter(COUNTRY %in% c("China", "Hong Kong")) %>% 
  mutate(COUNTRY = ifelse(COUNTRY == "China", "China\n(mainland)","Hong Kong")) %>%
  group_by(COUNTRY, LH_CNPJ) %>%
  summarise(RR_KTON = sum(BEEF_DEF_5_YEAR_HA, na.rm = T) / (sum(CWE) / 1000) * 1e03)  %>% 
  ungroup() 


# (B) Make df of new China slaughterhouses
china_new_shs <-
  tibble(
    DATA = c(
      # July 2019
      "93 – Cooperativa dos Produtores de Carne e Derivados de Gurupi – Gurupi (TO)",
      "112 – Rio Maria Refrigerator – Rio Maria (PA)",
      "411 – Redentor Refrigerator – Guarantã Do Norte (MT)",
      "431 – Minerva – Palmeira De Goiás (GO)",
      "791 – Minerva S/A – Rolim De Moura (RO)",
      "941 – Barra Mansa Comércio De Carnes E Derivados LTDA – Sertãozinho (SP)",
      "1440 – Agroindustrial Iguatemi Eirelli – Iguatemi (MS)",
      "1751 – Marfrig Global Foods – Tangará Da Serra (MT)",
      "1811 – Naturafrig Alimentos Ltda – Barra Do Bugres (MT)",
      "2015 – Marfrig Global Foods – Várzea Grande (MT)",
      "2437 – Masterboi Ltda – São Geraldo Araguaia (PA)",
      "2583 – Frigol – Água Azul Do Norte (PA)",
      "3215 – Plena Alimentos S.A – Paraíso Do Tocantins (TO)",
      "3941 – Agroindustrial De Alimentos S.A – Rondonópolis (MT)",
      "3974 – Naturafrig – Rochedo (MS)",
      "4490 – Vale Grande Indústria E Comércio De Alimentos – Matupá (MT)",
      "4554 – Mercúrio Alimentos – Castanhal (PA)",
      # Nov 2019
      "847 – MARFRIG GLOBAL FOODS – São Gabriel (RS)",
      "889 – FRIGORÍFICO SUL LTDA – Aparecida do Taboado (MS)",
      "1365 – NATURAFRIG ALIMENTOS LTDA – Pirapozinho (SP)",
      "1900 – Marfrig Global Foods – Pontes e Lacerda (MT)",
      "2058 – JBS S.A – Senador Canedo (GO)"
    )) %>%
  mutate(DATA = str_split(DATA, pattern = " – "),
         SIF_NUM = map_chr(DATA, 1),
         SIF_COMPANY = str_trans(map_chr(DATA, 2)),
         SIF_MUNI_STATE = map_chr(DATA, 3)) %>%
  mutate(SIF_STATE = gsub(".* \\(|\\)","",SIF_MUNI_STATE),
         SIF_MUNICIPALITY = str_trans(str_squish(gsub("\\(.*","",SIF_MUNI_STATE)))) %>%
  select(-SIF_MUNI_STATE, -DATA) %>%
  mutate_all(str_to_upper)


# Add CNPJ to new China-approved slaughterhouses
# (A) Match by SIF_NUM, STATE, & MUNICIPALITY
china_new_shs2 <- 
  china_new_shs %>%
  left_join(sifs_bov, by = c("SIF_NUM", "SIF_STATE" = "SIF_UF", "SIF_MUNICIPALITY"))


# (B) Then match by company name, number, and state
china_new_shs3 <- 
  china_new_shs2 %>% 
  filter(is.na(SIF_CNPJ)) %>% 
  mutate(SIF_COMPANY_CLEAN = fn_clean_spaced_names(SIF_COMPANY.x), 
         SIF_COMPANY_CLEAN = case_when(
           SIF_COMPANY_CLEAN == "AGROINDUSTRIAL DE ALIMENTOS" ~ "AGRA AGROINDUSTRIAL DE ALIMENTOS",
           SIF_COMPANY_CLEAN == "MARFRIG GLOBAL FOODS" ~ "MARFRIG",
           TRUE ~ SIF_COMPANY_CLEAN
         )) %>%
  select(SIF_NUM, 
         SIF_STATE,
         SIF_MUNICIPALITY,
         SIF_COMPANY_CLEAN,
         -SIF_COMPANY.y) %>%
  left_join(sifs_bov, by = c("SIF_NUM", "SIF_STATE" = "SIF_UF", "SIF_COMPANY_CLEAN"))


# Nb there are some cases where the slaughterhouse name doesn't match - I think these must have been sold
# ... and undergone an ownership change, since the SIF number and municipality match.
# ... e.g. SIF 2015 sold from Minerva -> Marfrig


# Look at remaining unmatched SHs
china_new_shs4 <- 
  china_new_shs3 %>% 
  filter(is.na(SIF_CNPJ)) %>%
  mutate(SIF_CNPJ = case_when(SIF_COMPANY_CLEAN == "NATURAFRIG" ~ "18626084000139",
                              SIF_COMPANY_CLEAN == "REDENTOR REFRIGERATOR"~ "02165984000196",
                              SIF_NUM == "1900" & SIF_COMPANY_CLEAN == "MARFRIG" ~ "03853896006423",
                              SIF_NUM == "2015" & SIF_COMPANY_CLEAN == "MARFRIG" ~ "67620377006317",
                              TRUE ~ NA_character_))


# Join together
# ... to make df with CNPJs for all China's new slaughterhouses
china_new_shs_all <- 
  bind_rows(china_new_shs2, china_new_shs3, china_new_shs4) %>%
  filter(!is.na(SIF_CNPJ))
if(any(is.na(china_new_shs_all$SIF_CNPJ))){stop("Matching is incomplete")}
if(nrow(china_new_shs_all) != nrow(china_new_shs)){stop("Duplicates potentially introduced")}
rm(china_new_shs2, china_new_shs3, china_new_shs4)


# For 'new' slaughterhouses
# ... if exist in the trade data, calculate dfrs risk as before.
# ... if don't exist, then use sigsif.
sigsif_dfrs_df <- 
  left_join(sigsif, prod_dfrs_df, by = "GEOCODE") %>% 
  mutate(DFRS_HA_PER_TON_5_YR = ifelse(is.na(DFRS_HA_PER_TON_5_YR), 0, DFRS_HA_PER_TON_5_YR)) %>%
  mutate(WEIGHTED_DFRS_HA_PER_TON_5_YR = PROP_FLOWS * DFRS_HA_PER_TON_5_YR) %>%
  group_by(STATE_OF_SLAUGHTER, YEAR) %>%
  summarise(RR_KTON = sum(WEIGHTED_DFRS_HA_PER_TON_5_YR) * 1e03) %>%
  group_by(STATE_OF_SLAUGHTER) %>%
  summarise(RR_KTON = mean(RR_KTON))
china_new_shs_dfrs <- 
  data_simple  %>%
  filter(LH_CNPJ %in% china_new_shs_all$SIF_CNPJ) %>% 
  group_by(LH_CNPJ) %>%
  summarise(RR_KTON = sum(BEEF_DEF_5_YEAR_HA, na.rm = T) / (sum(CWE) / 1000) * 1e03)  %>% 
  ungroup() 
china_new_shs_dfrs2 <- 
  china_new_shs_all %>% 
  select(SIF_NUM, SIF_STATE, SIF_CNPJ) %>%
  filter(!SIF_CNPJ %in% china_new_shs_dfrs$LH_CNPJ) %>%
  left_join(sigsif_dfrs_df, by = c("SIF_STATE" = "STATE_OF_SLAUGHTER")) %>%
  rename(LH_CNPJ = SIF_CNPJ) %>%
  bind_rows(china_new_shs_dfrs) %>%
  mutate(COUNTRY = "2019\napprovals\nfor China\n(mainland)")
if(nrow(china_new_shs_dfrs2) != nrow(china_new_shs_all)){stop("error in join")}


# Make data for plotting
ch_shs <- shs %>%
  as.data.frame() %>%
  select(LH_CNPJ = SH_CNPJ,
         SH_STATE, 
         SH_GEOCODE) %>%
  distinct()
ch_data_all <- 
  bind_rows(ch_summary, china_new_shs_dfrs2) %>%
  filter(!grepl("UNKNOWN", LH_CNPJ)) %>%
  left_join(ch_shs, by = c("LH_CNPJ")) %>%
  select(COUNTRY, LH_CNPJ, RR_KTON, SH_STATE, SH_GEOCODE) %>%
  mutate(TWO_DIGIT_CODE = str_sub(SH_GEOCODE, start = 1, end = 2)) %>%
  left_join(state_dic, by = "TWO_DIGIT_CODE")
ch_data_average <- 
  ch_data_all %>%
  group_by(COUNTRY) %>%
  summarise(RR_KTON = mean(RR_KTON)) %>%
  ungroup() %>%
  mutate(COUNTRY = as_factor(COUNTRY), 
         COUNTRY = fct_relevel(COUNTRY, 
                               levels = c("China\n(mainland)", "Hong Kong", 
                                          "2019\napprovals\nfor China\n(mainland)")))
rm(ch_shs)


# # Where are top risks among 'new' slaughterhouses?
# ch_data_all %>%
#   filter(COUNTRY == "Newly-licensed\nslaughterhouses\nChina (mainland)") %>%
#   arrange(desc(RR_KTON))


# Plot
# ... may need to relevel the states to specify order
p_CHINA_HK_RISKS <- 
  ch_data_all %>%
  mutate(COUNTRY = as_factor(COUNTRY), 
         COUNTRY = fct_relevel(COUNTRY, 
                               levels = c("China\n(mainland)", "Hong Kong", 
                                          "2019\napprovals\nfor China\n(mainland)"))) %>%
  ggplot(aes(RR_KTON, fill = STATE_ABBR)) + 
  geom_histogram() + 
  facet_grid(rows = vars(COUNTRY)) +
  theme(axis.title.y = element_text(angle = 90),
        legend.title = element_blank(), 
        legend.position = "none",
        strip.text = element_text(size = 12)) + 
  ylab("Number of slaughterhouses") + 
  xlab("Deforestation risk (ha/kton)") + 
  geom_vline(data = ch_data_average, aes(xintercept = RR_KTON), linetype = "dashed", col = "darkgrey") +
  geom_hline(yintercept = 0) +
  theme_beef_with_axes() +
  scale_fill_manual(values = state_colors) +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = guide_legend(nrow = 7), byrow = TRUE)


# Add (C) label
multiplot_china_c <- cowplot::plot_grid(p_CHINA_HK_RISKS, labels = "C")


# Extract legend
p_for_legend <-
  data_simple %>%
  filter(COUNTRY_GROUP == "China") %>%
  mutate(COUNTRY = ifelse(COUNTRY == "China","China (Mainland)", "China (Hong Kong)")) %>%
  group_by(COUNTRY) %>%
  mutate(TOTAL_CWE = sum(CWE)) %>%
  group_by(COUNTRY, STATE) %>%
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CWE)) %>%
  ungroup() %>%
  left_join(state_dic, by = c("STATE" = "STATE_NAME")) %>%
  mutate(
    STATE_ABBR = ifelse(is.na(STATE_ABBR), "UN", STATE_ABBR),
    STATE_ABBR = as_factor(STATE_ABBR),
    STATE_ABBR = fct_relevel(STATE_ABBR,
                             levels = state_order)
  ) %>%
  filter(!STATE_ABBR == "UN") %>%
  ggplot(aes(COUNTRY, PROP_CWE * 100, fill = STATE_ABBR)) +
  geom_bar(stat = "identity") + 
  theme_beef() + 
  coord_flip() + 
  ylab("State of origin of Brazilian beef exports (%)") + 
  theme(axis.title.y = element_blank(), 
        legend.position = "none", 
        legend.title = element_blank()) +
  scale_fill_manual(values = state_colors) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  theme(legend.position = "right") +
  guides(fill = guide_legend(nrow = 8), byrow = TRUE)
my_legend <- cowplot::get_legend(p_for_legend)


# Bring A & B plots together
multiplot_china_ab <- 
  cowplot::plot_grid(p_china_muni, p_china_state, 
                     nrow = 2, 
                     rel_heights = c(1, 0.5), 
                     labels = c("A","B"))

# Bring C together
multiplot_china_c <- 
  cowplot::plot_grid(p_CHINA_HK_RISKS, 
                     my_legend, 
                     labels = c("C", ""),
                     ncol = 2,
                     rel_widths = c(1, .4))


multiplot_china_all <- 
  cowplot::plot_grid(multiplot_china_ab, multiplot_china_c, 
                     nrow = 2, 
                     rel_heights = c(1.2, 1))


# Export
ggsave(plot = multiplot_china_all, 
       paste0(output_dir, "/", Sys.Date(), "-FIGURE_5.tiff"),
       units = "cm", 
       width = 17.8, 
       height = 21)


# Figure S1 - major meatpackers -------------------------------------------


# The sourcing of the three largest exporters:
# ... JBS, Minerva, and Marfrig


# Sum each company (and subsidiary's) sourcing per municipality
munis_to_plot <- 
  data_simple %>% 
  filter(EXPORTER_GROUP %in% c("JBS", "Minerva", "Marfrig"),
         PRODUCT_DESC != "Live cattle exports") %>%
  mutate(TOTAL_CWE = sum(CWE)) %>%
  group_by(EXPORTER_GROUP, GEOCODE) %>%
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CWE)) %>%
  ungroup() %>%
  inner_join(munis, ., by = "GEOCODE")


# Select slaughterhouses operated by big 3 in 2015-2017
big3_shs <- 
  data_simple %>% 
  filter(EXPORTER_GROUP %in% c("JBS","Minerva","Marfrig"),
         PRODUCT_DESC != "Live cattle exports") %>%
  group_by(EXPORTER_GROUP, LH_CNPJ) %>%
  summarise(SUM_CWE_TONS = sum(CWE) / 1000) %>%
  ungroup()
shs_to_plot <- 
  shs %>%
  inner_join(big3_shs, by = c("SH_CNPJ" = "LH_CNPJ")) %>% 
  mutate(
    X = map_dbl(geometry, 1),
    Y = map_dbl(geometry, 2)) %>%
  as.data.frame() %>%
  select(-geometry)


p_g4_sourcing <- 
  ggplot() + 
  geom_sf(
    data = munis_to_plot,
    aes(fill=PROP_CWE * 100),
    color = NA
  ) +
  geom_sf(data = states, 
          fill = NA, 
          size = 0.6) +
  geom_point(data = shs_to_plot, 
             aes(x = X, y = Y, size = SUM_CWE_TONS/1000),
             fill = "lightgrey", 
             alpha = 0.5) +
  coord_sf(datum=NA) +
  theme_beef() +
  theme(legend.position = "left",
        axis.title = element_blank()) +
  scale_fill_viridis(option="magma",
                     name = bquote('Origin of\nsourcing (%)'),
                     na.value="white",
                     direction = -1) +
  scale_size_continuous(name = "kton per\nslaughterhouse") +
  guides(fill = guide_colourbar(ticks = FALSE)) +
  facet_wrap(~EXPORTER_GROUP, ncol = 3) +
  NULL


# Save output
ggsave(paste0(output_dir,"/",Sys.Date(),"-FIGURE_S1.tiff"),
       unit = "cm",
       width = 17.8,
       height = 9.5,
       plot = p_g4_sourcing)


# tidy up 
rm(p_g4_sourcing, munis_to_plot, shs_to_plot, big3_shs)


# Figure S2 - accumulation curves -----------------------------------------


# Accumulation curve of each slaughterhouse's sourcing by distance
(p_ACCUM <- 
   slaughterhouse_sourcing_distances %>% 
   mutate(DISTANCE_KM = TOTAL_LENGTH / 1000) %>%
   filter(!is.na(SH_STATE), !is.na(DISTANCE_KM)) %>%
   ggplot(aes(DISTANCE_KM, CUMSUM_PROP_FLOWS, group = taxNum, col = SH_STATE)) + 
   geom_line(size = 0.8) + 
   facet_wrap(~SH_STATE) + 
   theme_beef() + 
   theme(legend.position = "none",
         axis.line = element_line()) +
   scale_x_continuous(limits = c(0,1000)) + 
   geom_hline(yintercept = 0) +
   ylab("Cumulative proportion\nof municipal sourcing") + 
   xlab("Distance (km)") +
   scale_colour_manual(values = state_colors) +
   scale_y_continuous(expand = c(0,0)) +
   NULL
)


# Export
ggsave(paste0(output_dir, "/", Sys.Date(),"-FIGURE_S2.tiff"),
       units = "cm", 
       width = 17.8,
       height = 11,
       plot = p_ACCUM)


# tidy up
rm(p_ACCUM)


# Figure S3 - origin per product ------------------------------------------


# Origin of different cattle products
munis_to_plot <- data_simple %>% 
  mutate(HS4 = str_sub(HS6_CODE, start = 1, end = 4),
         HS4_DESC = case_when(HS4 == "0102" ~ "Live cattle",
                              HS4 == "0201" ~ "Beef cuts",
                              HS4 == "0202" ~ "Beef cuts",
                              HS4 == "1602" ~ "Processed beef\nproducts",
                              HS4 == "0206" ~ "Offals", 
                              HS4 == "0210" ~ "Processed beef\nproducts", 
                              TRUE ~ NA_character_)) %>%
  group_by(HS4_DESC) %>%
  mutate(TOTAL_CW_TONS = sum(CWE)) %>%
  group_by(HS4_DESC, GEOCODE) %>% 
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CW_TONS)) %>% 
  ungroup() %>% 
  inner_join(munis, ., by = "GEOCODE")


# plot
p_product_sourcing <-
  ggplot() +
  geom_sf(data = munis_to_plot, col = NA, aes(fill = PROP_CWE * 100)) +
  geom_sf(data = states, fill = NA, color = "darkgrey") +
  coord_sf(datum=NA) +
  theme_beef() +
  theme(legend.position = c(0.1, 0.58),
        strip.text = element_text(size = 14),
        text= element_text(size=12, family="Avenir Medium")) +
  facet_wrap(~HS4_DESC, ncol = 2) +
  scale_fill_viridis(option="magma",
                     name = bquote('Origin of\nsourcing (%)'),
                     na.value="white",
                     direction = -1)
p_product_sourcing


# Plot of ports per product
port_order <- c("Barcarena", "Itajai", "Paranagua",
                "Rio Grande", "Santos", "Rio de Janeiro","Sao Francisco do Sul", 
                "Sao Luis", "Sao Sebastiao", "Other ports")  
p_port_proportions <- 
  data_ports %>%
  mutate(HS4_DESC = ifelse(HS4_DESC == "Fresh/frozen beef", "Beef cuts", HS4_DESC)) %>%
  group_by(HS4_DESC, PORT_AGGREGATED) %>% 
  summarise(PROP_FOB = sum(PROP_FOB)) %>%
  ungroup() %>%
  mutate(PORT_AGGREGATED = gsub("Do", "do", PORT_AGGREGATED),
         PORT_AGGREGATED = gsub("De", "de", PORT_AGGREGATED),
         PORT_AGGREGATED = as_factor(PORT_AGGREGATED),
         PORT_AGGREGATED = fct_relevel(PORT_AGGREGATED, 
                                       levels = port_order)) %>%
  ggplot(aes(HS4_DESC, PROP_FOB * 100, fill = PORT_AGGREGATED)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_beef_with_axes() + 
  theme(axis.title.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank()) + 
  ylab("Exports per port (%)") +
  scale_fill_manual(values=port_colors,
                    name = "Port") +
  scale_y_continuous(expand = c(0,0)) 
p_port_proportions


# Plot port locations
# ... these are taken as the municipal centre
port_locations <- 
  data_ports %>%
  mutate(HS4_DESC = ifelse(HS4_DESC == "Fresh/frozen beef", "Beef cuts", HS4_DESC)) %>%
  filter(PORT_AGGREGATED != "Other ports") %>%
  inner_join(munis_cent, by = c("PORT" = "NAME")) %>%
  mutate(PORT = str_to_title(PORT),
         PORT = gsub("Do", "do", PORT),
         PORT = gsub("De", "de", PORT))
p_port_locations <-
  ggplot() +
  geom_sf(data = states, fill = NA, color = "darkgrey") +
  geom_point(data = port_locations, aes(x = X, y = Y, fill = PORT),
             shape = 21,
             size = 3) +
  theme_beef() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank(),
        # legend.position = c(0.12, 0.25),
        legend.position = "none",
        legend.title = element_blank()
  ) +
  coord_sf(datum = NA,
           xlim = c(mapRange[1],mapRange[2]), 
           ylim = c(mapRange[3],mapRange[4])) +
  scale_fill_manual(values = port_colors)


# Multiplot port proportions and locations
p_port_multiplot <- 
  cowplot::plot_grid(p_port_proportions, p_port_locations,
                     rel_widths = c(1, 1),
                     labels = c("B","C"))
my_legend <- cowplot::get_legend(p_port_proportions + theme(legend.position = "bottom"))
p_ports_with_legend <- 
  cowplot::plot_grid(
    p_port_multiplot,
    my_legend,
    nrow = 2,
    rel_heights = c(1,0.3))


# Multiplot of product sourcing - municipalities and ports
p_products_and_ports <- 
  cowplot::plot_grid(
    p_product_sourcing,
    p_ports_with_legend,
    nrow = 2,
    rel_heights = c(1.6,1), 
    labels = c("A",""))


# Export
ggsave(paste0(output_dir, "/", Sys.Date(),"-FIGURE_S3.tiff"),
       units = "cm", 
       width = 17.8,
       height = 30,
       plot = p_products_and_ports)


# tidy up
rm(p_products_and_ports, p_ports_with_legend, my_legend, p_port_multiplot, port_colors, 
   port_locations, port_order, p_product_sourcing, p_port_proportions, p_port_locations)


# Figure S4 - EU sourcing -------------------------------------------------


# Map of EU sourcing
states_approved_for_processed_meat_in_2016 <- 
  c("ACRE", "RONDONIA","PARA", "TOCANTINS","MARANHAO","PIAUI","BAHIA",
    "CEARA","RIO GRANDE DO NORTE","PARAIBA", "PERNAMBUCO","ALAGOAS","SERGIPE")
states_approved_for_processed_meat_pre_2016 <- 
  c("GOIAS","MINAS GERAIS",  "ESPIRITO SANTO", "MATO GROSSO", "MATO GROSSO DO SUL","PARANA",
    "RIO GRANDE DO SUL", "SANTA CATARINA","SAO PAULO")
states_approved_for_fresh_meat_pre_2016 <- 
  c("MINAS GERAIS", "ESPIRITO SANTO","GOIAS", "MATO GROSSO","RIO GRANDE DO SUL","MATO GROSSO DO SUL",
    "SANTA CATARINA", "PARANA","SAO PAULO")
munis_approved_for_fresh_meat_in_2016 <- 
  c("PORTO MURTINHO","CARACOL","BELA VISTA","ANTONIO JOAO","PONTA PORA",
    "ARAL MOREIRA","CORONEL SAPUCAIA", "PARANHOS", "SETE QUEDAS","JAPORA","MUNDO NOVO",
    "CORUMBA","LADARIO")
states_eu_fresh <- 
  states %>% 
  filter(STATE_NAME %in% states_approved_for_fresh_meat_pre_2016)
states_eu_processed <- 
  states %>% 
  filter(STATE_NAME %in% c(states_approved_for_processed_meat_in_2016, 
                           states_approved_for_processed_meat_pre_2016)) %>%
  mutate(YEAR_PERMITTED = ifelse(STATE_NAME %in% states_approved_for_processed_meat_in_2016, "2016", "Pre-2016"))
munis_eu_fresh <- # These are in Mato Grosso do Sul, code 50
  munis %>%
  mutate(TWO_DIGIT_CODE = str_sub(GEOCODE, start = 1, end = 2)) %>%
  filter(NAME %in% munis_approved_for_fresh_meat_in_2016, 
         TWO_DIGIT_CODE == "50")  
stopifnot(nrow(munis_eu_fresh) == length(munis_approved_for_fresh_meat_in_2016))


# Plot
brazil <- st_union(states)
p_fresh_meat_eu <- 
  ggplot() + 
  geom_sf(data = brazil, fill = NA) +
  geom_sf(data = states_eu_fresh, 
          fill = "#045a8d") +
  geom_sf(data = munis_eu_fresh, 
          fill = "#74a9cf") +
  theme_beef() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  coord_sf(datum = NA,
           xlim = c(mapRange[1],mapRange[2]), 
           ylim = c(mapRange[3],mapRange[4])) +
  labs(title = "Beef cuts")
p_processed_meat_eu <- 
  ggplot() + 
  geom_sf(data = brazil, fill = NA) +
  geom_sf(data = states_eu_processed,
          aes(fill = YEAR_PERMITTED)) +
  theme_beef() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(), 
        legend.position = c(0.2, 0.2)) +
  coord_sf(datum = NA,
           xlim = c(mapRange[1],mapRange[2]), 
           ylim = c(mapRange[3],mapRange[4])) +
  scale_fill_manual(values = c("#74a9cf","#045a8d"), 
                    name = "Year approved\nfor export\nto EU") + 
  labs(title = "Processed beef products")


# Multiplot
p_eu_approved_sourcing <- 
  cowplot::plot_grid(p_fresh_meat_eu, p_processed_meat_eu)
p_eu_approved_sourcing


# Export
ggsave(paste0(output_dir, "/", Sys.Date(), "-FIGURE_S3.tiff"),
       units = "cm", 
       width = 17.8, 
       height = 12,
       plot = p_eu_approved_sourcing)


# tidy up
rm(p_eu_approved_sourcing, p_fresh_meat_eu, p_processed_meat_eu,
   states_eu_fresh, states_eu_processed, munis_eu_fresh,
   states_approved_for_processed_meat_in_2016,
   states_approved_for_processed_meat_pre_2016,
   states_approved_for_fresh_meat_pre_2016,
   munis_approved_for_fresh_meat_in_2016)


# Figure S5 - non-uniform risks -------------------------------------------


# A comparison of deforestation risk when estimated with subnational or national-average approaches


# Sum the exported carcass per state
exp_per_geocode <- 
  data_simple %>%
  filter(GEOCODE != "UNKNOWN") %>%
  group_by(GEOCODE, YEAR) %>% 
  summarise(EXPORTED_CWE_TONS = sum(CWE) / 1000) %>%
  ungroup()


# Calculate the national average cattle-associated deforestation (ha/ton/year)
national_average_pasture_dfrs <- 
  left_join(prod, prod_dfrs_df, by = c("GEOCODE","YEAR")) %>%
  mutate(
    DFRS_HA_PER_TON_5_YR = ifelse(is.na(DFRS_HA_PER_TON_5_YR), 0, DFRS_HA_PER_TON_5_YR),
  ) %>%
  group_by(YEAR) %>%
  mutate(TOTAL_CW_PRODUCTION = sum(CW_PRODUCTION_TONS_5_YR)) %>%
  ungroup() %>%
  mutate(PROP_CW_PRODUCTION = CW_PRODUCTION_TONS_5_YR / TOTAL_CW_PRODUCTION,
         WEIGHTED_DFRS_HA_PER_TON_5_YR = PROP_CW_PRODUCTION * DFRS_HA_PER_TON_5_YR) %>%
  group_by(YEAR) %>%
  summarise(DFRS_HA_PER_TON = sum(WEIGHTED_DFRS_HA_PER_TON_5_YR)) %>%
  ungroup() 


# Calculate the deforestation risk of export markets, using the national-average cattle-associated deforestation 
export_national_risk <- 
  data_simple %>% 
  group_by(YEAR, COUNTRY_GROUP) %>%
  summarise(SUM_CWE = sum(CWE)) %>%
  left_join(national_average_pasture_dfrs, by = "YEAR") %>%
  unnest() %>%
  mutate(DFRS_HA = DFRS_HA_PER_TON * SUM_CWE / 1000) %>%
  group_by(COUNTRY_GROUP, YEAR) %>%
  summarise(SUM_DFRS_HA = sum(DFRS_HA)) %>%
  ungroup() %>%
  mutate(SOURCE = "National average", 
         YEAR = paste0(YEAR, " - ", SOURCE)) 


# Calculate the domestic market's risk using national-average and subnational deforestation risk
dom_mrkt_production <- 
  prod %>%
  filter(CW_PRODUCTION_TONS_5_YR != 0) %>%
  left_join(exp_per_geocode, by = c("GEOCODE", "YEAR")) %>%
  filter(YEAR %in% c(2015:2017)) %>%
  mutate(EXPORTED_CWE_TONS = ifelse(is.na(EXPORTED_CWE_TONS), 0, EXPORTED_CWE_TONS),
         CWE_PRODUCTION_TONS = CW_PRODUCTION_TONS_5_YR / 5, 
         DOMESTIC_CWE_TONS = CWE_PRODUCTION_TONS - EXPORTED_CWE_TONS, 
         DOMESTIC_CWE_TONS = ifelse(DOMESTIC_CWE_TONS < 0, 0, DOMESTIC_CWE_TONS)) 
dom_mrkt_national_risk <-
  dom_mrkt_production %>%
  group_by(YEAR) %>% 
  summarise(DOMESTIC_CWE_TONS = sum(DOMESTIC_CWE_TONS)) %>% 
  ungroup() %>%
  left_join(national_average_pasture_dfrs, by = "YEAR") %>%
  mutate(SUM_DFRS_HA = DOMESTIC_CWE_TONS * DFRS_HA_PER_TON,
         SOURCE = "National average", 
         COUNTRY_GROUP = "Domestic\nmarket") %>%
  select(COUNTRY_GROUP, YEAR, SUM_DFRS_HA, SOURCE) %>%
  mutate(YEAR = as.character(YEAR))
dom_mrkt_subnational_risk <-
  left_join(dom_mrkt_production, prod_dfrs_df, by = c("GEOCODE", "YEAR")) %>%
  mutate(DFRS_HA = DOMESTIC_CWE_TONS * DFRS_HA_PER_TON_5_YR ) %>% 
  group_by(YEAR) %>%
  summarise(SUM_DFRS_HA = sum(DFRS_HA)) %>% 
  ungroup() %>%
  mutate(SOURCE = "Subnational", 
         COUNTRY_GROUP = "Domestic\nmarket") %>%
  mutate(YEAR = as.character(YEAR))


# Calculate the average % difference
top_10_cntry_grp_abbreviated <- 
  c("Domestic\nmarket","China", "Egypt", "Russia", "Iran", "USA","Chile","Venezuela",
    "Italy","Netherlands","UK","Other EU", "Other halal","Other")
comparison_national_subnational_df <- 
  data_simple %>% 
  group_by(YEAR, COUNTRY_GROUP) %>%
  summarise(SUM_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA)) %>%
  ungroup() %>%
  mutate(SOURCE = "Subnational",
         YEAR = paste0(YEAR, " - ",SOURCE)) %>%
  bind_rows(export_national_risk, 
            dom_mrkt_national_risk, 
            dom_mrkt_subnational_risk) %>%
  mutate(YEAR = str_sub(YEAR, start = 1, end = 4)) %>%
  mutate(YEAR = as.character(YEAR), 
         YEAR = as_factor(YEAR), 
         YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015")),
         COUNTRY_GROUP = case_when(COUNTRY_GROUP == "United States" ~ "USA",
                                   COUNTRY_GROUP == "United Kingdom" ~ "UK",
                                   COUNTRY_GROUP == "Other EU markets" ~ "Other EU",
                                   COUNTRY_GROUP == "Other halal markets" ~ "Other halal",
                                   COUNTRY_GROUP == "Other markets" ~ "Other",
                                   TRUE ~ COUNTRY_GROUP),
         COUNTRY_GROUP = as_factor(COUNTRY_GROUP),
         COUNTRY_GROUP = fct_relevel(COUNTRY_GROUP,
                                     levels = rev(top_10_cntry_grp_abbreviated)),
         COUNTRY_GROUP_2 = ifelse(COUNTRY_GROUP %in% c("USA", "UK"), "GROUP 2", "GROUP 1")) %>% 
  spread(SOURCE, SUM_DFRS_HA) %>%
  mutate(PERC_DIFFERENCE = (Subnational - `National average` ) / Subnational * 100 )


# Plot the % difference between the deforestation risk when calculated using subnational vs national-avergae data
# ... NB the US and UK results are plotted with a different x-axis, to allow visualisation of differences for
# ... other markets (which they dwarf).
p1 <- 
  comparison_national_subnational_df %>%
  filter(COUNTRY_GROUP_2 == "GROUP 1") %>%
  ggplot(aes(COUNTRY_GROUP, PERC_DIFFERENCE, fill = YEAR)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() +
  theme_beef_with_axes() + 
  theme(axis.title = element_blank(),
        legend.title = element_blank(), 
        legend.position =  c(0.2, 0.8)) + 
  scale_fill_manual(values = year_colors, 
                    breaks = c("2015", "2016", "2017")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(label = unit_format(unit = "%"), 
                     limits = c(-150, 50))
p2 <- 
  comparison_national_subnational_df %>%
  filter(COUNTRY_GROUP_2 == "GROUP 2") %>%
  ggplot(aes(COUNTRY_GROUP, PERC_DIFFERENCE, fill = YEAR)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() +
  theme_beef_with_axes() + 
  theme(axis.title.y = element_blank(),
        legend.position = "none") + 
  ylab("Percentage difference in deforestation risk calculated using:\nSubnational supply chain data\nvs\nAssumed nationwide sourcing") +
  scale_fill_manual(values = year_colors, 
                    breaks = c("2015", "2016", "2017")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    label = unit_format(unit = "%"))
multiplot <- 
  cowplot::plot_grid(p1, p2, 
                     ncol = 1, 
                     rel_heights = c(1, 0.4), 
                     align = "v")


# Export
ggsave(paste0(output_dir, "/", Sys.Date(),"-FIGURE_S5.tiff"),
       units = "cm", 
       width = 17.8,
       height = 17.8,
       plot = multiplot)


# tidy up
rm(multiplot, p1, p2, 
   export_national_risk, 
   dom_mrkt_national_risk, 
   dom_mrkt_subnational_risk, 
   comparison_national_subnational_df)


# Figure S6 - dfrs risk per actor -----------------------------------------


# Deforestation risks, per actor


# Select upper limit for relative risk
# ... used to standardize axis scales
max_dfrs_risk <- 
  cd_all %>% 
  mutate(EXPORTER_GROUP = case_when(EXPORTER %in% top_10_exporter ~ EXPORTER, 
                                    TRUE ~ "Other exporters")) %>%
  group_by(YEAR, EXPORTER_GROUP, LINKED_CNPJ) %>%
  summarise(SUM_CWE_PER_FLOW = sum(CWE_PER_FLOW)) %>%
  left_join(supply_sheds_df_dfrs, by = c("LINKED_CNPJ", "YEAR")) %>%
  unnest() %>%
  mutate(DFRS_HA = SUM_DFRS_PER_TON * SUM_CWE_PER_FLOW / 1000) %>%
  group_by(EXPORTER_GROUP, YEAR, NUM_RANDOMISATION) %>%
  summarise(SUM_DFRS_HA = sum(DFRS_HA),
            DFRS_HA_PER_TON = SUM_DFRS_HA / (sum(SUM_CWE_PER_FLOW)/1000)) %>%
  ungroup() %>%
  group_by(EXPORTER_GROUP, YEAR) %>%
  summarise(DFRS_HA_PER_KTON_95 = quantile(DFRS_HA_PER_TON, probs = c(0.95)) * 1000) %>% 
  ungroup() %>%
  filter(DFRS_HA_PER_KTON_95 == max(DFRS_HA_PER_KTON_95)) %>%
  pull(DFRS_HA_PER_KTON_95) + 5


# (A) Per exporter


# Plot total deforestation risk
abbr_exporter_order <- 
  c("JBS", "Minerva", "Marfrig", "Prima Foods",
    "Meat Snack Partners", "Mato Grosso Bovinos", "Pampeano Alimentos",
    "Irmãos Gonçalves", "Mercurio Alimentos", "Frisa")
(pE_tot <- 
   cd_all %>% 
   mutate(EXPORTER_GROUP = case_when(EXPORTER %in% top_10_exporter ~ EXPORTER, 
                                     TRUE ~ "Other exporters")) %>%
   mutate(EXPORTER_GROUP = case_when(EXPORTER_GROUP == "Meat Snack Partners do Brasil" ~ "Meat Snack Partners",
                                     EXPORTER_GROUP ==  "Irmaos Goncalves Comercio e Industria" ~  "Irmãos Gonçalves",
                                     EXPORTER_GROUP ==  "Frisa Frigorifico Rio Doce" ~  "Frisa",
                                     EXPORTER_GROUP ==  "Mataboi Alimentos" ~  "Prima Foods",
                                     TRUE ~ EXPORTER_GROUP)) %>%
   group_by(YEAR, EXPORTER_GROUP, LINKED_CNPJ) %>%
   summarise(SUM_CWE_PER_FLOW = sum(CWE_PER_FLOW)) %>%
   left_join(supply_sheds_df_dfrs, by = c("LINKED_CNPJ", "YEAR")) %>%
   unnest() %>%
   mutate(DFRS_HA = SUM_DFRS_PER_TON * SUM_CWE_PER_FLOW / 1000) %>%
   group_by(EXPORTER_GROUP, YEAR, NUM_RANDOMISATION) %>%
   summarise(SUM_DFRS_HA = sum(DFRS_HA)) %>%
   ungroup() %>%
   group_by(EXPORTER_GROUP, YEAR) %>%
   summarise(SUM_DFRS_MEAN = mean(SUM_DFRS_HA),
             SUM_DFRS_05 = quantile(SUM_DFRS_HA, probs = c(0.05)),
             SUM_DFRS_95 = quantile(SUM_DFRS_HA, probs = c(0.95)),
             SUM_DFRS_33 = quantile(SUM_DFRS_HA, probs = c(0.33)),
             SUM_DFRS_66 = quantile(SUM_DFRS_HA, probs = c(0.66))
   ) %>% 
   ungroup() %>%
   mutate(EXPORTER_GROUP = as_factor(EXPORTER_GROUP), 
          EXPORTER_GROUP = fct_relevel(EXPORTER_GROUP, levels = rev(c(abbr_exporter_order, "Other exporters"))),
          YEAR = as.character(YEAR), 
          YEAR = as_factor(YEAR), 
          YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015"))) %>%
   ggplot(aes(EXPORTER_GROUP, SUM_DFRS_MEAN, fill = YEAR)) +
   geom_bar(stat = "identity", 
            position="dodge") + 
   geom_errorbar(aes(ymin = SUM_DFRS_05, ymax = SUM_DFRS_95), 
                 size = 0.5, width = 0, 
                 position=position_dodge(width=1)) + 
   theme_beef_with_axes() + 
   coord_flip() +
   theme(axis.title = element_blank(), 
         legend.title = element_blank(), 
         # legend.position = c(0.8,0.35)) + 
         legend.position = "none") + 
   ylab("Deforestation risk (ha/year)") + 
   scale_fill_manual(values = year_colors, 
                     breaks = c("2015", "2016", "2017")) +
   scale_y_continuous(label=comma, 
                      expand = c(0, 0),
                      limits = c(0, 30000)) +
   NULL
)


# Relative deforestation risk
(pE_rel <- 
    cd_all %>% 
    mutate(EXPORTER_GROUP = case_when(EXPORTER %in% top_10_exporter ~ EXPORTER, 
                                      TRUE ~ "Other exporters")) %>%
    mutate(EXPORTER_GROUP = case_when(EXPORTER_GROUP == "Meat Snack Partners do Brasil" ~ "Meat Snack Partners",
                                      EXPORTER_GROUP ==  "Irmaos Goncalves Comercio e Industria" ~  "Irmãos Gonçalves",
                                      EXPORTER_GROUP ==  "Frisa Frigorifico Rio Doce" ~  "Frisa",
                                      EXPORTER_GROUP ==  "Mataboi Alimentos" ~  "Prima Foods",
                                      TRUE ~ EXPORTER_GROUP)) %>%
    group_by(YEAR, EXPORTER_GROUP, LINKED_CNPJ) %>%
    summarise(SUM_CWE_PER_FLOW = sum(CWE_PER_FLOW)) %>%
    left_join(supply_sheds_df_dfrs, by = c("LINKED_CNPJ", "YEAR")) %>%
    unnest() %>%
    mutate(DFRS_HA = SUM_DFRS_PER_TON * SUM_CWE_PER_FLOW / 1000) %>%
    group_by(EXPORTER_GROUP, YEAR, NUM_RANDOMISATION) %>%
    summarise(SUM_DFRS_HA = sum(DFRS_HA),
              DFRS_HA_PER_TON = SUM_DFRS_HA / (sum(SUM_CWE_PER_FLOW)/1000)) %>%
    ungroup() %>%
    group_by(EXPORTER_GROUP, YEAR) %>%
    summarise(DFRS_HA_PER_KTON_MEAN = mean(DFRS_HA_PER_TON) * 1000,
              DFRS_HA_PER_KTON_05 = quantile(DFRS_HA_PER_TON, probs = c(0.05)) * 1000,
              DFRS_HA_PER_KTON_95 = quantile(DFRS_HA_PER_TON, probs = c(0.95)) * 1000,
              DFRS_HA_PER_KTON_33 = quantile(DFRS_HA_PER_TON, probs = c(0.33)) * 1000,
              DFRS_HA_PER_KTON_66 = quantile(DFRS_HA_PER_TON, probs = c(0.66)) * 1000
    ) %>% 
    ungroup() %>%
    mutate(EXPORTER_GROUP = as_factor(EXPORTER_GROUP), 
           EXPORTER_GROUP = fct_relevel(EXPORTER_GROUP, levels = rev(c(abbr_exporter_order, "Other exporters"))),
           YEAR = as.character(YEAR), 
           YEAR = as_factor(YEAR), 
           YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015"))) %>%
    ggplot(aes(EXPORTER_GROUP, DFRS_HA_PER_KTON_MEAN, fill = YEAR)) +
    geom_bar(stat = "identity", 
             position="dodge") + 
    geom_errorbar(aes(ymin = DFRS_HA_PER_KTON_05, ymax = DFRS_HA_PER_KTON_95), 
                  size = 0.5, width = 0, 
                  position=position_dodge(width=1)) + 
    theme_beef_with_axes() + 
    coord_flip() +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank(), 
          legend.position = "none") + 
    ylab("Relative deforestation risk\n(ha/kton)") + 
    scale_fill_manual(values = year_colors, 
                      breaks = c("2015", "2016", "2017")) +
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, max_dfrs_risk)) +
    NULL
)


# (B) Per 10 market


# Total dfrs risk
(pC_tot <- 
    cd_all %>% 
    group_by(YEAR, COUNTRY_GROUP, LINKED_CNPJ) %>%
    summarise(SUM_CWE_PER_FLOW = sum(CWE_PER_FLOW)) %>%
    left_join(supply_sheds_df_dfrs, by = c("LINKED_CNPJ", "YEAR")) %>%
    unnest() %>%
    mutate(DFRS_HA = SUM_DFRS_PER_TON * SUM_CWE_PER_FLOW / 1000) %>%
    group_by(COUNTRY_GROUP, YEAR, NUM_RANDOMISATION) %>%
    summarise(SUM_DFRS_HA = sum(DFRS_HA)) %>%
    ungroup() %>%
    group_by(COUNTRY_GROUP, YEAR) %>%
    summarise(SUM_DFRS_MEAN = mean(SUM_DFRS_HA),
              SUM_DFRS_05 = quantile(SUM_DFRS_HA, probs = c(0.05)),
              SUM_DFRS_95 = quantile(SUM_DFRS_HA, probs = c(0.95)),
              SUM_DFRS_33 = quantile(SUM_DFRS_HA, probs = c(0.33)),
              SUM_DFRS_66 = quantile(SUM_DFRS_HA, probs = c(0.66))
    ) %>% 
    ungroup() %>%
    mutate(COUNTRY_GROUP = as_factor(COUNTRY_GROUP), 
           COUNTRY_GROUP = fct_relevel(COUNTRY_GROUP, levels = rev(top_10_cntry_grp)),
           YEAR = as.character(YEAR), 
           YEAR = as_factor(YEAR), 
           YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015"))) %>%
    ggplot(aes(COUNTRY_GROUP, SUM_DFRS_MEAN, fill = YEAR)) +
    geom_bar(stat = "identity", 
             position="dodge") + 
    geom_errorbar(aes(ymin = SUM_DFRS_05, ymax = SUM_DFRS_95), 
                  size = 0.5, width = 0, 
                  position=position_dodge(width=1)) + 
    theme_beef() + 
    coord_flip() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(), 
          legend.position = c(0.8,0.35),
          axis.line.x = element_line()) + 
    ylab("Deforestation risk (ha/year)") + 
    scale_fill_manual(values = year_colors, 
                      breaks = c("2015", "2016", "2017")) +
    scale_y_continuous(label=comma,
                       expand = c(0,0)) +
    NULL
)


# Relative deforestation risk
(pC_rel <- 
    cd_all %>% 
    group_by(YEAR, COUNTRY_GROUP, LINKED_CNPJ) %>%
    summarise(SUM_CWE_PER_FLOW = sum(CWE_PER_FLOW)) %>%
    left_join(supply_sheds_df_dfrs, by = c("LINKED_CNPJ", "YEAR")) %>%
    unnest() %>%
    mutate(DFRS_HA = SUM_DFRS_PER_TON * SUM_CWE_PER_FLOW / 1000) %>%
    group_by(COUNTRY_GROUP, YEAR, NUM_RANDOMISATION) %>%
    summarise(SUM_DFRS_HA = sum(DFRS_HA),
              DFRS_HA_PER_TON = SUM_DFRS_HA / (sum(SUM_CWE_PER_FLOW) / 1000)) %>%
    ungroup() %>%
    group_by(COUNTRY_GROUP, YEAR) %>%
    summarise(DFRS_HA_PER_KTON_MEAN = mean(DFRS_HA_PER_TON) * 1000,
              DFRS_HA_PER_KTON_05 = quantile(DFRS_HA_PER_TON, probs = c(0.05)) * 1000,
              DFRS_HA_PER_KTON_95 = quantile(DFRS_HA_PER_TON, probs = c(0.95)) * 1000,
              DFRS_HA_PER_KTON_33 = quantile(DFRS_HA_PER_TON, probs = c(0.33)) * 1000,
              DFRS_HA_PER_KTON_66 = quantile(DFRS_HA_PER_TON, probs = c(0.66)) * 1000
    ) %>% 
    ungroup() %>%
    mutate(COUNTRY_GROUP = as_factor(COUNTRY_GROUP), 
           COUNTRY_GROUP = fct_relevel(COUNTRY_GROUP, 
                                       levels = rev(top_10_cntry_grp)),
           YEAR = as.character(YEAR), 
           YEAR = as_factor(YEAR), 
           YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015"))) %>%
    ggplot(aes(COUNTRY_GROUP, DFRS_HA_PER_KTON_MEAN, fill = YEAR)) +
    geom_bar(stat = "identity", 
             position="dodge") + 
    geom_errorbar(aes(ymin = DFRS_HA_PER_KTON_05, ymax = DFRS_HA_PER_KTON_95), 
                  size = 0.5, width = 0, 
                  position=position_dodge(width=1)) + 
    theme_beef() + 
    coord_flip() +
    theme(axis.title.y = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank(), 
          legend.position = "none",
          axis.line.x = element_line()) + 
    ylab("Relative deforestation risk (ha/kton)") + 
    scale_fill_manual(values = year_colors,
                      breaks = c("2015","2016","2017")) +
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, max_dfrs_risk)) +
    NULL
)


# (C) Per domestic/export markets


# Relative domestic vs export total deforestation risk
pD_rel <- markets_compared %>%
  mutate(MARKET = ifelse(MARKET == "DOMESTIC", "Domestic market", "Export markets")) %>%
  group_by(MARKET, YEAR) %>%
  summarise(SUM_DFRS_MEAN = mean(DFRS_HA_PER_TON) * 1000,
            SUM_DFRS_05 = quantile(DFRS_HA_PER_TON, probs = c(0.05)) * 1000,
            SUM_DFRS_95 = quantile(DFRS_HA_PER_TON, probs = c(0.95)) * 1000,
            SUM_DFRS_33 = quantile(DFRS_HA_PER_TON, probs = c(0.33)) * 1000,
            SUM_DFRS_66 = quantile(DFRS_HA_PER_TON, probs = c(0.66)) * 1000
  ) %>% 
  ungroup() %>%
  mutate(YEAR = as.character(YEAR), 
         YEAR = as_factor(YEAR), 
         YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015"))) %>%
  ggplot(aes(MARKET, SUM_DFRS_MEAN, fill = as.factor(YEAR))) +  
  geom_bar(stat = "identity", 
           position="dodge") + 
  geom_errorbar(aes(ymin = SUM_DFRS_05, ymax = SUM_DFRS_95), 
                size = 0.5, width = 0, 
                position=position_dodge(width=1)) + 
  theme_beef() + 
  coord_flip() +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "none",
        axis.line.x = element_line()) + 
  ylab("Relative deforestation risk\n(ha/kton)") + 
  scale_fill_manual(values = year_colors, 
                    breaks = c("2015", "2016", "2017")) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max_dfrs_risk)) +
  NULL


# Total domestic vs export total deforestation risk
pD_tot <-
  markets_compared %>%
  mutate(MARKET = ifelse(MARKET == "DOMESTIC", "Domestic market", "Export markets")) %>%
  group_by(MARKET, YEAR) %>%
  summarise(SUM_DFRS_MEAN = mean(SUM_DFRS_HA),
            SUM_DFRS_05 = quantile(SUM_DFRS_HA, probs = c(0.05)),
            SUM_DFRS_95 = quantile(SUM_DFRS_HA, probs = c(0.95)),
            SUM_DFRS_33 = quantile(SUM_DFRS_HA, probs = c(0.33)),
            SUM_DFRS_66 = quantile(SUM_DFRS_HA, probs = c(0.66))
  ) %>% 
  ungroup() %>%
  mutate(YEAR = as.character(YEAR), 
         YEAR = as_factor(YEAR), 
         YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015"))) %>%
  ggplot(aes(MARKET, SUM_DFRS_MEAN, fill = as.factor(YEAR))) + 
  geom_bar(stat = "identity", 
           position="dodge") + 
  geom_errorbar(aes(ymin = SUM_DFRS_05, ymax = SUM_DFRS_95), 
                size = 0.5, width = 0, 
                position=position_dodge(width=1)) + 
  theme_beef() +
  coord_flip() +
  theme(axis.title.y = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "none",
        axis.line.x = element_line()) + 
  ylab("Deforestation risk (ha)") + 
  scale_fill_manual(values = year_colors, 
                    breaks = c("2015", "2016", "2017")) +
  scale_y_continuous(label=comma, 
                     expand = c(0,0),
                     limits = c(0, 600000)) +
  NULL


# Use patchwork to compose plot
layout <- '
AB
CD
EF
'
p_all <- 
  pE_tot + pE_rel + pC_tot +  pC_rel + pD_tot + pD_rel + 
  plot_layout(design = layout,
              widths = c(1, 1), 
              heights = c(0.84,1,0.2)) +
  plot_annotation(tag_levels = 'A')


# Export
ggplot2::ggsave(paste0(output_dir,"/",Sys.Date(),"-FIGURE_S6.tiff"), 
                units = "cm", 
                width = 17.8,
                height = 30,
                plot = p_all)


# tidy up
rm(markets_compared, pC_tot, pC_rel, pD_tot, pD_rel, 
   pE_tot, pE_rel, layout,
   max_dfrs_risk)


# Figure S7 - origin of exports vs domestic market ------------------------


# Comparison of weighted mean statistics of municipalities
# ... supplying the domestic and export markets


# Plot risk of domestic/export markets per biome 
exports_per_geocode <- 
  data_simple %>% 
  group_by(YEAR, GEOCODE) %>%
  summarise(EXPORTED_CWE_TONS = sum(CWE) / 1000) %>%
  ungroup()
production_per_geocode <- 
  prod %>%
  mutate(PRODUCTION_TONS = CW_PRODUCTION_TONS_5_YR / 5) %>% 
  left_join(exports_per_geocode, by = c("YEAR","GEOCODE")) %>%
  mutate(EXPORTED_CWE_TONS = ifelse(is.na(EXPORTED_CWE_TONS), 0, EXPORTED_CWE_TONS),
         DOMESTIC_CWE_TONS = PRODUCTION_TONS - EXPORTED_CWE_TONS) %>%
  select(YEAR, GEOCODE, DOMESTIC_CWE_TONS, EXPORTED_CWE_TONS) %>%
  left_join(muni_biomes, by = c("GEOCODE")) %>% 
  filter(YEAR %in% c(2015:2017))


# Plot barchart of deforestation risk per biome/market
risk_per_biome <- 
  production_per_geocode %>% 
  gather(MARKET, CWE_TONS, DOMESTIC_CWE_TONS:EXPORTED_CWE_TONS) %>%
  mutate(MARKET = ifelse(MARKET == "DOMESTIC_CWE_TONS","Domestic market","Exports")) %>%
  left_join(prod_dfrs_df, by = c("YEAR","GEOCODE")) %>%
  mutate(DFRS_HA_PER_TON_5_YR = ifelse(is.na(DFRS_HA_PER_TON_5_YR), 0, DFRS_HA_PER_TON_5_YR)) %>%
  group_by(MARKET, BIOME) %>%
  mutate(TOTAL_CWE = sum(CWE_TONS)) %>%
  ungroup() %>% 
  mutate(PROP_MARKET = CWE_TONS / TOTAL_CWE,
         WEIGHTED_DFRS_RISK_PER_TON = PROP_MARKET * DFRS_HA_PER_TON_5_YR ) %>%
  group_by(MARKET, BIOME) %>%
  summarise(SUM_PROP_MARKET = sum(PROP_MARKET), 
            SUM_WEIGHTED_DFRS_RISK_PER_TON = sum(WEIGHTED_DFRS_RISK_PER_TON)) %>%
  ungroup() %>%
  filter(BIOME %in% c("Amazon", "Cerrado","Atlantic Forest"))
p_dfrs_per_biome_market <-
  production_per_geocode %>% 
  gather(MARKET, CWE_TONS, DOMESTIC_CWE_TONS:EXPORTED_CWE_TONS) %>%
  mutate(MARKET = ifelse(MARKET == "DOMESTIC_CWE_TONS","Domestic market","Exports")) %>%
  left_join(prod_dfrs_df, by = c("YEAR","GEOCODE")) %>%
  mutate(DFRS_HA_PER_TON_5_YR = ifelse(is.na(DFRS_HA_PER_TON_5_YR), 0, DFRS_HA_PER_TON_5_YR)) %>%
  group_by(MARKET) %>%
  mutate(TOTAL_CWE = sum(CWE_TONS)) %>%
  ungroup() %>% 
  mutate(PROP_MARKET = CWE_TONS / TOTAL_CWE,
         WEIGHTED_DFRS_RISK_PER_TON = PROP_MARKET * DFRS_HA_PER_TON_5_YR ) %>%
  group_by(MARKET) %>%
  summarise(SUM_PROP_MARKET = sum(PROP_MARKET), 
            SUM_WEIGHTED_DFRS_RISK_PER_TON = sum(WEIGHTED_DFRS_RISK_PER_TON)) %>%
  ungroup() %>% 
  mutate(BIOME = "All biomes") %>%
  bind_rows(risk_per_biome) %>%
  mutate(BIOME = ifelse(BIOME == "Atlantic Forest", "Atlantic\nForest", BIOME),
         BIOME = as_factor(BIOME),
         BIOME = fct_relevel(BIOME, 
                             levels = rev(c("All biomes","Atlantic\nForest","Cerrado","Amazon")))) %>%
  ggplot(aes(BIOME, SUM_WEIGHTED_DFRS_RISK_PER_TON*1000, fill = MARKET)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  theme_beef_with_axes() +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(), 
        axis.line.x = element_line(),
        legend.position = c(0.6, 0.62)) +
  ylab("Relative deforestation risk\n(ha/kton)") + 
  scale_fill_manual(values = c("#0072B2", "#D55E00"), 
                    breaks = c("Exports", "Domestic market")) + 
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0, 110))


# tidy up
rm(risk_per_biome)


# Compare forest cover of the domestic and export markets


# Summarise proportion of land use which was natural vegetation in 2015
land_use_2015 <- 
  mapbiomas %>%
  filter(!nivel1 %in% c("Corpo D'água")) %>%
  group_by(CODIBGE) %>%
  mutate(TOTAL_AREA = sum(`2015`)) %>%
  ungroup() %>%
  filter(nivel1 %in% c("Floresta", "Formação Natural não Florestal"), 
         nivel2 != "Floresta Plantada") %>%
  group_by(CODIBGE) %>%
  summarise(PROP_NATURAL_VEGETATION = sum(`2015`) / unique(TOTAL_AREA)) %>% 
  rename(GEOCODE = CODIBGE)


# Per market/biome, summarise prop of land use which was natural vegetation in 2015
market_vegetation_cover <- 
  production_per_geocode %>% 
  gather(MARKET, CWE_TONS, DOMESTIC_CWE_TONS:EXPORTED_CWE_TONS) %>%
  mutate(MARKET = ifelse(MARKET == "DOMESTIC_CWE_TONS","Domestic market","Exports")) %>%
  left_join(land_use_2015, by = c("GEOCODE")) %>%
  mutate(BIOME = ifelse(BIOME == "Atlantic Forest", "Atlantic\nForest", BIOME)) %>%
  group_by(MARKET, BIOME) %>%
  mutate(TOTAL_CWE = sum(CWE_TONS)) %>%
  ungroup() %>% 
  mutate(PROP_MARKET = CWE_TONS / TOTAL_CWE,
         WEIGHTED_PROP_NAT_VEGETATION = PROP_MARKET * PROP_NATURAL_VEGETATION ) %>%
  group_by(MARKET, BIOME) %>%
  summarise(SUM_PROP_MARKET = sum(PROP_MARKET), 
            SUM_WEIGHTED_PROP_NAT_VEGETATION = sum(WEIGHTED_PROP_NAT_VEGETATION, na.rm = T)) %>%
  ungroup()


# Plot the weighted mean forest cover per market
p_market_vegetation_cover <- 
  production_per_geocode %>% 
  gather(MARKET, CWE_TONS, DOMESTIC_CWE_TONS:EXPORTED_CWE_TONS) %>%
  mutate(MARKET = ifelse(MARKET == "DOMESTIC_CWE_TONS","Domestic market","Exports")) %>%
  left_join(land_use_2015, by = c("GEOCODE")) %>%
  mutate(BIOME = ifelse(BIOME == "Atlantic Forest", "Atlantic\nForest", BIOME)) %>%
  group_by(MARKET) %>%
  mutate(TOTAL_CWE = sum(CWE_TONS)) %>%
  ungroup() %>% 
  mutate(PROP_MARKET = CWE_TONS / TOTAL_CWE,
         WEIGHTED_PROP_NAT_VEGETATION = PROP_MARKET * PROP_NATURAL_VEGETATION ) %>%
  group_by(MARKET) %>%
  summarise(SUM_PROP_MARKET = sum(PROP_MARKET), 
            SUM_WEIGHTED_PROP_NAT_VEGETATION = sum(WEIGHTED_PROP_NAT_VEGETATION, na.rm = T)) %>%
  ungroup() %>%
  mutate(BIOME = "All biomes") %>%
  bind_rows(market_vegetation_cover) %>%
  filter(BIOME %in% c("All biomes","Amazon", "Cerrado", "Atlantic\nForest")) %>%
  mutate(BIOME = ifelse(BIOME == "Atlantic Forest", "Atlantic\nForest", BIOME),
         BIOME = as_factor(BIOME),
         BIOME = fct_relevel(BIOME, 
                             levels = rev(c("All biomes","Atlantic\nForest","Cerrado","Amazon")))) %>%
  ggplot(aes(BIOME, SUM_WEIGHTED_PROP_NAT_VEGETATION * 100, fill = MARKET)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  theme_beef_with_axes() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(), 
        axis.line.x = element_line(),
        legend.position = "none" 
  ) +
  ylab("Natural vegetation cover\n(%)") + 
  scale_fill_manual(values = c("#0072B2", "#D55E00"), 
                    breaks = c("Exports", "Domestic market")) + 
  scale_y_continuous(expand = c(0,0)) +
  NULL


# tidy up
rm(mapbiomas, land_use_2015, market_vegetation_cover)


# Plot the agricultural and total GDP of export/domestic markets


# Make df of the weighted agricultural GDP
# ... weighted per market/biome
# ... NB units are mil reais/pessoa
market_pib <- 
  production_per_geocode %>% 
  gather(MARKET, CWE_TONS, DOMESTIC_CWE_TONS:EXPORTED_CWE_TONS) %>%
  mutate(MARKET = ifelse(MARKET == "DOMESTIC_CWE_TONS","Domestic market","Exports")) %>%
  left_join(agro_pib_per_capita, by = c("GEOCODE")) %>%
  mutate(BIOME = ifelse(BIOME == "Atlantic Forest", "Atlantic\nForest", BIOME)) %>%
  group_by(MARKET, BIOME) %>%
  mutate(TOTAL_CWE = sum(CWE_TONS)) %>%
  ungroup() %>% 
  mutate(PROP_MARKET = CWE_TONS / TOTAL_CWE,
         WEIGHTED_PIB = PROP_MARKET * AGRO_GDP_PER_CAPITA ) %>%
  group_by(MARKET, BIOME) %>%
  summarise(SUM_PROP_MARKET = sum(PROP_MARKET), 
            SUM_WEIGHTED_PIB = sum(WEIGHTED_PIB, na.rm = T)) %>%
  ungroup()


# Plot
p_market_pib <- 
  production_per_geocode %>% 
  gather(MARKET, CWE_TONS, DOMESTIC_CWE_TONS:EXPORTED_CWE_TONS) %>%
  mutate(MARKET = ifelse(MARKET == "DOMESTIC_CWE_TONS","Domestic market","Exports")) %>%
  left_join(agro_pib_per_capita, by = c("GEOCODE")) %>%
  mutate(BIOME = ifelse(BIOME == "Atlantic Forest", "Atlantic\nForest", BIOME)) %>%
  group_by(MARKET) %>%
  mutate(TOTAL_CWE = sum(CWE_TONS)) %>%
  ungroup() %>% 
  mutate(PROP_MARKET = CWE_TONS / TOTAL_CWE,
         WEIGHTED_PIB = PROP_MARKET * AGRO_GDP_PER_CAPITA ) %>%
  group_by(MARKET) %>%
  summarise(SUM_PROP_MARKET = sum(PROP_MARKET), 
            SUM_WEIGHTED_PIB = sum(WEIGHTED_PIB, na.rm = T)) %>%
  ungroup() %>%
  mutate(BIOME = "All biomes") %>%
  bind_rows(market_pib) %>%
  filter(BIOME %in% c("All biomes","Amazon", "Cerrado", "Atlantic\nForest")) %>%
  mutate(BIOME = ifelse(BIOME == "Atlantic Forest", "Atlantic\nForest", BIOME),
         BIOME = as_factor(BIOME),
         BIOME = fct_relevel(BIOME, 
                             levels = rev(c("All biomes","Atlantic\nForest","Cerrado","Amazon")))) %>%
  ggplot(aes(BIOME, SUM_WEIGHTED_PIB, fill = MARKET)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  theme_beef_with_axes() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(), 
        axis.line.x = element_line(),
        legend.position = "none") +
  ylab("Agricultural GDP\n($BRL/capita)") + 
  scale_y_continuous(label = unit_format(unit = "k"),
                     expand = c(0,0)) +
  scale_fill_manual(values = c("#0072B2", "#D55E00"), 
                    breaks = c("Exports", "Domestic market")) + 
  NULL


# tidy up
rm(agro_pib_per_capita, market_pib)


# Multiplot of cattle deforestation, agricultural GDP, and natural vegetation cover
p_markets_compared <- 
  cowplot::plot_grid(p_dfrs_per_biome_market, p_market_pib, p_market_vegetation_cover,  
                     align = "h",
                     rel_widths = c(1,0.8,0.8),
                     ncol = 3)


# Export
ggsave(paste0(output_dir, "/", Sys.Date(), "-FIGURE_S7.tiff"), 
       units = "cm",
       width = 17.8, 
       height = 9,
       plot = p_markets_compared)


# tidy up
rm(p_dfrs_per_biome_market, p_market_pib, p_market_vegetation_cover,
   p_markets_compared, production_per_geocode)


# Figure S8 - commodity-deforestation -------------------------------------


# The proportion of deforestation explained by commodity expansion


# Total deforestation that becomes pasture within 5 years
past_dfrs_5yr <- 
  pasture_dfrs %>%
  filter(PASTURE_YEAR <= DEF_YEAR + 5) %>%
  group_by(BIOME, DEF_YEAR) %>%
  summarise(SUM_AREA_HA = sum(AREA_HA)) %>%
  ungroup() %>%
  mutate(BIOME = case_when(BIOME == "AMAZONIA"~ "AMAZON", 
                           TRUE ~ BIOME),
         DATA = "Max 5 years between\ndeforestation & pasture") %>%
  rename(YEAR = DEF_YEAR)
past_dfrs_allyr <- 
  pasture_dfrs %>%
  group_by(BIOME, DEF_YEAR) %>%
  summarise(SUM_AREA_HA = sum(AREA_HA)) %>%
  ungroup() %>%
  mutate(BIOME = case_when(BIOME == "AMAZONIA"~ "AMAZON", 
                           TRUE ~ BIOME),
         DATA = "Pasture deforestation\n(open time window)") %>%
  rename(YEAR = DEF_YEAR)


# Deforestation -> pasture -> soy within 5 years
soy_past_dfrs_5yr <- 
  pasture_dfrs %>%
  filter(PASTURE_YEAR <= DEF_YEAR + 5) %>%
  mutate(PAST_SOY_EXP_HA = AREA_WITH_POTENTIAL_SOY_HA - AREA_HA) %>%
  group_by(BIOME, DEF_YEAR) %>%
  summarise(SUM_AREA_HA = sum(PAST_SOY_EXP_HA)) %>%
  ungroup() %>%
  mutate(BIOME = case_when(BIOME == "AMAZONIA" ~"AMAZON", 
                           TRUE ~ BIOME),
         DATA = "Pasture deforestation\nbecoming soy within 5 yrs") %>%
  rename(YEAR = DEF_YEAR)


# Annunal soy deforestation
# ... nb The year of soy planting is the year before export
annual_soy_dfrs <- 
  soy_dfrs %>%
  mutate_if(is.character, str_to_upper) %>%
  group_by(SOY_EXPORT_YEAR, BIOME) %>%
  summarise(SUM_AREA_HA = sum(AVERAGE_SOY_DEF_HA, na.rm = T)) %>%
  ungroup() %>%
  mutate(DATA = "Soy planted on\nrecently deforestated land",
         YEAR = SOY_EXPORT_YEAR) %>%
  select(-SOY_EXPORT_YEAR)


# Join
dfrs_comp <- 
  bind_rows(total_ibge_dfrs, 
            past_dfrs_5yr,
            past_dfrs_allyr, 
            soy_past_dfrs_5yr,
            annual_soy_dfrs )


# plot
p_dfrs_allocation <- 
  dfrs_comp %>%
  filter(BIOME %in% c("AMAZON", "CERRADO")) %>%
  spread(DATA, SUM_AREA_HA) %>%
  filter(YEAR >= 2006) %>%
  mutate(
    `Deforestation becoming\npasture after > 5 yrs` = `Pasture deforestation\n(open time window)` - `Max 5 years between\ndeforestation & pasture`,
    `Soy planted on pasture\ndeforested < 5 yrs previously` = `Pasture deforestation\nbecoming soy within 5 yrs` / `Total deforestation`,
    `Cattle deforestation: deforestation\nbecoming pasture within 5 yrs` = `Max 5 years between\ndeforestation & pasture` / `Total deforestation`,
    `Deforestation becoming\npasture after > 5 yrs` = `Deforestation becoming\npasture after > 5 yrs` / `Total deforestation`,
    `Soy planted on land\ndeforested < 5 yrs previously` = (
      `Soy planted on\nrecently deforestated land` - `Pasture deforestation\nbecoming soy within 5 yrs`
    ) / `Total deforestation`,
    `Unattributed deforestation` = (1 - 
                                      `Soy planted on pasture\ndeforested < 5 yrs previously` - 
                                      `Cattle deforestation: deforestation\nbecoming pasture within 5 yrs` -
                                      `Deforestation becoming\npasture after > 5 yrs` -
                                      `Soy planted on land\ndeforested < 5 yrs previously`)
  ) %>%
  select(
    -`Pasture deforestation\nbecoming soy within 5 yrs`,
    -`Soy planted on\nrecently deforestated land`,
    -`Total deforestation`,
    -`Pasture deforestation\n(open time window)`,
    -`Max 5 years between\ndeforestation & pasture`
  ) %>%
  gather(
    DATA,
    PROP,
    `Deforestation becoming\npasture after > 5 yrs`:`Unattributed deforestation`
  ) %>%
  mutate(DATA = as_factor(DATA),
         DATA = fct_relevel(DATA,
                            c("Unattributed deforestation",
                              "Cattle deforestation: deforestation\nbecoming pasture within 5 yrs",
                              "Deforestation becoming\npasture after > 5 yrs",
                              "Soy planted on pasture\ndeforested < 5 yrs previously",
                              "Soy planted on land\ndeforested < 5 yrs previously"))) %>%
  ggplot(aes(YEAR, PROP, fill = DATA)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ BIOME) +
  theme_beef_with_axes() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_blank()
  ) +
  ylab("Proportion of deforested\narea per year") +
  scale_x_continuous(limits = c(2005, 2018),
                     breaks = seq(2006, 2017, by = 2)) +
  guides(fill = guide_legend(ncol = 2), byrow = TRUE) + 
  scale_fill_manual(values = c("darkgrey", trase_palette_categorical2))


# Export
ggsave(paste0(output_dir, "/", Sys.Date(), "-FIGURE_S8.tiff"),
       units = "cm", 
       width = 17.8, 
       height = 11,
       plot = p_dfrs_allocation)


# Figure S9 - amortized deforestation risk --------------------------------


# Deforestation risk of major markets, calculated using 10-yr amortization


# Select upper limit for relative risk
# ... used to standardize axis scales
max_dfrs_risk <- 
  cd_all %>% 
  mutate(EXPORTER_GROUP = case_when(EXPORTER %in% top_10_exporter ~ EXPORTER, 
                                    TRUE ~ "Other exporters")) %>%
  group_by(YEAR, EXPORTER_GROUP, LINKED_CNPJ) %>%
  summarise(SUM_CWE_PER_FLOW = sum(CWE_PER_FLOW)) %>%
  left_join(supply_sheds_df_dfrs_amortized, by = c("LINKED_CNPJ", "YEAR")) %>%
  unnest() %>%
  mutate(DFRS_HA = SUM_DFRS_PER_TON * SUM_CWE_PER_FLOW / 1000) %>%
  group_by(EXPORTER_GROUP, YEAR, NUM_RANDOMISATION) %>%
  summarise(SUM_DFRS_HA = sum(DFRS_HA),
            DFRS_HA_PER_TON = SUM_DFRS_HA / (sum(SUM_CWE_PER_FLOW)/1000)) %>%
  ungroup() %>%
  group_by(EXPORTER_GROUP, YEAR) %>%
  summarise(DFRS_HA_PER_KTON_95 = quantile(DFRS_HA_PER_TON, probs = c(0.95)) * 1000) %>% 
  ungroup() %>%
  filter(DFRS_HA_PER_KTON_95 == max(DFRS_HA_PER_KTON_95)) %>%
  pull(DFRS_HA_PER_KTON_95) + 5


# (A) Per exporter


# Plot total deforestation risk
(pE_tot <- 
    cd_all %>% 
    mutate(EXPORTER_GROUP = case_when(EXPORTER %in% top_10_exporter ~ EXPORTER, 
                                      TRUE ~ "Other exporters")) %>%
    mutate(EXPORTER_GROUP = case_when(EXPORTER_GROUP == "Meat Snack Partners do Brasil" ~ "Meat Snack Partners",
                                      EXPORTER_GROUP ==  "Irmaos Goncalves Comercio e Industria" ~  "Irmãos Gonçalves",
                                      EXPORTER_GROUP ==  "Frisa Frigorifico Rio Doce" ~  "Frisa",
                                      EXPORTER_GROUP ==  "Mataboi Alimentos" ~  "Prima Foods",
                                      TRUE ~ EXPORTER_GROUP)) %>%
    group_by(YEAR, EXPORTER_GROUP, LINKED_CNPJ) %>%
    summarise(SUM_CWE_PER_FLOW = sum(CWE_PER_FLOW)) %>%
    left_join(supply_sheds_df_dfrs_amortized, by = c("LINKED_CNPJ", "YEAR")) %>%
    unnest() %>%
    mutate(DFRS_HA = SUM_DFRS_PER_TON * SUM_CWE_PER_FLOW / 1000) %>%
    group_by(EXPORTER_GROUP, YEAR, NUM_RANDOMISATION) %>%
    summarise(SUM_DFRS_HA = sum(DFRS_HA)) %>%
    ungroup() %>%
    group_by(EXPORTER_GROUP, YEAR) %>%
    summarise(SUM_DFRS_MEAN = mean(SUM_DFRS_HA),
              SUM_DFRS_05 = quantile(SUM_DFRS_HA, probs = c(0.05)),
              SUM_DFRS_95 = quantile(SUM_DFRS_HA, probs = c(0.95)),
              SUM_DFRS_33 = quantile(SUM_DFRS_HA, probs = c(0.33)),
              SUM_DFRS_66 = quantile(SUM_DFRS_HA, probs = c(0.66))
    ) %>% 
    ungroup() %>%
    mutate(EXPORTER_GROUP = as_factor(EXPORTER_GROUP), 
           EXPORTER_GROUP = fct_relevel(EXPORTER_GROUP, levels = rev(c(abbr_exporter_order, "Other exporters"))),
           YEAR = as.character(YEAR), 
           YEAR = as_factor(YEAR), 
           YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015"))) %>%
    ggplot(aes(EXPORTER_GROUP, SUM_DFRS_MEAN, fill = YEAR)) +
    geom_bar(stat = "identity", 
             position="dodge") + 
    geom_errorbar(aes(ymin = SUM_DFRS_05, ymax = SUM_DFRS_95), 
                  size = 0.5, width = 0, 
                  position=position_dodge(width=1)) + 
    theme_beef_with_axes() + 
    coord_flip() +
    theme(axis.title = element_blank(), 
          legend.title = element_blank(), 
          # legend.position = c(0.8,0.35)) + 
          legend.position = "none") + 
    ylab("Deforestation risk (ha/year)") + 
    scale_fill_manual(values = year_colors, 
                      breaks = c("2015", "2016", "2017")) +
    scale_y_continuous(label=comma, 
                       expand = c(0, 0)) +
    NULL
)


# Relative deforestation risk
(pE_rel <- 
    cd_all %>% 
    mutate(EXPORTER_GROUP = case_when(EXPORTER %in% top_10_exporter ~ EXPORTER, 
                                      TRUE ~ "Other exporters")) %>%
    mutate(EXPORTER_GROUP = case_when(EXPORTER_GROUP == "Meat Snack Partners do Brasil" ~ "Meat Snack Partners",
                                      EXPORTER_GROUP ==  "Irmaos Goncalves Comercio e Industria" ~  "Irmãos Gonçalves",
                                      EXPORTER_GROUP ==  "Frisa Frigorifico Rio Doce" ~  "Frisa",
                                      EXPORTER_GROUP ==  "Mataboi Alimentos" ~  "Prima Foods",
                                      TRUE ~ EXPORTER_GROUP)) %>%
    group_by(YEAR, EXPORTER_GROUP, LINKED_CNPJ) %>%
    summarise(SUM_CWE_PER_FLOW = sum(CWE_PER_FLOW)) %>%
    left_join(supply_sheds_df_dfrs_amortized, by = c("LINKED_CNPJ", "YEAR")) %>%
    unnest() %>%
    mutate(DFRS_HA = SUM_DFRS_PER_TON * SUM_CWE_PER_FLOW / 1000) %>%
    group_by(EXPORTER_GROUP, YEAR, NUM_RANDOMISATION) %>%
    summarise(SUM_DFRS_HA = sum(DFRS_HA),
              DFRS_HA_PER_TON = SUM_DFRS_HA / (sum(SUM_CWE_PER_FLOW)/1000)) %>%
    ungroup() %>%
    group_by(EXPORTER_GROUP, YEAR) %>%
    summarise(DFRS_HA_PER_KTON_MEAN = mean(DFRS_HA_PER_TON) * 1000,
              DFRS_HA_PER_KTON_05 = quantile(DFRS_HA_PER_TON, probs = c(0.05)) * 1000,
              DFRS_HA_PER_KTON_95 = quantile(DFRS_HA_PER_TON, probs = c(0.95)) * 1000,
              DFRS_HA_PER_KTON_33 = quantile(DFRS_HA_PER_TON, probs = c(0.33)) * 1000,
              DFRS_HA_PER_KTON_66 = quantile(DFRS_HA_PER_TON, probs = c(0.66)) * 1000
    ) %>% 
    ungroup() %>%
    mutate(EXPORTER_GROUP = as_factor(EXPORTER_GROUP), 
           EXPORTER_GROUP = fct_relevel(EXPORTER_GROUP, levels = rev(c(abbr_exporter_order, "Other exporters"))),
           YEAR = as.character(YEAR), 
           YEAR = as_factor(YEAR), 
           YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015"))) %>%
    ggplot(aes(EXPORTER_GROUP, DFRS_HA_PER_KTON_MEAN, fill = YEAR)) +
    geom_bar(stat = "identity", 
             position="dodge") + 
    geom_errorbar(aes(ymin = DFRS_HA_PER_KTON_05, ymax = DFRS_HA_PER_KTON_95), 
                  size = 0.5, width = 0, 
                  position=position_dodge(width=1)) + 
    theme_beef_with_axes() + 
    coord_flip() +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank(), 
          legend.position = "none") + 
    ylab("Relative deforestation risk\n(ha/kton)") + 
    scale_fill_manual(values = year_colors, 
                      breaks = c("2015", "2016", "2017")) +
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, max_dfrs_risk)) +
    NULL
)


# (B) Per 10 market


# Total dfrs risk
(pC_tot <- 
    cd_all %>% 
    group_by(YEAR, COUNTRY_GROUP, LINKED_CNPJ) %>%
    summarise(SUM_CWE_PER_FLOW = sum(CWE_PER_FLOW)) %>%
    left_join(supply_sheds_df_dfrs_amortized, by = c("LINKED_CNPJ", "YEAR")) %>%
    unnest() %>%
    mutate(DFRS_HA = SUM_DFRS_PER_TON * SUM_CWE_PER_FLOW / 1000) %>%
    group_by(COUNTRY_GROUP, YEAR, NUM_RANDOMISATION) %>%
    summarise(SUM_DFRS_HA = sum(DFRS_HA)) %>%
    ungroup() %>%
    group_by(COUNTRY_GROUP, YEAR) %>%
    summarise(SUM_DFRS_MEAN = mean(SUM_DFRS_HA),
              SUM_DFRS_05 = quantile(SUM_DFRS_HA, probs = c(0.05)),
              SUM_DFRS_95 = quantile(SUM_DFRS_HA, probs = c(0.95)),
              SUM_DFRS_33 = quantile(SUM_DFRS_HA, probs = c(0.33)),
              SUM_DFRS_66 = quantile(SUM_DFRS_HA, probs = c(0.66))
    ) %>% 
    ungroup() %>%
    mutate(COUNTRY_GROUP = as_factor(COUNTRY_GROUP), 
           COUNTRY_GROUP = fct_relevel(COUNTRY_GROUP, levels = rev(top_10_cntry_grp)),
           YEAR = as.character(YEAR), 
           YEAR = as_factor(YEAR), 
           YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015"))) %>%
    ggplot(aes(COUNTRY_GROUP, SUM_DFRS_MEAN, fill = YEAR)) +
    geom_bar(stat = "identity", 
             position="dodge") + 
    geom_errorbar(aes(ymin = SUM_DFRS_05, ymax = SUM_DFRS_95), 
                  size = 0.5, width = 0, 
                  position=position_dodge(width=1)) + 
    theme_beef() + 
    coord_flip() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(), 
          legend.position = c(0.8,0.35),
          axis.line.x = element_line()) + 
    ylab("Deforestation risk (ha/year)") + 
    scale_fill_manual(values = year_colors, 
                      breaks = c("2015", "2016", "2017")) +
    scale_y_continuous(label=comma,
                       expand = c(0,0)) +
    NULL
)


# Relative deforestation risk
(pC_rel <- 
    cd_all %>% 
    group_by(YEAR, COUNTRY_GROUP, LINKED_CNPJ) %>%
    summarise(SUM_CWE_PER_FLOW = sum(CWE_PER_FLOW)) %>%
    left_join(supply_sheds_df_dfrs_amortized, by = c("LINKED_CNPJ", "YEAR")) %>%
    unnest() %>%
    mutate(DFRS_HA = SUM_DFRS_PER_TON * SUM_CWE_PER_FLOW / 1000) %>%
    group_by(COUNTRY_GROUP, YEAR, NUM_RANDOMISATION) %>%
    summarise(SUM_DFRS_HA = sum(DFRS_HA),
              DFRS_HA_PER_TON = SUM_DFRS_HA / (sum(SUM_CWE_PER_FLOW) / 1000)) %>%
    ungroup() %>%
    group_by(COUNTRY_GROUP, YEAR) %>%
    summarise(DFRS_HA_PER_KTON_MEAN = mean(DFRS_HA_PER_TON) * 1000,
              DFRS_HA_PER_KTON_05 = quantile(DFRS_HA_PER_TON, probs = c(0.05)) * 1000,
              DFRS_HA_PER_KTON_95 = quantile(DFRS_HA_PER_TON, probs = c(0.95)) * 1000,
              DFRS_HA_PER_KTON_33 = quantile(DFRS_HA_PER_TON, probs = c(0.33)) * 1000,
              DFRS_HA_PER_KTON_66 = quantile(DFRS_HA_PER_TON, probs = c(0.66)) * 1000
    ) %>% 
    ungroup() %>%
    mutate(COUNTRY_GROUP = as_factor(COUNTRY_GROUP), 
           COUNTRY_GROUP = fct_relevel(COUNTRY_GROUP, 
                                       levels = rev(top_10_cntry_grp)),
           YEAR = as.character(YEAR), 
           YEAR = as_factor(YEAR), 
           YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015"))) %>%
    ggplot(aes(COUNTRY_GROUP, DFRS_HA_PER_KTON_MEAN, fill = YEAR)) +
    geom_bar(stat = "identity", 
             position="dodge") + 
    geom_errorbar(aes(ymin = DFRS_HA_PER_KTON_05, ymax = DFRS_HA_PER_KTON_95), 
                  size = 0.5, width = 0, 
                  position=position_dodge(width=1)) + 
    theme_beef() + 
    coord_flip() +
    theme(axis.title.y = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank(), 
          legend.position = "none",
          axis.line.x = element_line()) + 
    ylab("Relative deforestation risk (ha/kton)") + 
    scale_fill_manual(values = year_colors,
                      breaks = c("2015","2016","2017")) +
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, max_dfrs_risk)) +
    NULL
)


# (C) Per domestic/export markets


# Relative domestic vs export total deforestation risk
pD_rel <- markets_compared_10_yr_amortized %>%
  mutate(MARKET = ifelse(MARKET == "DOMESTIC", "Domestic market", "Export markets")) %>%
  group_by(MARKET, YEAR) %>%
  summarise(SUM_DFRS_MEAN = mean(DFRS_HA_PER_TON) * 1000,
            SUM_DFRS_05 = quantile(DFRS_HA_PER_TON, probs = c(0.05)) * 1000,
            SUM_DFRS_95 = quantile(DFRS_HA_PER_TON, probs = c(0.95)) * 1000,
            SUM_DFRS_33 = quantile(DFRS_HA_PER_TON, probs = c(0.33)) * 1000,
            SUM_DFRS_66 = quantile(DFRS_HA_PER_TON, probs = c(0.66)) * 1000
  ) %>% 
  ungroup() %>%
  mutate(YEAR = as.character(YEAR), 
         YEAR = as_factor(YEAR), 
         YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015"))) %>%
  ggplot(aes(MARKET, SUM_DFRS_MEAN, fill = as.factor(YEAR))) +  
  geom_bar(stat = "identity", 
           position="dodge") + 
  geom_errorbar(aes(ymin = SUM_DFRS_05, ymax = SUM_DFRS_95), 
                size = 0.5, width = 0, 
                position=position_dodge(width=1)) + 
  theme_beef() + 
  coord_flip() +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "none",
        axis.line.x = element_line()) + 
  ylab("Relative deforestation risk\n(ha/kton)") + 
  scale_fill_manual(values = year_colors, 
                    breaks = c("2015", "2016", "2017")) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max_dfrs_risk)) +
  NULL


# Total domestic vs export total deforestation risk
pD_tot <-
  markets_compared_10_yr_amortized %>%
  mutate(MARKET = ifelse(MARKET == "DOMESTIC", "Domestic market", "Export markets")) %>%
  group_by(MARKET, YEAR) %>%
  summarise(SUM_DFRS_MEAN = mean(SUM_DFRS_HA),
            SUM_DFRS_05 = quantile(SUM_DFRS_HA, probs = c(0.05)),
            SUM_DFRS_95 = quantile(SUM_DFRS_HA, probs = c(0.95)),
            SUM_DFRS_33 = quantile(SUM_DFRS_HA, probs = c(0.33)),
            SUM_DFRS_66 = quantile(SUM_DFRS_HA, probs = c(0.66))
  ) %>% 
  ungroup() %>%
  mutate(YEAR = as.character(YEAR), 
         YEAR = as_factor(YEAR), 
         YEAR = fct_relevel(YEAR, levels = c("2017", "2016","2015"))) %>%
  ggplot(aes(MARKET, SUM_DFRS_MEAN, fill = as.factor(YEAR))) + 
  geom_bar(stat = "identity", 
           position="dodge") + 
  geom_errorbar(aes(ymin = SUM_DFRS_05, ymax = SUM_DFRS_95), 
                size = 0.5, width = 0, 
                position=position_dodge(width=1)) + 
  theme_beef() +
  coord_flip() +
  theme(axis.title.y = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "none",
        axis.line.x = element_line()) + 
  ylab("Deforestation risk (ha)") + 
  scale_fill_manual(values = year_colors, 
                    breaks = c("2015", "2016", "2017")) +
  scale_y_continuous(label=comma, 
                     expand = c(0,0)) +
  NULL


# Use patchwork to compose plot
layout <- '
AB
CD
EF
'
p_all <- 
  pE_tot + pE_rel + pC_tot +  pC_rel + pD_tot + pD_rel + 
  plot_layout(design = layout,
              widths = c(1, 1), 
              heights = c(0.84,1,0.2)) +
  plot_annotation(tag_levels = 'A')


# Export
ggplot2::ggsave(paste0(output_dir,"/",Sys.Date(),"-FIGURE_S9.tiff"), 
                units = "cm", 
                width = 17.8,
                height = 30,
                plot = p_all)


# tidy up
rm(markets_compared, pC_tot, pC_rel, pD_tot, pD_rel, 
   pE_tot, pE_rel, layout,
   max_dfrs_risk)


# Figure S10 - G4 risk breakdown ------------------------------------------


# Deforestation risk of G4 companies


# Plot source of data on animal supply to slaughterhouses
p_source_risk <- 
  data_simple %>% 
  filter(EXPORTER %in% c("Marfrig", "Minerva", "JBS")) %>%
  group_by(EXPORTER, YEAR, GEOCODE_SOURCE) %>%
  summarise(SUM_DFRS = sum(BEEF_DEF_5_YEAR_HA)) %>%
  ungroup() %>%
  mutate(GEOCODE_SOURCE = ifelse(GEOCODE_SOURCE == "UNKNOWN","Average risk", GEOCODE_SOURCE),
         GEOCODE_SOURCE = as_factor(GEOCODE_SOURCE),
         GEOCODE_SOURCE = fct_relevel(GEOCODE_SOURCE,
                                      levels = rev(c("GTA","SIGSIF", "Average risk")))) %>%
  ggplot(aes(YEAR, SUM_DFRS/1e03, fill = GEOCODE_SOURCE)) + 
  facet_grid(rows = vars(EXPORTER)) + 
  geom_bar(stat = "identity") +
  theme_beef() +
  coord_flip() + 
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA) ) +
  ylab("Deforestation risk linked to exports\n(thousand ha/year)") +
  scale_fill_manual(values = unname(biome_colors),
                    breaks = c("GTA","SIGSIF", "National-average risk")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,30)) +
  NULL


# Plot the deforestation risk per biomes 
p_biome_risk <- 
  data_simple %>% 
  filter(EXPORTER %in% c("Marfrig", "Minerva", "JBS")) %>%
  group_by(EXPORTER, YEAR, BIOME) %>%
  summarise(SUM_DFRS = sum(BEEF_DEF_5_YEAR_HA)) %>%
  filter(BIOME %in% c("Amazon", "Cerrado", "Atlantic Forest")) %>%
  mutate(BIOME = as_factor(BIOME),
         BIOME = fct_relevel(BIOME, 
                             levels = c("Amazon", "Cerrado", "Atlantic Forest"))) %>%
  ggplot(aes(YEAR, SUM_DFRS/1e03, fill = BIOME)) + 
  facet_grid(rows = vars(EXPORTER)) + 
  geom_bar(stat = "identity") +
  theme_beef() +
  coord_flip() + 
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA) ) +
  ylab("Deforestation risk linked to exports\n(thousand ha/year)") +
  scale_fill_manual(values = biome_colors,
                    breaks = c("Cerrado","Atlantic Forest", "Amazon")) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0,30)) +
  NULL


# Multiplot
p_G4 <- cowplot::plot_grid(p_source_risk, p_biome_risk,
                           labels = c("A","B"))


# Export
ggplot2::ggsave(paste0(output_dir,"/",Sys.Date(),"-FIGURE_S10.tiff"), 
                units = "cm", 
                width = 17.8,
                height = 12,
                plot = p_G4)


# tidy up
rm(p_G4, p_source_risk, p_biome_risk)



# Figure S11 - DT ---------------------------------------------------------


# Decision tree schematic
# ... see manuscript for this



# Figure S12 - GTA distribution -------------------------------------------


# The number of cattle GTAs per state and year


# Plot barchart of GTAs/year
p_time <- 
  gtas_per_year_df %>%
  mutate(transport.year = as.numeric(transport.year)) %>%
  filter(transport.year %in% c(2012:2017)) %>%
  group_by(transport.year) %>%
  summarise(NUM_GTA = sum(NUM_GTA)) %>%
  ungroup() %>%
  ggplot(aes(transport.year, NUM_GTA/1e06)) + 
  geom_bar(stat = "identity", fill = "#0072B2") +
  theme_beef_with_axes() +
  theme(axis.text.x = element_text(hjust = 1, angle = 45),
        axis.title.x = element_blank()) +
  scale_x_continuous(breaks = seq(2012,2016, by = 2)) +
  ylab("GTAs/year\n(millions)") +
  NULL


# Map the number of valid bovine GTAs per state
# ... nb I can use cowplot::panel_border(remove = FALSE) to get rid of the border
state_cent <- st_centroid(states) # Get state centroids
states_no_gtas <- states %>%
  filter(STATE_NAME %in% c("ACRE", "RORAIMA", "AMAPA", "AMAZONAS"))
num_gta_df <- 
  gta_cleaning_summary_df %>%
  mutate(VALUE = as.numeric(VALUE)) %>%
  filter(VARIABLE == "Number of valid cattle GTAs") %>%
  mutate(`GTAs/state\n(millions)` = VALUE / 1e06) %>%
  filter(VALUE > 0) 
my_coord <- st_coordinates(state_cent) %>%
  as.data.frame() 
state_cent_data <- state_cent %>%
  bind_cols(., my_coord) %>%
  left_join(., num_gta_df, by = c("STATE_ABBR" = "STATE")) 
p_map_num_gta <- 
  ggplot() + 
  geom_sf(data = states, fill = NA) + 
  geom_sf(data = states_no_gtas, fill = "#999999", alpha = 0.4) + 
  geom_point(data = state_cent_data, col = "#0072B2", 
             alpha = 0.8, aes(x=X,y=Y,size = `GTAs/state\n(millions)`)) +
  coord_sf(datum=NA) + 
  scale_x_continuous(limits = c(-75, -35)) +
  scale_size_continuous(name = "GTAs/state\n(millions)") +
  theme_beef() + 
  theme(legend.title = element_text(),
        axis.title = element_blank()) +
  NULL


# tidy up
rm(states_no_gtas, num_gta_df, my_coord, state_cent_data, 
   gta_cleaning_summary_df, state_cent, gtas_per_year_df)


# Multiplot
# ... extract legend
# ... add inset plot
plot_with_inset <-
  cowplot::ggdraw() +
  cowplot::draw_plot(p_map_num_gta + theme(legend.position = "none",
                                  plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  cowplot::draw_plot(p_time, x = 0.1, y = .03, width = .33, height = .4)
my_legend <- cowplot::get_legend(p_map_num_gta + theme(legend.position = "right"))
plot_with_legend <- 
  cowplot::plot_grid(plot_with_inset, my_legend,
                     rel_widths = c(1, 0.2))

# save plot
ggplot2::ggsave(paste0(output_dir,"/",Sys.Date(),"-FIGURE_S12.tiff"), 
                width = 16, height = 11.2, units =  "cm",
                plot = plot_with_legend)


# tidy up
rm(plot_with_inset, plot_with_legend, my_legend, 
   p_map_num_gta, p_time)


# Figure S13 - GTA method -------------------------------------------------


# See manuscript for schematic.


# Figure S14 --------------------------------------------------------------


# Example supply shed data
# ... based on animal movements (top row), and SIGSIF (bottom row)


taxNums_to_plot <- c("06088741000829", "03853896001545", "18626084000210")
max_value <- 
  supply_sheds_df_sbst %>% 
  filter(taxNum %in% taxNums_to_plot) %>%
  unnest() %>%
  filter(PROP_FLOWS == max(PROP_FLOWS)) %>%
  pull(PROP_FLOWS)
max_value_plus_5_percent <- max_value + max_value*0.05
fn_plot_source <- function(TAXNUM){
  supply_shed <- supply_sheds_df_sbst %>% filter(taxNum == TAXNUM) %>% unnest()
  shs_to_plot <- shs %>%
    filter(SH_CNPJ == TAXNUM)
  munis_to_plot <- inner_join(munis, supply_shed, by = "GEOCODE")
  if(TAXNUM == "18626084000210"){
    ggplot() +
      geom_sf(
        data = munis_to_plot,
        aes(fill=PROP_FLOWS),
        color = NA, size = 0.25) +
      geom_sf(data = states, fill = NA, color = "darkgrey") +
      geom_sf(data = shs_to_plot, color = "#009E73", size = 3) +
      theme_beef() +
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            legend.position = c(0.05, 0.22)
      ) +
      scale_fill_viridis(option="magma",
                         name = bquote('Origin of\nsourcing (%)'),
                         na.value="white",
                         direction = -1) +
      coord_sf(datum = NA) +
      NULL 
  } else {
    ggplot() +
      geom_sf(
        data = munis_to_plot,
        aes(fill=PROP_FLOWS),
        color = NA, size = 0.25) +
      geom_sf(data = states, fill = NA, color = "darkgrey") +
      geom_sf(data = shs_to_plot, color = "#009E73", size = 3) +
      theme_beef() +
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            legend.position = "none"
      ) +
      scale_fill_viridis(option="magma",
                         name = bquote('Origin of\nsourcing (%)'),
                         na.value="white",
                         limits = c(0,max_value_plus_5_percent),
                         direction = -1) +
      coord_sf(datum = NA) +
      NULL
  }
}
list_of_plots_gta <- map(taxNums_to_plot, fn_plot_source)


# tidy up
rm(fn_plot_source, taxNums_to_plot, max_value_plus_5_percent, max_value)


# Plot example states where SIF data were relied on
states_to_plot <- c("RO", "GO", "SP")
max_value <- sigsif %>% filter(STATE_OF_SLAUGHTER %in% c("RO", "GO", "SP")) %>% pull(PROP_FLOWS) %>% max(.)
max_value_plus_5_percent <- max_value + max_value*0.05
fn_plot_source <- function(STATES){
  supply_shed <- sigsif %>% filter(STATE_OF_SLAUGHTER == STATES)
  munis_to_plot <- inner_join(munis, supply_shed, by = "GEOCODE")
  if(STATES == "SP"){
    ggplot() +
      geom_sf(
        data = munis_to_plot,
        aes(fill=PROP_FLOWS),
        color = NA, size = 0.25) +
      geom_sf(data = states, fill = NA, color = "darkgrey") +
      theme_beef() +
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            legend.position = c(0.05, 0.22)
      ) +
      scale_fill_viridis(option="magma",
                         name = bquote('Origin of\nsourcing (%)'),
                         na.value="white",
                         limits = c(0,max_value_plus_5_percent),
                         direction = -1) +
      coord_sf(datum = NA) +
      annotate("text", x=-70, y=5, size=6, family="Avenir Medium", label= STATES) +
      NULL 
  } else {
    ggplot() +
      geom_sf(
        data = munis_to_plot,
        aes(fill=PROP_FLOWS),
        color = NA, size = 0.25) +
      geom_sf(data = states, fill = NA, color = "darkgrey") +
      theme_beef() +
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            legend.position = "none"
      ) +
      scale_fill_viridis(option="magma",
                         name = bquote('Origin of\nsourcing (%)'),
                         na.value="white",
                         limits = c(0,max_value_plus_5_percent),
                         direction = -1) +
      coord_sf(datum = NA) +
      annotate("text", x=-70, y=5, size=6, family="Avenir Medium", label= STATES) +
      NULL
  }
}
list_of_plots_sif <- map(states_to_plot, fn_plot_source)


# Plot all together
layout <- '
ABC
DEF
'
p_all <-
  list_of_plots_gta[[1]] + list_of_plots_gta[[2]] + list_of_plots_gta[[3]] + 
  list_of_plots_sif[[1]] + list_of_plots_sif[[2]] + list_of_plots_sif[[3]] +
  plot_layout(design = layout)


# Export
ggsave(
  paste0(output_dir, "/", Sys.Date(), "-FIGURE_S14.tiff"),
  plot = p_all
)


# tidy up
rm(fn_plot_source, list_of_plots_gta, list_of_plots_sif, layout,
   states_to_plot, max_value_plus_5_percent, max_value)


# Figure S15 - cattle deforestation/yr ------------------------------------


# Cattle deforestation per year
p_dfrs <-
  pasture_dfrs %>% 
  filter(PASTURE_YEAR <= DEF_YEAR + 5) %>%
  mutate(BIOME = case_when(BIOME == "AMAZONIA" ~ "Amazon",
                           BIOME == "CERRADO" ~ "Cerrado",
                           BIOME == "ATL" ~ "Atlantic Forest",
                           TRUE ~ NA_character_)) %>%
  group_by(PASTURE_YEAR, BIOME) %>%
  summarise(SUM_AREA_HA = sum(AREA_HA) ) %>%
  ungroup() %>%
  ggplot(aes(PASTURE_YEAR, SUM_AREA_HA/1e06, fill = BIOME)) +
  geom_area() +  
  scale_fill_manual(values = biome_colors) +
  scale_y_continuous(expand = c(0,0), limits = c(0,2)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_beef() + 
  theme(legend.position = c(0.75,0.75), 
        legend.title = element_blank()) +
  ylab("Area of cattle deforestation\n(Mha/year)")+
  xlab("Year of pasture expansion")
p_dfrs


# Export
ggsave(paste0(output_dir, "/", Sys.Date(), "-FIGURE_S15.tiff"),
       units = "cm",
       width = 17.8,
       height = 9,
       plot = p_dfrs)


# tidy up
rm(p_dfrs)


# Figure S16 - GTA completeness -------------------------------------------


# Completeness of the GTA data used in this analysis


# Plot
p_gta_completeness <- 
  bind_rows(slaughter_gtas, anualpec) %>% 
  filter(YEAR %in% c(2012:2017)) %>% 
  complete(STATE_ABBR, YEAR, DATA_SOURCE,  fill = list(ABATE_HEAD = 0)) %>%
  ggplot(aes(YEAR, ABATE_HEAD/1e06, fill = DATA_SOURCE)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_beef_with_axes() +
  facet_wrap(~STATE_ABBR, scales = "free_y", ncol = 7) + 
  theme(legend.position = c(0.93,0.05), 
        legend.title = element_blank(), 
        axis.title.x = element_blank()) + 
  scale_y_continuous(label = unit_format(unit = "M"), 
                     expand = c(0,0)) + 
  ylab("Number of cattle slaughtered") + 
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("#0072B2", "#D55E00"))


# Export
ggsave(paste0(output_dir, "/", Sys.Date(), "-FIGURE_S16.tiff"),
       units = "cm", 
       width = 17.8, 
       height = 10.5,
       plot = p_gta_completeness)


# Figure S17 - pasture deforestation method -------------------------------


# See manuscript for schematic.


# Figure S18 - exports per muni ---------------------------------------------


# The proportion of cattle production exported per muni/year


# Calculate the proportion exported per geocode
exp_prop_per_geocode <-
  prod %>%
  filter(CW_PRODUCTION_TONS_5_YR != 0) %>%
  left_join(exp_per_geocode, by = c("GEOCODE", "YEAR")) %>%
  filter(YEAR %in% c(2015:2017)) %>%
  mutate(EXPORTED_CWE_TONS = ifelse(is.na(EXPORTED_CWE_TONS), 0, EXPORTED_CWE_TONS),
         CWE_PRODUCTION_TONS = CW_PRODUCTION_TONS_5_YR / 5,
         EXP_PROP = EXPORTED_CWE_TONS / CWE_PRODUCTION_TONS,
         EXP_PROP_CAT = case_when(EXP_PROP == 0 ~ "0%", 
                                  EXP_PROP < 0.2 ~ "< 20%", 
                                  EXP_PROP >= 0.2 & EXP_PROP <0.4 ~ "20-40%", 
                                  EXP_PROP >= 0.4 & EXP_PROP <0.6 ~ "40-60%", 
                                  EXP_PROP >= 0.6 & EXP_PROP <0.8 ~ "60-80%", 
                                  EXP_PROP >= 0.8 & EXP_PROP <=1 ~ "80-100%",
                                  EXP_PROP > 1 ~ "> 100%",
                                  TRUE ~ NA_character_),
         EXP_PROP_CAT = as_factor(EXP_PROP_CAT),
         EXP_PROP_CAT = fct_relevel(EXP_PROP_CAT, 
                                    levels = c("0%","< 20%","20-40%", "40-60%","60-80%","80-100%","> 100%")))
munis_to_plot <- exp_prop_per_geocode %>%
  inner_join(munis, ., by = "GEOCODE") 


# Plot
p_map <-
  ggplot() +
  geom_sf(data = munis_to_plot, col = NA, aes(fill = EXP_PROP_CAT)) +
  geom_sf(data = states, fill = NA) +
  coord_sf(datum=NA) +
  theme_beef() +
  scale_fill_manual(values = rev(c("#d73027", "#fc8d59", "#fee090", "#ffffbf", "#e0f3f8", "#91bfdb","#4575b4")),
                    breaks = c("0%","< 20%","20-40%", "40-60%","60-80%","80-100%","> 100%"), 
                    name="Exports (%)") +
  facet_wrap(~YEAR)


# Export 
ggsave(paste0(output_dir, "/", Sys.Date(), "-FIGURE_S18.tiff"),
       units = "cm",
       width = 17.8,
       height = 9,
       plot = p_map)


# tidy up
rm(p_map, munis_to_plot, exp_prop_per_geocode)