# Script to produce statistics from:
# ... "The origin, supply chain, and deforestation risk of Brazilian beef exports"
# Author: Erasmus zu Ermgassen
# Contact erasmus.zuermgassen@uclouvain.be with any questions


# Workstation set up ------------------------------------------------------


# Load packages
library(tidyverse)
library(sf)
library(stringi)


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


# Pre-process data --------------------------------------------------------


# subset bovine SIFs
sifs_bov <- 
  sifs_all %>%
  filter(grepl("AB1|AB2|AB3|AB4|AB5|MB1|MB|MB3|MB4|MB5", SIF_CATEGORIES))


# Exports value -----------------------------------------------------------


data_simple %>% 
  group_by(YEAR) %>%
  summarise(VALUE_BILLION_USD = sum(FOB) / 1e09)


# Number of supply chain nodes --------------------------------------------


# Number of MUNICIPALITIES, SLAUGHTERHOUSES, EXPORTERs, IMPORTERs, COUNTRIES
# ... in the cattle export supply chain
data_simple %>% filter(!grepl("UNKNOWN|AGGREGATED",GEOCODE)) %>% pull(GEOCODE) %>% unique() %>% length()
data_simple %>% filter(!grepl("UNKNOWN",LH_CNPJ), PRODUCT_DESC != "Live cattle exports") %>% pull(LH_CNPJ) %>% unique() %>% length()
data_simple %>% filter(!grepl("UNKNOWN",EXPORTER)) %>% pull(EXPORTER) %>% unique() %>% length()
data_simple %>% filter(!grepl("UNKNOWN",IMPORTER)) %>% pull(IMPORTER) %>% unique() %>% length()
data_simple %>% pull(COUNTRY) %>% unique() %>% length()


# Nationwide exported proportion ------------------------------------------


# Sum the exported carcass per state
exp_nationwide <- 
  data_simple %>%
  group_by(YEAR) %>% 
  summarise(EXPORTED_CWE_TONS = sum(CWE) / 1000) %>%
  ungroup()


# Calculate production per state
prod_nationwide <- prod %>% 
  group_by(YEAR) %>%
  summarise(SUM_CW_PRODUCTION_TONS_5_YR = sum(CW_PRODUCTION_TONS_5_YR) ) %>%
  ungroup()


# Calculate the proportion exported at the national level
left_join(prod_nationwide, exp_nationwide, by =  "YEAR") %>%
  filter(YEAR %in% c(2015:2017)) %>%
  mutate(EXPORTED_CWE_TONS = ifelse(is.na(EXPORTED_CWE_TONS), 0, EXPORTED_CWE_TONS),
         CWE_PRODUCTION_TONS = SUM_CW_PRODUCTION_TONS_5_YR / 5,
         EXP_PERC = EXPORTED_CWE_TONS / CWE_PRODUCTION_TONS) %>%
  select(YEAR, CWE_PRODUCTION_TONS, EXPORTED_CWE_TONS, EXP_PERC)


# Tidy up
rm(exp_nationwide, prod_nationwide)


# State exported proportion -----------------------------------------------


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
left_join(prod_per_state, exp_per_state, by = c("STATE_NAME" = "STATE", "YEAR")) %>%
  filter(YEAR %in% c(2015:2017)) %>%
  mutate(EXPORTED_CWE_TONS = ifelse(is.na(EXPORTED_CWE_TONS), 0, EXPORTED_CWE_TONS),
         CWE_PRODUCTION_TONS = SUM_CW_PRODUCTION_TONS_5_YR / 5) %>%
  group_by(STATE_NAME) %>%
  summarise(EXPORTED_CWE_TONS = sum(EXPORTED_CWE_TONS), 
            CWE_PRODUCTION_TONS = sum(CWE_PRODUCTION_TONS)) %>%
  ungroup() %>%
  mutate(EXPORTED_PERC = EXPORTED_CWE_TONS / CWE_PRODUCTION_TONS * 100) %>%
  arrange(desc(EXPORTED_PERC))
  

# Contribution of states to exports ---------------------------------------


# Count the number of exporting slaughterhouses per state
slaughter_data_summary <- 
  data_simple %>%
  filter(!grepl("UNKNOWN", LH_CNPJ),
         PRODUCT_DESC != "Live cattle exports") %>%
  mutate(TOTAL_CWE = sum(CWE),
         TOTAL_NUM_SH = n_distinct(LH_CNPJ)) %>%
  group_by(LH_STATE) %>%
  summarise(SUM_CWE = sum(CWE),
            PROP_CWE = SUM_CWE / median(TOTAL_CWE),
            NUM_SH = n_distinct(LH_CNPJ),
            PROP_NUM_SH = NUM_SH / median(TOTAL_NUM_SH)) %>% 
  ungroup() %>%
  arrange(desc(PROP_CWE)) 


# The number of exporting slaughterhouses in "MATO GROSSO","SAO PAULO","GOIAS","RONDONIA"
slaughter_data_summary %>% 
  filter(LH_STATE %in% c("MATO GROSSO","SAO PAULO","MATO GROSSO DO SUL","RONDONIA")) %>%
  summarise(SUM_NUM_SH = sum(NUM_SH), 
            SUM_PROP_SH = sum(PROP_NUM_SH))


# And the proportion of exports which come from each state 
# ... calculated based on the location of cattle production, not slaughter
data_simple %>%
  mutate(TOTAL_CWE = sum(CWE),
         TOTAL_NUM_SH = n_distinct(LH_CNPJ)) %>%
  group_by(STATE) %>%
  summarise(SUM_CWE = sum(CWE),
            PROP_CWE = SUM_CWE / median(TOTAL_CWE)) %>% 
  ungroup() %>%
  arrange(desc(PROP_CWE)) %>%
  filter(STATE %in% c("MATO GROSSO","SAO PAULO","MATO GROSSO DO SUL","RONDONIA")) %>%
  summarise(SUM_PROP_CWE = sum(PROP_CWE))


# The proportion of production and exports from the North East
left_join(prod_per_state, exp_per_state, by = c("STATE_NAME" = "STATE", "YEAR")) %>%
  filter(YEAR %in% c(2015:2017)) %>%
  mutate(EXPORTED_CWE_TONS = ifelse(is.na(EXPORTED_CWE_TONS), 0, EXPORTED_CWE_TONS),
         CWE_PRODUCTION_TONS = SUM_CW_PRODUCTION_TONS_5_YR / 5) %>%
  mutate(TOTAL_EXPORTED_CWE_TONS = sum(EXPORTED_CWE_TONS), 
         TOTAL_CWE_PRODUCTION_TONS = sum(CWE_PRODUCTION_TONS)) %>%
  filter(STATE_NAME %in% c(
    "ALAGOAS",
    "BAHIA",
    "CEARA",
    "PARAIBA",
    "PERNAMBUCO",
    "MARANHAO",
    "PIAUI",
    "RIO GRANDE DO NORTE",
    "SERGIPE"
  )) %>%
  summarise(PERC_EXPORTS = sum(EXPORTED_CWE_TONS) / unique(TOTAL_EXPORTED_CWE_TONS) * 100,
            PERC_PRODUCTION = sum(CWE_PRODUCTION_TONS) / unique(TOTAL_CWE_PRODUCTION_TONS) * 100)

# tidy up
rm(slaughter_data_summary, 
   prod_per_state, 
   exp_per_state)


# Contributions of biomes to exports --------------------------------------


# The proportion of exports' sourcing per biome
# ... NB "Unknown" biome is where small cattle flows were aggregated
# ... since cattle come from many municipalities per slaughterhousee, small flows
# ... were aggregated to reduce the data size.
# ... see methods
data_simple %>% 
  mutate(TOTAL_FOB = sum(FOB),
         TOTAL_CWE = sum(CWE)) %>%
  group_by(BIOME) %>%
  summarise(PROP_FOB = sum(FOB) / median(TOTAL_FOB),
            PROP_CWE = sum(CWE) / median(TOTAL_CWE)) %>%
  arrange(desc(PROP_CWE))


# Consolidation in exporting companies ------------------------------------


# The number of traders
data_simple %>% 
  summarise(NUM_EXPORTERS = n_distinct(EXPORTER),
            NUM_EXPORTER_GROUPS = n_distinct(EXPORTER_GROUP),
            NUM_IMPORTER = n_distinct(IMPORTER),
            NUM_IMPORTER_GROUPS = n_distinct(IMPORTER_GROUP))


# The number of traders operating in all years
data_simple %>% 
  group_by(EXPORTER) %>% 
  mutate(NUM_YEARS = n_distinct(YEAR)) %>%
  distinct(EXPORTER, NUM_YEARS) %>%
  group_by(NUM_YEARS) %>% 
  summarise(NUM_EXPORTERS = n_distinct(EXPORTER)) %>%
  ungroup() %>%
  mutate(PERC_EXPORTERS = NUM_EXPORTERS / sum(NUM_EXPORTERS) * 100 )


# Export share of the big-3, incl subsidiaries
data_simple %>%
  mutate(TOTAL_FOB = sum(FOB), 
         TOTAL_CWE = sum(CWE),
         TOTAL_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA)) %>%
  group_by(EXPORTER_GROUP) %>% 
  summarise(SUM_FOB = sum(FOB), 
            PROP_FOB = SUM_FOB / median(TOTAL_FOB),
            SUM_CWE = sum(CWE),
            PROP_CWE = SUM_CWE / median(TOTAL_CWE),
            SUM_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA),
            PROP_DFRS = SUM_DFRS_HA / median(TOTAL_DFRS_HA)) %>%
  ungroup() %>% 
  arrange(desc(SUM_FOB)) %>%
  top_n(n =  10, wt = SUM_FOB) %>%
  mutate(CUMSUM_PROP_CWE = cumsum(PROP_CWE),
         CUMSUM_PROP_FOB = cumsum(PROP_FOB),
         CUMSUM_PROP_DFRS = cumsum(PROP_DFRS)) %>% 
  select(EXPORTER_GROUP, CUMSUM_PROP_CWE, CUMSUM_PROP_FOB, CUMSUM_PROP_DFRS)


# Legal Amazon states where they operate exporting slaughterhouses
data_simple %>% 
  filter(
    PRODUCT_DESC != "Live cattle exports",
    EXPORTER_GROUP %in% c("JBS","Minerva","Marfrig"), 
    !grepl("UNKNOWN SLAUGHTERHOUSE", LH_CNPJ),
    LH_STATE != "UNKNOWN"
  ) %>%
  distinct(EXPORTER_GROUP, LH_STATE) %>%
  filter(LH_STATE %in% legal_amazon) %>%
  mutate(PRESENT = "YES") %>%
  spread(EXPORTER_GROUP, PRESENT)


# Big three's exports as a % of exports per biome
data_simple %>% 
  group_by(BIOME) %>%
  mutate(TOTAL_CWE = sum(CWE)) %>%
  group_by(EXPORTER_GROUP, BIOME) %>%
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CWE)) %>%
  filter(EXPORTER_GROUP %in% c("JBS","Minerva","Marfrig"), 
         BIOME %in% c("Amazon", "Pampa"))


# The proportion of each company's sourcing which comes from each biome
# ... Marfrig export 20.9% from Pampa
data_simple %>% 
  filter(EXPORTER_GROUP %in% c("JBS","Minerva","Marfrig")
  ) %>%
  group_by(EXPORTER_GROUP) %>%
  mutate(TOTAL_CWE = sum(CWE)) %>%
  group_by(EXPORTER_GROUP, BIOME) %>%
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CWE)) %>%
  filter(PROP_CWE > 0.15)


# Vertically integrated businesses 
# What proportion of exports handled by companies who operate own slaughterhouses?
# ... these are Abiec members, exporters with names matching SIF slaughterhouses
# ... and a few additional cases we know of.
data_simple %>% 
  filter(PRODUCT_DESC != "Live cattle exports") %>%
  mutate(
    EXPORTER = str_to_upper(EXPORTER),
    OPERATE_OWN_SH = ifelse(EXPORTER %in% c(sifs_bov$SIF_COMPANY_CLEAN, "BONMART FRIGORIFICO") |
                              EXPORTER %in% c(abiec$COMPANY,
                                              "JNJ COMERCIAL IMPORTADORA E EXPORTADORA DE CARNES",
                                              "MONDELLI INDUSTRIA DE ALIMENTOS", 
                                              "COOPERATIVA DOS PRODUTORES DE CARNE E DERIVADOS DE GURUPI",
                                              "FRIGORIFICO VALE DO SAPUCAI",
                                              "FRIGOSUL FRIGORIFICO SUL") |
                              EXPORTER_GROUP %in% c("JBS","Minerva","Marfrig","Vpr Brasil Importacoes e Exportacoes"),
                            TRUE,
                            FALSE)
  ) %>%
  mutate(TOTAL_FOB = sum(FOB)) %>%
  group_by(OPERATE_OWN_SH) %>%
  summarise(NUM_EXPORTERS = n_distinct(EXPORTER),
            PROP_FOB = sum(FOB) / median(TOTAL_FOB))


# Sourcing distances of slaughterhouses -----------------------------------


# Note: distances are calculated based on the distance to the municipal-center
# ... as explained in the methods


# Number of municipalities identified per slaughterhouse
# ... for direct/indirect networks
# Extract distances
num_munis_indirect <- 
  supply_sheds_df_sbst %>% 
  select(taxNum, SH_STATE, SH_GEOCODE, muni_df) %>%
  unnest() %>% 
  filter(PROP_FLOWS > 0) %>%
  group_by(taxNum) %>%
  summarise(NUM_MUNIS = n_distinct(GEOCODE)) %>% 
  ungroup() %>%
  summarise(MEAN_NUM_MUNI = mean(NUM_MUNIS),
            MEDIAN_NUM_MUNI = median(NUM_MUNIS),
            SD_NUM_MUNI = sd(NUM_MUNIS))
num_munis_direct <- 
  supply_sheds_df_sbst %>% 
  select(taxNum, SH_STATE, SH_GEOCODE, muni_direct_df) %>%
  unnest() %>% 
  filter(PROP_FLOWS > 0) %>%
  group_by(taxNum) %>%
  summarise(NUM_MUNIS = n_distinct(GEOCODE)) %>%
  ungroup() %>%
  summarise(MEAN_NUM_MUNI = mean(NUM_MUNIS),
            MEDIAN_NUM_MUNI = median(NUM_MUNIS),
            SD_NUM_MUNI = sd(NUM_MUNIS))
num_munis_direct
num_munis_indirect


# Number of municipalities identified per slaughterhouse
# ... for direct/indirect networks
# ... which supply 80% of supply
num_munis_indirect_80_pc <- 
  supply_sheds_df_sbst %>% 
  select(taxNum, SH_STATE, SH_GEOCODE, muni_df) %>%
  unnest() %>% 
  filter(PROP_FLOWS > 0) %>%
  group_by(taxNum) %>%
  arrange(desc(PROP_FLOWS)) %>%
  mutate(CUMSUM_PROP_FLOWS = cumsum(PROP_FLOWS)) %>%
  filter(CUMSUM_PROP_FLOWS <= 0.8) %>%
  summarise(NUM_MUNIS = n_distinct(GEOCODE)) %>% 
  ungroup() %>%
  summarise(MEAN_NUM_MUNI = mean(NUM_MUNIS),
            MEDIAN_NUM_MUNI = median(NUM_MUNIS),
            SD_NUM_MUNI = sd(NUM_MUNIS))
num_munis_indirect_80_pc


# 80% of sourcing is achieved within an average of 279 km
slaughterhouse_sourcing_distances %>% 
  filter(!is.na(SH_STATE)) %>%
  mutate(DISTANCE_KM = TOTAL_LENGTH / 1000) %>%
  filter(CUMSUM_PROP_FLOWS > 0.8) %>% 
  group_by(taxNum) %>% 
  filter(CUMSUM_PROP_FLOWS == min(CUMSUM_PROP_FLOWS)) %>% 
  ungroup() %>% 
  summarise(MEAN_DISTANCE_KM = mean(DISTANCE_KM),
            MEDIAN_DISTANCE_KM = median(DISTANCE_KM),
            SD_DISTANCE_KM = sd(DISTANCE_KM))


# tidy up
rm(num_munis_indirect_80_pc, 
   num_munis_direct,
   num_munis_indirect)


# Where do major markets source from? -------------------------------------


# Volumes and value of trade to major markets
data_simple %>% 
  mutate(COUNTRY_GROUP = ifelse(COUNTRY_GROUP %in% c("Italy","Netherlands", "United Kingdom","Other EU markets"), "EU", COUNTRY_GROUP)) %>%
  mutate(TOTAL_FOB = sum(FOB),
         TOTAL_VOL = sum(VOL),
         TOTAL_DFRS = sum(BEEF_DEF_5_YEAR_HA)) %>%
  group_by(COUNTRY_GROUP) %>%
  summarise(PROP_FOB = sum(FOB) / median(TOTAL_FOB),
            PROP_VOL = sum(VOL) / median(TOTAL_VOL),
            PROP_DFRS = sum(BEEF_DEF_5_YEAR_HA) / median(TOTAL_DFRS)) %>%
  ungroup() %>%
  arrange(desc(PROP_VOL))  %>%
  top_n(n = 10, wt = PROP_VOL)


# Count number of states major markets sourced from
data_simple %>% 
  mutate(COUNTRY_GROUP = ifelse(COUNTRY_GROUP %in% c("Italy","Netherlands", "United Kingdom","Other EU markets"), "EU", COUNTRY_GROUP)) %>%
  filter(!grepl("UNKNOWN", LH_STATE), 
         COUNTRY_GROUP %in% c("China", "Egypt","Russia", "EU")) %>% 
  group_by(COUNTRY_GROUP) %>% 
  summarise(NUM_STATES = n_distinct(LH_STATE))


# UK and US imports
data_simple %>% 
  filter(COUNTRY %in% c("United States", "United Kingdom")) %>%
  group_by(COUNTRY) %>% 
  mutate(TOTAL_FOB = sum(FOB), 
         TOTAL_CWE = sum(CWE)) %>%
  group_by(COUNTRY, PRODUCT_DESC) %>% 
  summarise(PERC_FOB = sum(FOB) / unique(TOTAL_FOB) * 100, 
            PERC_CWE = sum(CWE) / unique(TOTAL_CWE) * 100) %>%
  ungroup() %>%
  filter(PRODUCT_DESC == "Processed beef products")


# What proportion of Venezuela's imports originate from the legal amazon, esp Para?
data_simple %>% 
  filter(COUNTRY == "Venezuela") %>%
  mutate(TOTAL_CWE = sum(CWE), 
         LEGAL_AMAZON = ifelse(STATE %in% legal_amazon, TRUE, FALSE)) %>%
  group_by(LEGAL_AMAZON) %>%
  summarise(PERC_CWE = sum(CWE) / unique(TOTAL_CWE) * 100) %>%
  ungroup()
data_simple %>% 
  filter(COUNTRY == "Venezuela") %>%
  mutate(TOTAL_CWE = sum(CWE)) %>%
  group_by(STATE) %>%
  summarise(PERC_CWE = sum(CWE) / median(TOTAL_CWE) * 100) %>%
  ungroup() %>%
  filter(STATE == "PARA")


# Venezeula's importance for the live cattle market
data_simple %>% 
  filter(PRODUCT_DESC == "Live cattle exports") %>%
  group_by(YEAR) %>%
  mutate(TOTAL_CWE = sum(CWE)) %>%
  group_by(COUNTRY, YEAR) %>%
  summarise(SUM_CWE = sum(CWE), 
            PERC_CWE = SUM_CWE / median(TOTAL_CWE) * 100) %>%
  ungroup() %>%
  filter(COUNTRY == "Venezuela") 


# Venezuela's imports over time
data_simple %>% 
  filter(COUNTRY == "Venezuela") %>%
  group_by(COUNTRY, YEAR) %>%
  summarise(SUM_FOB_MILLIONS = sum(FOB) / 1e06) %>%
  ungroup()


# How are deforestation risks distributed? --------------------------------


# Total deforestation risk of cattle exports
data_simple %>% 
  group_by(YEAR) %>%
  summarise(DEFORESTATION_RISK = sum(BEEF_DEF_5_YEAR_HA)) %>%
  ungroup()


# Total cattle deforestation risk/year
left_join(prod, prod_dfrs_df, by = c("YEAR","GEOCODE")) %>%
  filter(YEAR %in% c(2015:2017)) %>%
  mutate(CW_PRODUCTION_TONS_ANNUAL = CW_PRODUCTION_TONS_5_YR / 5, 
         DFRS_HA_PER_TON_5_YR = ifelse(is.na(DFRS_HA_PER_TON_5_YR), 0, DFRS_HA_PER_TON_5_YR),
         DFRS_HA_PER_YEAR = CW_PRODUCTION_TONS_ANNUAL * DFRS_HA_PER_TON_5_YR) %>%
  group_by(YEAR) %>% 
  summarise(DEFORESTATION_RISK = sum(DFRS_HA_PER_YEAR)) 


# Export deforestation risk, per biome
data_simple %>%
  group_by(YEAR) %>% 
  mutate(TOTAL_DFRS_RISK_HA = sum(BEEF_DEF_5_YEAR_HA)) %>%
  group_by(YEAR, BIOME) %>% 
  summarise(DEFORESTATION_RISK = sum(BEEF_DEF_5_YEAR_HA), 
            PERC_DFRS_RISK = DEFORESTATION_RISK / unique(TOTAL_DFRS_RISK_HA) * 100) %>%
  filter(BIOME %in% c("Amazon","Cerrado","Atlantic Forest")) 


# Spotlight on specific companies -----------------------------------------


# Irmaos Goncalves' ranking for deforestation risk
data_simple %>% 
  group_by(EXPORTER) %>% 
  summarise(SUM_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA)) %>%
  ungroup() %>% 
  arrange(desc(SUM_DFRS_HA)) %>%
  mutate(RANKING = row.names(.)) %>%
  filter(EXPORTER == "Irmaos Goncalves Comercio e Industria") 


# Irmaos Goncalves' deforestation risk / year
data_simple %>% 
  group_by(EXPORTER, YEAR) %>% 
  summarise(SUM_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA)) %>%
  ungroup() %>% 
  filter(EXPORTER == "Irmaos Goncalves Comercio e Industria") 


# Identify Iraos Goncalves' facility in RO
sifs_bov %>% 
  filter(grepl("IRMÃOS GONÇALVES", SIF_COMPANY)) %>% 
  select(SIF_COMPANY, SIF_UF, SIF_MUNICIPALITY)


# Frisa
(frisa_slaughterhouses <- 
  data_simple %>% 
  filter(EXPORTER == "Frisa Frigorifico Rio Doce") %>% 
  pull(LH_CNPJ) %>%
  unique())
sifs_all %>% 
  filter(SIF_CNPJ %in% frisa_slaughterhouses) %>% 
  select(SIF_COMPANY, SIF_UF, SIF_MUNICIPALITY, SIF_STATUS, SIF_CATEGORIES)


# Pampeano Alimentos
data_simple %>% 
  filter(EXPORTER == "Pampeano Alimentos") %>% 
  pull(LH_STATE) %>%
  unique()


# tidy up 
rm(frisa_slaughterhouses)


# Spotlight on specific markets -------------------------------------------


# US deforestation risk
# ... historical risk from processed beef imports
# ... without reopening fresh meat imports
data_simple %>%
  filter(COUNTRY_GROUP == "United States", 
         PRODUCT_DESC == "Processed beef products") %>%
  group_by(YEAR, PRODUCT_DESC) %>%
  summarise(SUM_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA))


# China's deforestation risk
data_simple %>%
  group_by(YEAR) %>%
  mutate(TOTAL_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA)) %>%
  filter(COUNTRY_GROUP == "China") %>%
  group_by(YEAR) %>%
  summarise(SUM_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA),
            PERC_DFRS_RISK = SUM_DFRS_HA / unique(TOTAL_DFRS_HA) * 100)


# Changes in deforestation risk of slaughterhouses supplying mainland China


# (a) slaughterhouses exporting to China 2015-2017
china_shs <- 
  data_simple %>% 
  filter(COUNTRY == "China", !grepl("UNKNOWN", LH_CNPJ)) %>% 
  group_by(LH_CNPJ, EXPORTER, LH_STATE) %>%
  summarise(RR_KTON = sum(BEEF_DEF_5_YEAR_HA, na.rm = T) / (sum(CWE) / 1000) * 1e03)


# (b) new China-approved slaughterhouses   
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


# Compare the deforestation risk of the previous/new slaughterhouses supplying
# ... mainland China
china_shs %>%
  bind_rows(china_new_shs_dfrs2, .id = "id") %>%
  mutate(id = ifelse(id == 1, "Slaughterhouses\nexporting to\nChina (mainland)\nbetween 2015-2017", "Slaughterhouses\nlicensed for export to\nChina (mainland) in 2019")) %>%
  group_by(id) %>% 
  summarise(MEAN_DFRS_HA_PER_KTON_5_YR = mean(RR_KTON),
            SD_DFRS_HA_PER_KTON_5_YR = sd(RR_KTON))


# Proportion of Hong Kong's imports which were sourced from slaughterhouses newly licensed for
# ... export to mainland China
data_simple %>% 
  filter(COUNTRY == "Hong Kong") %>%
  mutate(TOTAL_CWE = sum(CWE)) %>%
  filter(LH_CNPJ %in% china_new_shs_dfrs2$LH_CNPJ) %>%
  summarise(PERC_CWE = sum(CWE) / unique(TOTAL_CWE) * 100)


# tidy up
rm(china_new_shs_all, china_new_shs, china_shs, china_new_shs_dfrs2)


# Biome-breakdown of deforestation risk per market ------------------------


# Deforestation risk of EU and Halal markets, per biome
data_simple %>% 
  mutate(COUNTRY_GROUP = case_when(COUNTRY_GROUP == "Other EU markets" | COUNTRY %in% c("United Kingdom","Italy","Netherlands") ~ "EU",
                                   COUNTRY_GROUP == "Other halal markets" | COUNTRY %in% c("Egypt","Iran") ~ "Halal markets",
                                   TRUE ~ COUNTRY_GROUP)) %>%
  filter(COUNTRY_GROUP %in% c("EU","Halal markets")) %>%
  group_by(COUNTRY_GROUP, YEAR) %>%
  mutate(TOTAL_CWE = sum(CWE),
         TOTAL_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA)) %>%
  group_by(COUNTRY_GROUP, YEAR, BIOME) %>%
  summarise(SUM_CWE = sum(CWE),
            SUM_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA),
            PROP_CWE = SUM_CWE / median(TOTAL_CWE),
            PROP_DFRS = SUM_DFRS_HA / median(TOTAL_DFRS_HA),
            TOTAL_DFRS_HA = median(TOTAL_DFRS_HA)) %>%
  filter(BIOME %in% c("Amazon","Cerrado")) 


# Live cattle exports -----------------------------------------------------


# Value of exports/year
data_simple %>% 
  filter(PRODUCT_DESC == "Live cattle exports") %>%
  group_by(YEAR) %>% 
  summarise(FOB_MILLION = sum(FOB) / 1e06)


# Overall value and deforestation risk of live cattle exports
data_simple %>% 
  mutate(TOTAL_FOB_MILLION = sum(FOB) / 1e06,
         TOTAL_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA)) %>%
  filter(PRODUCT_DESC == "Live cattle exports") %>%
  summarise(FOB_MILLION = sum(FOB) / 1e06, 
            PERC_EXPORT_VALUE = FOB_MILLION / unique(TOTAL_FOB_MILLION) * 100,
            PERC_DFRS_RISK = sum(BEEF_DEF_5_YEAR_HA) / unique(TOTAL_DFRS_HA) * 100)


# Major exporters
# ... Minerva is by far the largest
data_simple %>% 
  filter(PRODUCT_DESC == "Live cattle exports") %>%
  mutate(TOTAL_FOB_MILLION = sum(FOB) / 1e06) %>%
  group_by(EXPORTER) %>% 
  summarise(FOB_MILLION = sum(FOB) / 1e06, 
            PERC_VALUE = FOB_MILLION / unique(TOTAL_FOB_MILLION)) %>%
  ungroup() %>% 
  arrange(desc(FOB_MILLION))


# Comparison of domestic market & exports ---------------------------------


# Extract exports and domestic market sourcing per geocode 
exports_per_geocode <- 
  data_simple %>% 
  group_by(YEAR, GEOCODE) %>%
  summarise(EXPORTED_CWE_TONS = sum(CWE) / 1000) %>%
  ungroup()
production_per_geocode <- 
  prod %>%
  mutate(PRODUCTION_TONS = CW_PRODUCTION_TONS_5_YR / 5) %>% 
  full_join(exports_per_geocode, by = c("YEAR","GEOCODE")) %>%
  mutate(EXPORTED_CWE_TONS = ifelse(is.na(EXPORTED_CWE_TONS), 0, EXPORTED_CWE_TONS),
         PRODUCTION_TONS = ifelse(is.na(PRODUCTION_TONS), 0, PRODUCTION_TONS),
         DOMESTIC_CWE_TONS = PRODUCTION_TONS - EXPORTED_CWE_TONS, 
         DOMESTIC_CWE_TONS = ifelse(DOMESTIC_CWE_TONS < 0, 0, DOMESTIC_CWE_TONS)) %>%
  select(YEAR, GEOCODE, DOMESTIC_CWE_TONS, EXPORTED_CWE_TONS) %>%
  left_join(muni_biomes, by = c("GEOCODE")) %>%
  filter(YEAR %in% c(2015:2017))


# Calculate the proportion of deforestation risk linked to each market
production_per_geocode %>% 
  gather(MARKET, CWE_TONS, DOMESTIC_CWE_TONS:EXPORTED_CWE_TONS) %>%
  mutate(MARKET = ifelse(MARKET == "DOMESTIC_CWE_TONS","Domestic market","Exports")) %>%
  left_join(prod_dfrs_df, by = c("YEAR","GEOCODE")) %>%
  mutate(DFRS_HA_PER_TON_5_YR = ifelse(is.na(DFRS_HA_PER_TON_5_YR), 0, DFRS_HA_PER_TON_5_YR),
         DFRS_HA_PER_YEAR = CWE_TONS * DFRS_HA_PER_TON_5_YR) %>%
  group_by(MARKET, YEAR) %>% 
  summarise(CWE_TONS = sum(CWE_TONS), 
            DEFORESTATION_RISK = sum(DFRS_HA_PER_YEAR)) %>%
  group_by(YEAR) %>%
  mutate(TOTAL_DFRS_RISK = sum(DEFORESTATION_RISK)) %>%
  ungroup() %>%
  mutate(PERC_DFRS_RISK = DEFORESTATION_RISK / unique(TOTAL_DFRS_RISK) * 100)


# Exports from the Amazon
production_per_geocode %>% 
  filter(BIOME == "Amazon") %>%
  gather(MARKET, CWE_TONS, DOMESTIC_CWE_TONS:EXPORTED_CWE_TONS) %>%
  mutate(MARKET = ifelse(MARKET == "DOMESTIC_CWE_TONS","Domestic market","Exports")) %>%
  left_join(prod_dfrs_df, by = c("YEAR","GEOCODE")) %>%
  mutate(DFRS_HA_PER_TON_5_YR = ifelse(is.na(DFRS_HA_PER_TON_5_YR), 0, DFRS_HA_PER_TON_5_YR),
         DFRS_HA_PER_YEAR = CWE_TONS * DFRS_HA_PER_TON_5_YR) %>%
  group_by(MARKET, YEAR) %>% 
  summarise(CWE_TONS = sum(CWE_TONS), 
            DEFORESTATION_RISK = sum(DFRS_HA_PER_YEAR)) %>%
  group_by(YEAR) %>%
  mutate(TOTAL_CWE_TONS = sum(CWE_TONS), 
         TOTAL_DFRS_RISK = sum(DEFORESTATION_RISK)) %>%
  ungroup() %>%
  mutate(PERC_PRODUCTION = CWE_TONS / unique(TOTAL_CWE_TONS),
         PERC_DFRS_RISK = DEFORESTATION_RISK / unique(TOTAL_DFRS_RISK) * 100)


# Commodity-linked deforestation ------------------------------------------


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


# Calculate the proportion of deforestation allocated to each commodity
min_r <- function(x){round(min(x),2)}
max_r <- function(x){round(max(x),2)}
mean_r <- function(x){round(mean(x),2)}
sd_r <- function(x){round(min(x),2)}
summary_df <- 
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
    -`Max 5 years between\ndeforestation & pasture`,
    -YEAR
  ) %>% 
  group_by(BIOME) %>% 
  summarise_all(list(min = min_r, max = max_r, mean = mean_r, sd = sd_r )) %>% 
  ungroup() %>%
  gather(METRIC, VALUE, `Deforestation becoming\npasture after > 5 yrs_min`:`Unattributed deforestation_sd`)


# Proportion of deforstation linked to cattle per biome
summary_df %>%
  filter(grepl("Cattle deforestation", METRIC))


# Proportion of deforestation unattributed in the Amazon and Cerrado
dfrs_comp %>%
  filter(BIOME %in% c("AMAZON", "CERRADO")) %>%
  group_by(DATA, YEAR) %>%
  summarise(SUM_AREA_HA = sum(SUM_AREA_HA)) %>%
  ungroup() %>%
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
    -`Max 5 years between\ndeforestation & pasture`,
    -YEAR
  ) %>% 
  summarise_all(list(min = min_r, max = max_r, mean = mean_r, sd = sd_r )) %>% 
  gather(METRIC, VALUE, `Deforestation becoming\npasture after > 5 yrs_min`:`Unattributed deforestation_sd`) %>%
  filter(grepl("Unattributed", METRIC))


# tidy up
rm(dfrs_comp, summary_df, 
   min_r, max_r, mean_r, sd_r,
   total_ibge_dfrs, 
   past_dfrs_5yr,
   past_dfrs_allyr, 
   soy_past_dfrs_5yr,
   annual_soy_dfrs)


# 10-yr amortized deforestation risk --------------------------------------


# Export-associated deforestation risk per year
# ... calculated using a 10-yr amortization
cd_all %>% 
  group_by(YEAR, LINKED_CNPJ) %>%
  summarise(SUM_CWE_PER_FLOW = sum(CWE_PER_FLOW)) %>%
  left_join(supply_sheds_df_dfrs_amortized, by = c("LINKED_CNPJ", "YEAR")) %>%
  unnest() %>%
  mutate(DFRS_HA = SUM_DFRS_PER_TON * SUM_CWE_PER_FLOW / 1000) %>%
  group_by(YEAR, NUM_RANDOMISATION) %>%
  summarise(SUM_DFRS_HA = sum(DFRS_HA),
            DFRS_HA_PER_TON = SUM_DFRS_HA / (sum(SUM_CWE_PER_FLOW) / 1000)) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  summarise(SUM_DFRS_MEAN = mean(SUM_DFRS_HA),
            SUM_DFRS_05 = quantile(SUM_DFRS_HA, probs = c(0.05)),
            SUM_DFRS_95 = quantile(SUM_DFRS_HA, probs = c(0.95)),
            DFRS_HA_PER_KTON_MEAN = mean(DFRS_HA_PER_TON) * 1000,
            DFRS_HA_PER_KTON_05 = quantile(DFRS_HA_PER_TON, probs = c(0.05)) * 1000,
            DFRS_HA_PER_KTON_95 = quantile(DFRS_HA_PER_TON, probs = c(0.95)) * 1000
  ) 


# TAC & G4 commitments ----------------------------------------------------


# Proportion of exports covered by commitments
data_simple %>% 
  mutate(TOTAL_CWE = sum(CWE),
         TOTAL_DFRS = sum(BEEF_DEF_5_YEAR_HA)) %>%
  group_by(ZDC) %>%
  summarise(PERC_CWE = sum(CWE) / unique(TOTAL_CWE) * 100, 
            SUM_DFRS = sum(BEEF_DEF_5_YEAR_HA),
            PROP_DFRS = SUM_DFRS / unique(TOTAL_DFRS)) %>%
  ungroup() 


# Proportion of exports from the Legal Amazon covered by commitments
data_simple %>% 
  filter(BIOME == "Amazon") %>%
  mutate(TOTAL_CWE = sum(CWE),
         TOTAL_DFRS = sum(BEEF_DEF_5_YEAR_HA)) %>%
  group_by(ZDC) %>%
  summarise(PERC_CWE = sum(CWE) / unique(TOTAL_CWE) * 100, 
            SUM_DFRS = sum(BEEF_DEF_5_YEAR_HA),
            PROP_DFRS = SUM_DFRS / unique(TOTAL_DFRS)) %>%
  ungroup() %>%
  mutate(TOTAL_DFRS_RISK = sum(SUM_DFRS))


# Risk linked to G4 signatoties (and subsidiaries)
data_simple %>% 
  filter(EXPORTER_GROUP %in% c("JBS", "Minerva","Marfrig")) %>%
  mutate(TOTAL_CWE = sum(CWE),
         TOTAL_DFRS = sum(BEEF_DEF_5_YEAR_HA)) %>%
  group_by(BIOME) %>%
  summarise(PERC_CWE = sum(CWE) / unique(TOTAL_CWE) * 100, 
            SUM_DFRS = sum(BEEF_DEF_5_YEAR_HA),
            PROP_DFRS = SUM_DFRS / unique(TOTAL_DFRS)) %>%
  ungroup() %>%
  mutate(TOTAL_DFRS_RISK = sum(SUM_DFRS))


# Proportion of G4 companies' risk outside the Legal Amazon
data_simple %>% 
  filter(EXPORTER_GROUP %in% c("JBS", "Minerva","Marfrig")) %>%
  # filter(STATE %in% legal_amazon) %>%
  mutate(TOTAL_CWE = sum(CWE),
         TOTAL_DFRS = sum(BEEF_DEF_5_YEAR_HA)) %>%
  filter(!STATE %in% legal_amazon) %>%
  summarise(PERC_CWE = sum(CWE) / unique(TOTAL_CWE) * 100, 
            SUM_DFRS = sum(BEEF_DEF_5_YEAR_HA),
            PROP_DFRS = SUM_DFRS / unique(TOTAL_DFRS)) %>%
  ungroup()


# Statistics quoted in the supplementary material -------------------------


# Data used to make municipal link ----------------------------------------


# The proportion of flows traced back to municipalities
# ... using GTA or state-level SIGSIF data
data_simple %>%
  mutate(TOTAL_CWE = sum(CWE)) %>% 
  group_by(GEOCODE_SOURCE) %>% 
  summarise(PROP = sum(CWE) / unique(TOTAL_CWE))


# Where state-level SIGSIF data was used to identify the municipalities supplying
# ... cattle to each slaughterhouse, most was linked to slaughter in:
# ... SP, GO, RO.
data_simple %>%
  filter(GEOCODE_SOURCE == "SIGSIF") %>%
  mutate(TOTAL_CWE = sum(CWE)) %>% 
  group_by(STATE) %>% 
  summarise(PERC = sum(CWE) / unique(TOTAL_CWE) * 100) %>%
  ungroup() %>%
  arrange(desc(PERC)) %>% 
  mutate(CUMSUM_PERC = cumsum(PERC))


# Excess exports ----------------------------------------------------------


# Identify what proportion of exports are in 'excess' of our estimate of municipal production
exp_per_geocode <- 
  data_simple %>%
  filter(GEOCODE != "UNKNOWN") %>%
  group_by(STATE, GEOCODE, YEAR) %>% 
  summarise(EXPORTED_CWE_TONS = sum(CWE) / 1000) %>%
  ungroup()
prod %>%
  filter(CW_PRODUCTION_TONS_5_YR != 0) %>%
  left_join(exp_per_geocode, by = c("GEOCODE", "YEAR")) %>%
  filter(YEAR %in% c(2015:2017)) %>%
  mutate(EXPORTED_CWE_TONS = ifelse(is.na(EXPORTED_CWE_TONS), 0, EXPORTED_CWE_TONS),
         CWE_PRODUCTION_TONS = CW_PRODUCTION_TONS_5_YR / 5,
         EXCESS = ifelse(EXPORTED_CWE_TONS > CWE_PRODUCTION_TONS, EXPORTED_CWE_TONS - CWE_PRODUCTION_TONS, 0)) %>%
  group_by(YEAR) %>%
  summarise(PERC_EXCESS = sum(EXCESS) / sum(EXPORTED_CWE_TONS) * 100)
         

# Where do these 'excess' exports arise?
prod %>%
  filter(CW_PRODUCTION_TONS_5_YR != 0) %>%
  left_join(exp_per_geocode, by = c("GEOCODE", "YEAR")) %>%
  filter(YEAR %in% c(2015:2017)) %>%
  mutate(EXPORTED_CWE_TONS = ifelse(is.na(EXPORTED_CWE_TONS), 0, EXPORTED_CWE_TONS),
         CWE_PRODUCTION_TONS = CW_PRODUCTION_TONS_5_YR / 5,
         EXCESS = ifelse(EXPORTED_CWE_TONS > CWE_PRODUCTION_TONS, EXPORTED_CWE_TONS - CWE_PRODUCTION_TONS, 0) ) %>%
  group_by(YEAR) %>% 
  mutate(TOTAL_EXCESS = sum(EXCESS)) %>% 
  group_by(YEAR, STATE) %>%
  summarise(PERC_EXCESS = sum(EXCESS) / unique(TOTAL_EXCESS) * 100) %>% 
  ungroup() %>% 
  filter(STATE == "SAO PAULO")


# Sourcing patterns of Brazil's major exporters ---------------------------


# Exports handled by JBS, Minerva, Marfrig directly
data_simple %>% 
  mutate(TOTAL_CWE = sum(CWE),
         TOTAL_FOB = sum(FOB)) %>%
  filter(EXPORTER %in% c("JBS","Minerva","Marfrig")) %>%
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CWE),
            PROP_FOB = sum(FOB) / median(TOTAL_FOB)) 


# Exports handled by JBS, Minerva, Marfrig & their subsidiaries
data_simple %>% 
  mutate(TOTAL_CWE = sum(CWE),
         TOTAL_FOB = sum(FOB)) %>%
  filter(EXPORTER_GROUP %in% c("JBS","Minerva","Marfrig")) %>%
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CWE),
            PROP_FOB = sum(FOB) / median(TOTAL_FOB)) 


# Exports handled by JBS, Minerva, Marfrig & their subsidiaries
# ... split per trader
data_simple %>% 
  filter(YEAR == 2017) %>%
  mutate(TOTAL_CWE = sum(CWE),
         TOTAL_FOB = sum(FOB)) %>%
  filter(EXPORTER_GROUP %in% c("JBS","Minerva","Marfrig")) %>%
  group_by(EXPORTER_GROUP) %>%
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CWE),
            PROP_FOB = sum(FOB) / median(TOTAL_FOB)) 


# Live cattle as a % of Minerva's export portfolio (by value)
data_simple %>% 
  filter(EXPORTER_GROUP == "Minerva") %>%
  mutate(TOTAL_FOB = sum(FOB)) %>%
  group_by(PRODUCT_DESC) %>%
  summarise(PROP_FOB = sum(FOB) / median(TOTAL_FOB)) %>%
  arrange(desc(PROP_FOB))


# Deforestation risk of major exporters -----------------------------------


# market share and deforestation risk of big 3
data_simple %>% 
  mutate(TOTAL_CWE = sum(CWE),
         TOTAL_FOB = sum(FOB), 
         TOTAL_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA)) %>%
  filter(EXPORTER_GROUP %in% c("JBS","Minerva","Marfrig")) %>%
  group_by(EXPORTER_GROUP) %>%
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CWE),
            PROP_FOB = sum(FOB) / median(TOTAL_FOB),
            SUM_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA), 
            PROP_DFRS = SUM_DFRS_HA / unique(TOTAL_DFRS_HA) ) 


# Deforestation risk per biome
data_simple %>% 
  filter(EXPORTER_GROUP %in% c("JBS")) %>%
  group_by(EXPORTER_GROUP) %>%
  mutate(TOTAL_CWE = sum(CWE),
       TOTAL_FOB = sum(FOB), 
       TOTAL_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA)) %>%
  group_by(EXPORTER_GROUP, BIOME) %>%
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CWE),
            PROP_FOB = sum(FOB) / median(TOTAL_FOB),
            SUM_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA), 
            PROP_DFRS = SUM_DFRS_HA / unique(TOTAL_DFRS_HA) ) %>%
  filter(BIOME %in% c("Amazon","Cerrado"))


# Minerva's deforestation risk per year
data_simple %>% 
  group_by(YEAR) %>%
  mutate(TOTAL_CWE = sum(CWE),
         TOTAL_FOB = sum(FOB), 
         TOTAL_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA)) %>%
  filter(EXPORTER_GROUP %in% c("Minerva")) %>%
  group_by(EXPORTER_GROUP, YEAR) %>%
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CWE),
            PROP_FOB = sum(FOB) / median(TOTAL_FOB),
            SUM_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA), 
            PROP_DFRS = SUM_DFRS_HA / unique(TOTAL_DFRS_HA) ) 


# Minerva's live cattle deforestation risk per year
data_simple %>% 
  filter(EXPORTER_GROUP %in% c("Minerva")) %>%
  group_by(YEAR) %>%
  mutate(TOTAL_FOB = sum(FOB), 
         TOTAL_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA)) %>%
  group_by(PRODUCT_DESC, YEAR) %>%
  summarise(PROP_FOB = sum(FOB) / median(TOTAL_FOB),
            SUM_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA), 
            PROP_DFRS = SUM_DFRS_HA / unique(TOTAL_DFRS_HA) ) %>%
  filter(PRODUCT_DESC == "Live cattle exports")


# Marfrig export share
data_simple %>% 
  group_by(YEAR) %>%
  mutate(TOTAL_CWE = sum(CWE),
         TOTAL_FOB = sum(FOB), 
         TOTAL_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA)) %>%
  filter(EXPORTER_GROUP %in% c("Marfrig")) %>%
  group_by(EXPORTER_GROUP, YEAR) %>%
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CWE),
            PROP_FOB = sum(FOB) / median(TOTAL_FOB),
            SUM_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA), 
            PROP_DFRS = SUM_DFRS_HA / unique(TOTAL_DFRS_HA) ) 


# Beef imports into China -------------------------------------------------


data_simple %>% 
  filter(COUNTRY == "China") %>% 
  mutate(TOTAL_CWE = sum(CWE)) %>%
  group_by(LH_STATE) %>% 
  summarise(PROP_CWE = sum(CWE) / median(TOTAL_CWE)) %>%
  ungroup() %>% 
  arrange(desc(PROP_CWE))
data_simple %>% 
  filter(COUNTRY == "China") %>% 
  group_by(LH_STATE) %>% 
  summarise(NUM_SH = n_distinct(LH_GEOCODE)) %>%
  ungroup() %>% 
  arrange(desc(NUM_SH))


# Deforestation risk linked to actors adopting ZDCs -----------------------


# Proportion of deforestation linked using GTAs
data_simple %>%
  filter(EXPORTER_GROUP %in% c("JBS","Minerva","Marfrig")) %>%
  mutate(TOTAL_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA)) %>% 
  group_by(GEOCODE_SOURCE) %>% 
  summarise(PROP_DFRS = sum(BEEF_DEF_5_YEAR_HA) / unique(TOTAL_DFRS_HA))
data_simple %>%
  mutate(TOTAL_DFRS_HA = sum(BEEF_DEF_5_YEAR_HA)) %>% 
  group_by(GEOCODE_SOURCE) %>% 
  summarise(PROP_DFRS = sum(BEEF_DEF_5_YEAR_HA) / unique(TOTAL_DFRS_HA))



# USA deforestation risks -------------------------------------------------


# Deforestation risk per product per year
usa_dfrs %>% 
  group_by(YEAR, HS4_DESC) %>% 
  summarise(SUM_DFRS_HA = sum(SUM_DFRS)) %>%
  ungroup() %>%
  filter(HS4_DESC != 'Offal')