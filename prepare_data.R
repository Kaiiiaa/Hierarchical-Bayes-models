library(dplyr)
library(eurostat)
library(rsdmx)
library(tidyr)

ILO <- read.csv("ILO.csv")
OECD <- read.csv("oecd.csv")
# Download the data from the Eurostat table
 sbs_sc_ovw_data <- get_eurostat("sbs_sc_ovw")
 sbs_sc_sca_r2 <- get_eurostat("sbs_sc_sca_r2")

eur_ind <- c("V11110", "V12110", "V16130", "V13320")
eur_nam <- c("ENT",  "TRN", "EMP", "WG")

eur_ind1 <- c("ENT_NR", "NETTUR_MEUR", "SAL_NR")

reg<-c("EU27_2007", "EU27_2020", "EU28")

sbs_sc_ovw_data <- read.csv("sbs_sc_ovw_data.csv")
sbs_sc_sca_r2 <- read.csv("ssbs_sc_sca_r2.csv")


eurostat1 <- sbs_sc_ovw_data%>%
  filter(freq == "A", indic_sbs %in% eur_ind1, !(geo %in% reg))%>%
  mutate(Year = substr(TIME_PERIOD, 1, 4),
         Indicator = case_when(indic_sbs == "ENT_NR" ~ "ENT",
                               indic_sbs == "NETTUR_MEUR" ~ "TRN",
                               indic_sbs == "SAL_NR" ~ "EMP",
                               indic_sbs == "WAGE_MEUR" ~ "WG",
                               TRUE ~ NA_character_))%>%
  select(-c(freq, indic_sbs, TIME_PERIOD))

eurostat2 <- sbs_sc_sca_r2%>%
  filter(freq == "A", indic_sb %in% eur_ind, !(geo %in% reg))%>%
  mutate(Year = substr(TIME_PERIOD, 1, 4),
         Indicator = case_when(indic_sb == "V11110" ~ "ENT",
                               indic_sb == "V12110" ~ "TRN",
                               indic_sb == "V16130" ~ "EMP",
                               indic_sb == "V13320" ~ "WG",
                               TRUE ~ NA_character_))%>%
  select(-c(freq, indic_sb, TIME_PERIOD))

eurostat <- eurostat1%>%
  select(colnames(eurostat2))%>%
  bind_rows(eurostat2)%>%
  rename(Industry = nace_r2,
         Size_Class = size_emp,
         CountryCode = geo)%>%
  mutate(Source="Eurostat",
         Year = as.numeric(Year))


