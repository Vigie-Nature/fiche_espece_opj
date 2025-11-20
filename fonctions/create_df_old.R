# HEADER --------------------------------------------
#
# Author:     Maël Pretet
# Copyright     Copyright 2025 - Maël Pretet
# Email:      mael.pretet1@mnhn.fr
#
# Date:     2025-01-10
#
# Script Name:    fonctions/create_df_old.R
#
# Script Description:   Création du dataframe des données historiques de opj
#
#
# ------------------------------------

source("fonctions/library.R")
source("fonctions/var.R")

# Tables historiques
obpj_observations <- read.csv2(paste0("data/donnees_historiques/",
                                      "observations_OBJ_mensuelles_20250107"), sep = "\t")
obpj_compo <- read.csv2(paste0("data/donnees_historiques/",
                               "composition_jardin_papillons_bourdons"), sep = "\t")
obpj_participations <- read.csv2(paste0("data/donnees_historiques/",
                                        "participations_OBJ_mensuelles_20250107"), sep = "\t")
obpj_pratiques <- read.csv2(paste0("data/donnees_historiques/",
                                   "pratiques_jardin_papillons_bourdons"), sep = "\t")

# Traitement du dataframe des pratiques
obpj_pratiques_trait = obpj_pratiques %>%
  pivot_wider(names_from = X.column., values_from = engrais) %>%
  rename(session_year = annee,
         type_Engrais = `type d'engrais`,
         type_Insecticide = `type d'insecticides`,
         type_Herbicide = `type d'herbicide`,
         type_Fongicide = `type de fongicides`,
         type_AntiLimace = `type d'antilimace`,
         type_BouillieBordelaise = `type de bouillie bordelaise`,
         frequence_Engrais = engrais,
         frequence_Insecticide = insecticides,
         frequence_Herbicide = herbicides,
         frequence_Fongicide = fongicides,
         frequence_AntiLimace = antilimaces,
         frequence_BouillieBordelaise = `bouillie bordelaise`) %>%
  group_by(zfk) %>%
  filter(session_year == max(session_year)) %>%
  ungroup()

# Liste user à partir de 2019
# source("fonctions/create_df_all_sp.R")
# df_all_sp %>%
#   select(c(id, email, jardin_id, latitude, longitude))

table_user_2019 = read.csv2(file = "data/table_user_2019.csv")


# Jointure des tables
df_join_obs_part = obpj_participations %>%
  mutate(ytmp = as.integer(strftime(date, "%Y"))) %>%
  full_join(obpj_observations, by = c("participationpk" = "participation_pk")) %>%
  left_join(obpj_pratiques_trait, by = c("zpk" = "zfk")) %>%
  filter(!is.na(date)) %>%
  select(-ytmp)

# Traitement des anciennes données pour adapter au format des nouvelles
df_old_data = df_join_obs_part %>%
  # On supprime les observations sans participation correspondante
  # On supprime les observations avec des NA rentrées en \N 
  filter(!is.na(date),
         !is.na(codepostal),
         codepostal != "",
         !is.na(date),
         long != "\\N",
         lat != "\\N",
         lat != 0) %>%
  # On renomme les colonnes selon les métadonnées de 2019 et +
  dplyr::rename(session_zip_code = codepostal,
                session_id = participationpk,
                latitude = lat,
                longitude = long,
                taxon_count = nb_individus,
                type_environnement = environnement,
                taxon = sp_name,
                jardin_id = zpk) %>%
  mutate(taxon = if_else(taxon == "Cuivrés", "Cuivré", taxon)) %>%
  # On supprime les données pour les espèces absentes de la liste principale
  filter(taxon %in% liste_principale) %>%
  # On modifie le type ou le contenu de certaines colonnes
  mutate(date = as.Date(str_replace(date, "-01$", "-15")), # On uniformise les dates au 15 du mois
         session_date = as.Date(date),
         session_year = as.integer(strftime(date, "%Y")),
         # adaptation pour les département en Corse
         dept_code = if_else(substr(session_zip_code, 1, 2)=="20",
                             if_else(session_zip_code < "20200",
                                     "2A", "2B"),
                             substr(session_zip_code, 1, 2)),  
         session_week = strftime(date, "%V"),
         user_id = -dense_rank(email),
         jardin_id = -jardin_id,
         longitude = as.double(longitude),
         latitude = as.double(latitude),
         an_sem = paste0(session_year, "-S", session_week),
         type_environnement = case_when(is.na(type_environnement) ~ NA,
                                        type_environnement == "" ~ NA,
                                        type_environnement == "urbain" ~ "Urbain",
                                        type_environnement == "peri-urbain" ~ "Péri-urbain",
                                        type_environnement == "rural" ~ "Rural"),
         observation_id = -c(1:n()),
         session_id = -(session_id))%>%
  # Pré 2014, les dates d'observations sont en milieu de mois, et post 2014
  # elles sont en début de mois. Il semble y avoir eu une transition créant des
  # dates en début de mois entre 2011 et 2014 qu'on supprime
  # filter(!( (session_year <= 2013 & strftime(date, "%d") == "01") | 
  #             (session_year == 2014 & strftime(date, "%d")=="15") )) %>%
  left_join(reg_dep, by = c("dept_code" = "code_departement")) %>%
  # Les users toujours actifs récupèrent l'id actuelle
  left_join(table_user_2019, by = c("email" = "email")) %>%
  mutate(user_id = if_else(!is.na(id), id, user_id)) %>%
  # On supprime les colonnes qui ne sont plus présentes
  select(-c(nom_user, name, adresse, ville, id, type, 
            distance_champ_cultive, frequence)) 

# Sauvegarde des données historiques dans un fichier RDS
saveRDS(object = df_old_data, file = "data/rdata/df_old_data.rds")


