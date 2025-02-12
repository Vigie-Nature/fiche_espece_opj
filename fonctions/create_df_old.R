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
  rename(type_Engrais = `type d'engrais`,
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
         frequenceBouillieBordelaise = `bouillie bordelaise`) %>%
  group_by(zfk) %>%
  filter(annee == max(annee)) %>%
  ungroup()

# Liste user à partir de 2019
# source("fonctions/create_df_all_sp.R")
# df_all_sp %>%
#   select(c(id, email, jardin_id, latitude, longitude))

table_user_2019 = read.csv2(file = "data/table_user_2019.csv")


# Jointure des tables
df_join_obs_part = obpj_participations %>%
  mutate(ytmp = as.integer(strftime(date, "%Y"))) %>%
  full_join(obpj_observations, by = c("participationpk" = "participation_fk")) %>%
  left_join(obpj_pratiques_trait, by = c("zpk" = "zfk")) %>%
  select(-ytmp)

# Traitement des anciennes données pour adapter au format des nouvelles
df_old_data = df_join_obs_part %>%
  # On supprime les observations sans participation correspondante
  # On supprime les observations avec des NA rentrées en \N 
  filter(!is.na(codepostal),
         codepostal != "",
         !is.na(date),
         long != "\\N",
         lat != "\\N",
         lat != 0) %>%
  # On renomme les colonnes selon les métadonnées de 2019 et +
  dplyr::rename(code_postal = codepostal,
                participation_id = participationpk,
                latitude = lat,
                longitude = long,
                abondance = nb_individus,
                type_environnement = environnement,
                nom_espece = sp_name,
                jardin_id = zpk) %>%
  mutate(nom_espece = if_else(nom_espece == "Cuivrés", "Cuivré", nom_espece)) %>%
  # On supprime les données pour les espèces absentes de la liste principale
  filter(nom_espece %in% liste_principale) %>%
  # On modifie le type ou le contenu de certaines colonnes
  mutate(date = as.Date(date),
         annee = as.integer(strftime(date, "%Y")),
         # adaptation pour les département en Corse
         dept_code = if_else(substr(code_postal, 1, 2)=="20",
                             if_else(code_postal < "20200",
                                     "2A", "2B"),
                             substr(code_postal, 1, 2)),  
         num_semaine = strftime(date, "%V"),
         user_id = -dense_rank(email),
         jardin_id = -jardin_id,
         longitude = as.double(longitude),
         latitude = as.double(latitude),
         an_sem = paste0(annee, "-S", num_semaine),
         type_environnement = case_when(is.na(type_environnement) ~ NA,
                                        type_environnement == "" ~ NA,
                                        type_environnement == "urbain" ~ "Urbain",
                                        type_environnement == "peri-urbain" ~ "Péri-urbain",
                                        type_environnement == "rural" ~ "Rural"),
         participation_id = -(participation_id))%>%
  # Pré 2014, les dates d'observations sont en milieu de mois, et post 2014
  # elles sont en début de mois. Il semble y avoir eu une transition créant des
  # dates en début de mois entre 2011 et 2014 qu'on supprime
  filter(!( (annee <= 2013 & strftime(date, "%d") == "01") | 
              (annee == 2014 & strftime(date, "%d")=="15") )) %>%
  left_join(reg_dep, by = c("dept_code" = "code_departement")) %>%
  # Les users toujours actifs récupèrent l'id actuelle
  left_join(table_user_2019, by = c("email" = "email")) %>%
  mutate(user_id = if_else(!is.na(id), id, user_id)) %>%
  # On supprime les colonnes qui ne sont plus présentes
  select(-c(nom_user, name, adresse, ville, id, type, 
            distance_champ_cultive, frequence)) 

# Sauvegarde des données historiques dans un fichier RDS
saveRDS(object = df_old_data, file = "data/rdata/df_old_data.rds")


