# HEADER --------------------------------------------
#
# Author:     Maël Pretet
# Copyright     Copyright 2024 - Maël Pretet
# Email:      mael.pretet1@mnhn.fr
#
# Date:     2024-03-26
#
# Script Name:    fonctions/create_df_all_sp.R
#
# Script Description:   Création du data frame opération papillons avec toutes
#   les espèces. Interrogation de la base de données mosaic et enregistrement
#   dans un fichier rds. Si le fichier existe déjà et qu'il date de moins d'une
#   semaine, celui-ci est directement lu.
#
#
# ------------------------------------

library(dplyr)
library(here)

if (Sys.getenv("CI") != "true") {
  readRenviron(".env")
}
source("fonctions/function_import_from_mosaic.R")
source("fonctions/var.R")


### Dataframe des données pour toutes les espèces
# -----------------------------------------------

# Mise à jour 
if (!file.exists("data/rdata/df_all_sp.rds") |                                  # Si le fichier n'existe pas OU
    (strftime(Sys.Date(), "%A") == "lundi" &                                    #  [que la date du jour est un lundi ET
     Sys.Date()-as.Date(file.info("data/rdata/df_all_sp.rds")$ctime) > 5)) {    #   que le fichier a plus de 5 jours]
  # Lecture depuis la base mosaic
  df_all_sp = import_from_mosaic(query = read_sql_query("SQL/export_a_plat_OPJ.sql"),
                                 database_name = "spgp")
  
  # On sauvegarde si on ne se trouve pas sur le serveur gitlab
  if (Sys.getenv("CI") != "true") {
    # Sauvegarde du df en format RDS
    saveRDS(object = df_all_sp, file = "data/rdata/df_all_sp.rds")
  }

}else{
  # Lecture du fichier RDS
  df_all_sp = readRDS("data/rdata/df_all_sp.rds")
}

