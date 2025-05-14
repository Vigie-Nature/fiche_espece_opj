# HEADER --------------------------------------------
#
# Author:     Maël Pretet
# Copyright     Copyright 2024 - Maël Pretet
# Email:      mael.pretet1@mnhn.fr
#
# Date:     2024-03-26
#
# Script Name:    fonctions/create_df.R
#
# Script Description:   Création de tous les dataframe nécessaires pour les
#   graphiques du dashboard. Ne fonctionne pas indépendamment du script 
#   "dashboard_espece.qmd"
#
#
# ------------------------------------

source("fonctions/library.R")
source("fonctions/var.R")

if (!exists("sp_name")) {
  sp_name = "Amaryllis"
}

if (!exists("is.histo")) {
  is.histo = FALSE
}

#########################################
#---------- Dataframe initial ----------#
#########################################

# Df de toutes les espèces
if (!exists("df_opj")) {
  source("fonctions/create_df_opj.R")
}

df_opj = df_opj %>%
  filter(!is.na(dept_code),         # suppression des départements nuls
         str_length(dept_code)==2,  # suppression des drom-com
         session_year >= 2019,
         taxon %in% liste_principale) %>%         # suppression des données avant 2019
  mutate(date = as.Date(session_date),
         session_date = as.Date(session_date),
         an_sem = if_else(as.numeric(session_week) < 10,
                          paste0(session_year, "-S0", session_week),
                          paste0(session_year, "-S", session_week))) %>%
  left_join(reg_dep, by = c("dept_code" = "code_departement")) # ajout des départements

# Df de l'historique
if (is.histo) {
  df_old_data = readRDS(file = "data/rdata/df_old_data.rds")
  df_opj = bind_rows(df_opj, df_old_data)
  rm("df_old_data")
  
  df_opj_new = df_opj %>%
    filter(session_year >= 2019)
  
  df_opj_old = df_opj %>%
    filter(session_year < 2019)
  
  df_sp_new = df_opj_new %>%
    filter(taxon == sp_name)
  
  df_sp_old = df_opj_old %>%
    filter(taxon == sp_name)
  
  df_sp_ab_new = df_sp_new %>%
    filter(taxon_count != 0)
  
  df_sp_ab_old = df_sp_old %>%
    filter(taxon_count != 0)
}else{
  df_opj_new = df_opj 
  
  df_opj_old = df_opj 
  
  df_sp_new = df_opj %>%
    filter(taxon == sp_name)
  
  df_sp_old = df_opj %>%
    filter(taxon == sp_name)
  
  df_sp_ab_new = df_opj_new %>%
    filter(taxon_count != 0)
  
  df_sp_ab_old = df_opj_old %>%
    filter(taxon_count != 0)
}

# Df d'une espèce
df_sp = df_opj %>%
  filter(taxon == sp_name)

df_sp_ab = df_sp %>%
  filter(taxon_count != 0)

# Carte de france en objet sf
france <- read_sf(paste0("carte/contour-des-departements.geojson"))
france_reg <- read_sf(paste0("carte/contour-des-regions.geojson"))

#########################################
#-------- Calcul d'indicateurs ---------#
#########################################

# Nombre de jardins participant aux observations
nb_jardin = length(unique(df_opj$jardin_id))
# Nombre de jardins où un individu a été observé
nb_jardin_obs = length(unique(df_sp_ab$jardin_id))
# Abondance maximale (calculée en groupant sur les années et les départements)
nb_max_ab = df_sp %>%
  group_by(session_year, nom_departement, nom_region) %>%
  summarise(sum_ab = sum(taxon_count), .groups = 'drop') %>%
  filter(sum_ab == max(sum_ab)) %>%
  as.data.frame()

#########################################
#----------- Nom de l'espèce -----------#
#########################################

#----- Carte d'abondance -----#

# Df abondance sur toutes les données (all_data)
# fct df abondance
fct_df_abondance <- function(df){
  return(df_abondance <- df %>% 
    filter(!is.na(jardin_id)) %>%
    group_by(dept_code) %>%
    summarise(n = sum(taxon_count),
              nb_participation = n_distinct(session_id),
              nb_jard = n_distinct(jardin_id),
              nb_j_nul = sum(sapply(split(taxon_count, jardin_id),
                                    function(x) all(x == 0))),
              nb_j_non_nul = sum(sapply(split(taxon_count, jardin_id),
                                    function(x) any(x > 0))),
              .groups = 'drop') %>%
    mutate(ab_moy = n/nb_jard,
           ab_rel = n/nb_participation,
           prc_vu = nb_j_non_nul / nb_jard,
           cl_ab = case_when(n == 0 ~ "0",
                             n > 0 & n <= 50 ~ "1-50",
                             n > 50 & n <= 100 ~ "51-100",
                             n > 100 & n <= 300 ~ "101-300",
                             n > 300 & n <= 500 ~ "301-500",
                             n > 500 ~ "+ de 500"),
           cl_moy = case_when(ab_moy == 0 ~ "0",
                              ab_moy > 0 & ab_moy <= 2 ~ "1-2",
                              ab_moy > 2 & ab_moy <= 5 ~ "3-5",
                              ab_moy > 5 & ab_moy <= 10 ~ "6-10",
                              ab_moy > 10 ~ "+ de 10"),
           cl_qual = case_when(ab_rel == 0 ~ "Pas de détection",
                               ab_rel > 0 & ab_rel <= 0.2 ~ "Peu abondant",
                               ab_rel > 0.2 & ab_rel <= 0.4 ~ "Abondant",
                               ab_rel > 0.4 & ab_rel <= 0.6 ~ "Très abondant",
                               ab_rel > 0.6 ~ "Extrêmement abondant"),
           cl_jard = case_when(prc_vu == 0 ~ "0%",
                               prc_vu > 0 & prc_vu <= 0.2 ~ "0%-20%",
                               prc_vu > 0.2 & prc_vu <= 0.4 ~ "20%-40%",
                               prc_vu > 0.4 & prc_vu <= 0.6 ~ "40%-60%",
                               prc_vu > 0.6 ~ "60%-100%")) )
}

df_dep = fct_df_abondance(df = df_sp)
df_dep_old = fct_df_abondance(df = df_sp_old)
df_dep_new = fct_df_abondance(df = df_sp_new)

df_dep = df_dep[c(1:6, 29:30, 7:28, 31:96),]
df_dep_old = df_dep_old[c(1:6, 29:30, 7:28, 31:96),]
df_dep_new = df_dep_new[c(1:6, 29:30, 7:28, 31:96),]

cat_carte_all = c("0", "1-50", "51-100", "101-300", "301-500", "+ de 500")
cat_carte_all_moy = c("0", "1-2", "3-5", "6-10", "+ de 10")
cat_carte_jard = c("0%", "0%-20%", "20%-40%", "40%-60%", "60%-100%")
cat_carte_tendance_moy = c("Pas de détection", "Peu abondant", "Abondant",
                           "Très abondant", "Extrêmement abondant")
couleurs = c("#7f7f7f", "#ffef6c", "#f7b905", "#ff7400", "#ff0000", "#950000")

source("fonctions/create_df_reg.R")

#########################################
#------------ Observations -------------#
#########################################

# Nombre de fois où l'individu est observé
nb_obs_idv = nrow(df_sp_ab)
# Nombre total d'individus observés (somme de l'abondance)
nb_idv_cpt = sum(df_sp_ab$taxon_count)

#----- Graphiques -----#

# Df abondance par espèce (post 2019)
df_repartition = df_opj_new %>% 
  group_by(taxon) %>% 
  summarise(sum_ab = sum(taxon_count),
            rel_ab = sum(taxon_count)/sum(df_opj_new$taxon_count),
            .groups = 'drop') %>%
  arrange(sum_ab) %>%
  mutate(couleur = c(rep("#3138cc", 10), rep("#6893fc", 9), rep("#90d3ff", 9)),
         couleur = if_else(taxon == sp_name, color_flag, couleur))

# Df abondance par espèce (old data)
df_repartition_old = df_opj_old %>%
  group_by(taxon) %>% 
  summarise(sum_ab = sum(taxon_count),
            rel_ab = sum(taxon_count)/sum(df_opj_old$taxon_count),
            .groups = 'drop') %>%
  arrange(sum_ab) %>%
  mutate(couleur = c(rep("#3138cc", 10), rep("#6893fc", 9), rep("#90d3ff", 9)),
         couleur = if_else(taxon == sp_name, color_flag, couleur))

# Df abondance par espèce (all data)
df_repartition_new_old = df_opj %>% 
  group_by(taxon) %>% 
  summarise(sum_ab = sum(taxon_count),
            rel_ab = sum(taxon_count)/sum(df_opj$taxon_count),
            .groups = 'drop') %>%
  arrange(sum_ab) %>%
  mutate(couleur = c(rep("#3138cc", 10), rep("#6893fc", 9), rep("#90d3ff", 9)),
         couleur = if_else(taxon == sp_name, color_flag, couleur))

#########################################
#-------- Variations annuelles ---------#
#########################################

# On calcule le nombre d'observations sur df_sp, sinon le summarise par ligne
# prendrait en compte un nombre d'observations x 28 (nombre d'espèces et donc
# de lignes à chaque observation)

# Semaine
df_nb_obs_date <- df_sp %>%
  group_by(date) %>%
  summarise(n = n(),
            .groups = 'drop') %>%
  arrange(date)

#----- Abondance moyenne par département -----#

# Df abondance par année
df_dep_y = df_sp %>% 
  group_by(dept_code, session_year) %>%
  summarise(n = sum(taxon_count),
            nb_jard = n_distinct(jardin_id),
            .groups = 'drop') %>%
  mutate(ab_moy = n/nb_jard,
         cl_ab = case_when(n == 0 ~ "0",
                           n >= 1 & n <= 25 ~ "1-25",
                           n > 25 & n <= 70 ~ "26-70",
                           n > 70 & n <= 100 ~ "71-100",
                           n > 100 ~ "+ de 100"),
         cl_moy = case_when(ab_moy == 0 ~ "0",
                            ab_moy > 0 & ab_moy <= 1 ~ "0-1",
                            ab_moy > 1 & ab_moy <= 5 ~ "2-5",
                            ab_moy > 5 & ab_moy <= 10 ~ "6-10",
                            ab_moy > 10 ~ "+ de 10"))

cat_carte = c("0", "1-25", "26-70", "71-100", "+ de 100")
cat_carte_moy = c("0", "0-1", "2-5", "6-10", "+ de 10")

#########################################
#------------- Phénologie --------------#
#########################################

#----- Indicateurs relatifs -----#

# Calcul du nombre de participations sur toute l'opération par semaine
nb_part_par_sem = df_opj %>%
  mutate(session_week = as.integer(session_week)) %>%
  group_by(session_year, session_week) %>%
  summarise(nb_part = n_distinct(session_id),
            .groups = 'drop')

# New
nb_part_par_sem_new = df_opj_new %>%
  mutate(session_week = as.integer(session_week)) %>%
  group_by(session_year, session_week) %>%
  summarise(nb_part = n_distinct(session_id),
            .groups = 'drop')

# Old
nb_part_par_sem_old = df_opj_old %>%
  mutate(session_week = as.integer(session_week)) %>%
  group_by(session_year, session_week) %>%
  summarise(nb_part = n_distinct(session_id),
            .groups = 'drop')

#----- Abondance relative -----#
# All data
df_ab_rel <- df_sp %>%
  mutate(session_week = as.integer(session_week)) %>%
  group_by(session_year, session_week, session_date) %>%
  summarise(sum_ab = sum(taxon_count),
            .groups = 'drop') %>%        # Somme des abondances
  left_join(nb_part_par_sem, by = c("session_year" = "session_year",
                                    "session_week" = "session_week")) %>%
  mutate(sum_ab_rel = sum_ab/nb_part) %>%  # Division par le nombre de participations
  arrange(session_week) %>%
  group_by(session_year, session_week) %>%
  mutate(col_sup = if_else(n() == 2 & sum_ab == 0, 1, 0)) %>%
  filter(col_sup == 0) %>%
  select(!col_sup) %>%
  ungroup()

# New data
df_ab_rel_new <- df_sp_new %>%
  mutate(session_week = as.integer(session_week)) %>%
  group_by(session_year, session_week, session_date) %>%
  summarise(sum_ab = sum(taxon_count),
            .groups = 'drop') %>%        # Somme des abondances
  left_join(nb_part_par_sem, by = c("session_year" = "session_year",
                                    "session_week" = "session_week")) %>%
  mutate(sum_ab_rel = sum_ab/nb_part) %>%  # Division par le nombre de participations
  arrange(session_week) %>%
  group_by(session_year, session_week) %>%
  mutate(col_sup = if_else(n() == 2 & sum_ab == 0, 1, 0)) %>%
  filter(col_sup == 0) %>%
  select(!col_sup) %>%
  ungroup()

# Old data
df_ab_rel_old <- df_sp_old %>%
  mutate(session_week = as.integer(session_week)) %>%
  group_by(session_year, session_week, session_date) %>%
  summarise(sum_ab = sum(taxon_count),
            .groups = 'drop') %>%        # Somme des abondances
  left_join(nb_part_par_sem, by = c("session_year" = "session_year",
                                    "session_week" = "session_week")) %>%
  mutate(sum_ab_rel = sum_ab/nb_part) %>%  # Division par le nombre de participations
  arrange(session_week) %>%
  group_by(session_year, session_week) %>%
  mutate(col_sup = if_else(n() == 2 & sum_ab == 0, 1, 0)) %>%
  filter(col_sup == 0) %>%
  select(!col_sup) %>%
  ungroup()

#----- Fréquence relative -----# 
# All data
df_freq_rel <- df_sp_ab %>%
  mutate(session_week = as.integer(session_week)) %>%
  group_by(session_year, session_week) %>%
  summarise(sum_obs = n(),
            .groups = 'drop') %>%       # Somme des observations
  full_join(nb_part_par_sem, by = c("session_year" = "session_year",
                                    "session_week" = "session_week")) %>%
  mutate(freq_rel = if_else(is.na(sum_obs), 0, sum_obs/nb_part)) %>%   # Division par le nombre de participations
  arrange(session_week)

# New data
df_freq_rel_new <- df_sp_ab_new %>%
  mutate(session_week = as.integer(session_week)) %>%
  group_by(session_year, session_week) %>%
  summarise(sum_obs = n(),
            .groups = 'drop') %>%       # Somme des observations
  full_join(nb_part_par_sem_new, by = c("session_year" = "session_year",
                                        "session_week" = "session_week")) %>%
  mutate(freq_rel = if_else(is.na(sum_obs), 0, sum_obs/nb_part)) %>%   # Division par le nombre de participations
  arrange(session_week)

# Old data
df_freq_rel_old <- df_sp_ab_old %>%
  mutate(session_week = as.integer(session_week)) %>%
  group_by(session_year, session_week) %>%
  summarise(sum_obs = n(),
            .groups = 'drop') %>%       # Somme des observations
  full_join(nb_part_par_sem_old, by = c("session_year" = "session_year",
                                        "session_week" = "session_week")) %>%
  mutate(freq_rel = if_else(is.na(sum_obs), 0, sum_obs/nb_part)) %>%   # Division par le nombre de participations
  arrange(session_week)

#----- Abondance relative biogéorégions -----#
# Biogéorégions (code -> Héloïse JEUX)
invisible(capture.output({biogeoregions = st_read("carte/region_biogeo_fr/region_biogeographique.shp")}))
biogeoregions = biogeoregions %>%
  st_transform(crs = 4326) # passage du lambert 93 au WGS 84
# Certains sommets sont dupliqués : on corrige les erreurs
invalid_index <- which(!st_is_valid(biogeoregions))
biogeoregions[invalid_index, ] <- st_make_valid(biogeoregions[invalid_index, ])
biogeoregions$colours = c("#FFBA08", "#573280", "#E9806E", "#A6B1E1", "#39A0ED", "#C4EBC8")

fct_biogeo <- function(df, biogeoregions, group = TRUE){
  df_biogeo = df %>% filter(!is.na(longitude)) %>%
    select(jardin_id, longitude, latitude) %>% unique()
  df_biogeo = st_as_sf(df_biogeo, coords = c("longitude", "latitude"), crs = 4326)
  df_biogeo = st_join(df_biogeo, biogeoregions)
  df_biogeo = as.data.frame(df_biogeo) %>% select(-geometry)
  df_biogeo = df %>% left_join(df_biogeo, by = c("jardin_id"="jardin_id"))
  
  if (group) {
    df_biogeo = df_biogeo %>% filter(!is.na(CODE)) %>%
      mutate(session_week = as.integer(session_week),
             CODE = if_else(CODE=="MATL", "ATL", CODE),
             CODE = if_else(CODE=="MMED", "MED", CODE)) %>%
      group_by(CODE, session_week) %>%
      summarise(ab_rel = sum(taxon_count)/n_distinct(session_id), .groups = 'drop')
  }
  return(df_biogeo)
}

# All data
df_biogeo = fct_biogeo(df = df_opj, biogeoregions = biogeoregions, group = FALSE)
df_biogeo = df_biogeo %>% filter(!is.na(CODE)) %>%
  mutate(session_month = as.integer(strftime(session_date, "%m")),
         CODE = if_else(CODE=="MATL", "ATL", CODE),
         CODE = if_else(CODE=="MMED", "MED", CODE)) %>%
  group_by(CODE, session_month) %>%
  summarise(ab_rel = sum(taxon_count)/n_distinct(session_id), .groups = 'drop')

# New data
df_biogeo_new = fct_biogeo(df = df_opj_new, biogeoregions = biogeoregions)

# Old data
df_biogeo_old = fct_biogeo(df = df_opj_old, biogeoregions = biogeoregions)

#----- Présence moyenne -----#
df_date_wm = df_sp %>%
  filter(taxon_count !=0, session_year != strftime(Sys.Date()+365/2, "%Y")) %>%
  mutate(semaine = as.integer(strftime(date, '%V'))) %>%
  group_by(session_year) %>%
  summarise(sum_sp = weighted.mean(semaine, taxon_count), .groups = 'drop')

df_date_wm_sqrt = df_sp %>%
  filter(taxon_count !=0, session_year != strftime(Sys.Date()+365/2, "%Y")) %>%
  mutate(semaine = as.integer(strftime(date, '%V'))) %>%
  left_join(df_date_wm, by = c("session_year" = "session_year")) %>%
  mutate(minus = taxon_count*((semaine - sum_sp)^2) ) %>%
  group_by(session_year, sum_sp) %>%
  summarise(sum_minus = sum(minus), 
            n = n(), .groups = 'drop') %>%
  mutate(rmse = sqrt(sum_minus/n))

#########################################
#------------- Grégarité ---------------#
#########################################

# Moyenne d'abondance
df_moyenne_greg = df_opj %>%
  filter(taxon_count!= 0) %>%
  group_by(taxon) %>%
  summarise(m_abn = mean(taxon_count), n = n(),
            .groups = 'drop') %>%
  mutate(sd = 1.96*sqrt(m_abn/n)) %>%
  arrange(desc(m_abn)) %>%
  as.data.frame()

# Espèce seule
df_gregarite = data.frame(nb_idv = as.numeric(names(summary(as.factor(df_sp_ab$taxon_count)))),
                          frequence = summary(as.factor(df_sp_ab$taxon_count))) %>%
  mutate(freq_prc = frequence/sum(frequence),
         class_idv = case_when(nb_idv < 2 ~ "1",
                               nb_idv >= 2 & nb_idv < 5 ~ "2 à 4",
                               nb_idv >= 5 ~ "5 et +"))

# Toutes les espèces
df_gregarite_all = df_opj %>%
  filter(taxon_count!= 0) %>%
  mutate(ab_grega = factor(if_else(taxon_count == 1, "1 individu", "+ de 1 individu"),
                           levels = c("1 individu", "+ de 1 individu"))) %>%
  group_by(taxon, ab_grega) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(taxon) %>%
  mutate(sum_n = sum(n)) %>%
  ungroup() %>%
  mutate(prop_grega = n/sum_n) %>%
  group_by(taxon) %>%
  mutate(sqrt_n = sqrt(prod(prop_grega)/sum_n),
         classif = prop_grega[2]) %>%
  ungroup()

#########################################
#--------------- Jardins ---------------#
#########################################

#----- Types de jardin -----#

# Breaks pour les jardins
cat_jard <- c("0 m", "50 m", "500 m", "1000 m", "2000 m", "+ 2000 m")

# Liste de paramètres à passer pour les histogrammes
bois = list()
bois[[1]] <- "distance_bois"
bois[[2]] <- c("#2bc259", "#785016")
bois[[3]] <- "Distance bois"
bois[[4]] <- "Bois"

champ = list()
champ[[1]] <- "distance_champs"
champ[[2]] <- c("#f5a130", "#785016")
champ[[3]] <- "Distance champ"
champ[[4]] <- "Champ"

prairie = list()
prairie[[1]] <- "distance_prairie"
prairie[[2]] <- c("#5faaff", "#785016")
prairie[[3]] <- "Distance prairie"
prairie[[4]] <- "Prairie"

environnement = list()
environnement[[1]] <- "type_environnement"
environnement[[2]] <- c("#5faaff", "#785016")
environnement[[3]] <- "Type de l'environnement"
environnement[[4]] <- "Rural/Urbain"

lst_param = list(bois, champ, prairie, environnement)

#----- Position + barycentre -----#

# Df des jardins positionnés sur la carte (new data)
df_jardin_point = df_sp_new %>%
  group_by(jardin_id, latitude, longitude) %>%
  summarise(sum_ab = sum(taxon_count), .groups = 'drop') %>%
  filter(!is.na(latitude)) %>%
  mutate(Présence = if_else(sum_ab == 0, "Espèce non observée", "Espèce observée"),
         alpha = if_else(sum_ab == 0, 0.7, 1)) %>%
  arrange(Présence)

# Df des jardins positionnés sur la carte (old data)
df_jardin_point_old = df_sp_old %>%
  group_by(jardin_id, latitude, longitude) %>%
  summarise(sum_ab = sum(taxon_count), .groups = 'drop') %>%
  filter(!is.na(latitude)) %>%
  mutate(Présence = if_else(sum_ab == 0, "Espèce non observée", "Espèce observée"),
         alpha = if_else(sum_ab == 0, 0.7, 1)) %>%
  arrange(Présence)

# Df des jardins positionnés sur la carte (all data)
df_jardin_point_new_old = df_sp %>%
  group_by(jardin_id, latitude, longitude) %>%
  summarise(sum_ab = sum(taxon_count), .groups = 'drop') %>%
  filter(!is.na(latitude)) %>%
  mutate(Présence = if_else(sum_ab == 0, "Espèce non observée", "Espèce observée"),
         alpha = if_else(sum_ab == 0, 0.7, 1)) %>%
  arrange(Présence)

# Fonction à appliquer aux df pour les barycentres
bary_function <- function(df,
                          gb1 = c("session_year", "jardin_id", "latitude", "longitude"),
                          gb2 = c("session_year")){
  
  df <- df %>%
    group_by(!!!syms(gb1)) %>%     # On groupe selon les paramètres de gb1
    summarise(sum_ab = sum(taxon_count), .groups = 'drop') %>%
    filter(!is.na(latitude)) %>%
    mutate(lat_pond = latitude*sum_ab,          # Calcul des latitudes et
           long_pond = longitude*sum_ab) %>%    # longitudes pondérées
    group_by(!!!syms(gb2)) %>%           # On groupe selon les paramètres de gb2
    # Pour chaque année, on somme les latitudes et longitudes pondérées
    # et l'abondance totale
    summarise(across(matches("*_pond"), \(x) sum(x, na.rm = TRUE)),
              across(matches("sum_ab"), sum), .groups = 'drop') %>%
    # On divise la latitude et la longitude pondérée de chaque année par 
    # la pondération (donc la somme des abondances)
    mutate(latitude = lat_pond/sum_ab,
           longitude = long_pond/sum_ab)
  
  return(df)
}

# Df barycentre de tous les jardins chaque année
df_bary_base<- df_opj %>%
  group_by(session_year, jardin_id, latitude, longitude) %>%
  summarise(sum_ab = n(), .groups = 'drop') %>%
  filter(!is.na(latitude)) %>%
  ungroup() %>%
  mutate(lat_pond = latitude*sum_ab,
         long_pond = longitude*sum_ab) %>%
  group_by(session_year) %>%
  summarise(across(matches("*_pond"), \(x) sum(x, na.rm = TRUE)),
            across(matches("sum_ab"), sum), .groups = 'drop') %>%
  mutate(latitude = lat_pond/sum_ab,
         longitude = long_pond/sum_ab,
         taxon = "Jardins",
         color = "#0baaff") %>%
  as.data.frame()

# Df du barycentre pour une espèce
df_bary_one_sp <- cbind(bary_function(df = df_sp),
                        data.frame(taxon = sp_name,
                                   color = "red"))

# Df des barycentres pour toutes les espèces
df_bary_all_sp <- bary_function(df = df_opj,
                                gb1 = c("session_year", "jardin_id", "latitude",
                                        "longitude", "taxon"),
                                gb2 = c("session_year", "taxon")) %>%
  mutate(nom_esp_min = if_else(taxon == sp_name, sp_name, "Autres"))



#------------------------------------------------------------------------------#
#                                    TESTS                                     #
#------------------------------------------------------------------------------#

#########################################
#------------ Co-occurence -------------#
#########################################

df_co = df_opj %>%
  filter(session_id %in% unique(df_sp_ab$session_id))

df_occurence = df_opj %>%
  select(session_id, an_sem, session_year, taxon, taxon_count) %>%
  pivot_wider(names_from = taxon, values_from = taxon_count)%>%
  filter(!!sym(sp_name) != 0)%>%
  mutate(session_id = as.character(session_id),
         session_year = as.character(session_year)) %>%
  mutate_if(~ any(is.numeric(.)), ~ if_else(.==0, "NON", "OUI")) %>%
  select(!c(session_id, an_sem, session_year, !!sym(sp_name)))

df_oui = apply(df_occurence, 2, function(x){return(length(which(x=="OUI"))/length(x))})
df_oui = sort(round(df_oui*100, digits = 2), decreasing = TRUE)
df_oui = data.frame(nom = names(df_oui), corr = as.numeric(df_oui))

all_names = unique(df_opj$taxon)
names_no_sp = all_names[-which(all_names == sp_name)]
df_oui = df_oui %>%
  dplyr::arrange(nom)

test_hierarchy = data.frame(from = rep(sp_name, nrow(df_oui)),
                            to = df_oui$nom,
                            value = df_oui$corr) %>%
  mutate(seuil = if_else(value > 30, "red", "grey"))
test_vertices = data.frame(name = unique(c(as.character(test_hierarchy$from), as.character(test_hierarchy$to))) )

df_tab = df_oui %>%
  arrange(corr) %>%
  mutate(Corrélation = if_else(corr > 30, "Non", "Oui"),
         couleur = if_else(corr < 30, "grey", "#1e39e1"))

#########################################
#------- Phénologies conjointes --------#
#########################################

vec_name = c(sp_name, (df_oui %>% arrange(desc(corr)))$nom[1:5])

df_coocc = df_opj %>%
  filter(taxon %in% vec_name) %>%
    group_by(taxon, an_sem) %>%
    summarise(sum_ab = sum(taxon_count), .groups = 'drop') %>%
    group_by(taxon) %>%
    mutate(sum_sp = sum(sum_ab),
           sum_ab_norm = sum_ab/sum_sp,
           taxon = factor(taxon, levels = vec_name)) %>%
    arrange(an_sem) %>%
    relocate(taxon, .before = an_sem)

# Nombre d'observations totales de chaque espèce
df_nbsp_all = df_opj %>%
  filter(taxon_count != 0) %>%
  group_by(taxon) %>%
  summarise(n = n(), .groups = 'drop')

if (file.exists("data/rdata/df_heatmap.rds") &
    Sys.Date()-as.Date(file.info("data/rdata/df_heatmap.rds")$ctime) <= 1) {
  # Lecture du fichier RDS
  df_heatmap = readRDS("data/rdata/df_heatmap.rds")
}else{
  df_heatmap = data.frame()
  
  for (name in rev(df_repartition$taxon)) {
    df_tmp = df_opj %>%
      select(session_id, an_sem, session_year, taxon, taxon_count) %>%
      arrange(factor(taxon, levels = rev(df_repartition$taxon))) %>%
      pivot_wider(names_from = taxon, values_from = taxon_count) %>%
      filter(!!sym(name) != 0) %>%
      mutate(session_id = as.character(session_id),
             session_year = as.character(session_year)) %>%
      mutate_if(~ any(is.numeric(.)), ~ if_else(.==0, 0, 1)) %>%
      select(!c(session_id, an_sem, session_year)) %>%
      summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))
    df_tmp = df_tmp / (df_nbsp_all %>% filter(taxon == name))$n
    df_heatmap = rbind(df_heatmap, df_tmp)
  }
  
  rownames(df_heatmap) = rev(df_repartition$taxon)
  df_heatmap = as.matrix(df_heatmap)
  
  saveRDS(object = df_heatmap, file = "data/rdata/df_heatmap.rds")
}

#############################
#------- Histo test --------#
#############################



df_histo_test = df_opj %>%
  filter(taxon_count!= 0) %>%
  mutate(ab_grega = factor(case_when(taxon_count == 1 ~ "1",
                                     taxon_count <= 4 ~ "2 à 4",
                                     taxon_count <= 9 ~ "5 à 9",
                                     taxon_count > 9 ~ "+ de 10"),
                           levels = c("1", "2 à 4", "5 à 9", "+ de 10"))) %>%
  group_by(taxon, ab_grega) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(taxon) %>%
  mutate(sum_n = sum(n)) %>%
  ungroup() %>%
  mutate(prop_grega = n/sum_n) %>%
  full_join(df_moyenne_greg %>% select(taxon, m_abn), by = c("taxon" = "taxon"))





