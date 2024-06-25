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

#########################################
#---------- Dataframe initial ----------#
#########################################

# Df de toutes les espèces
if (!exists("df_all_sp")) {
  source("fonctions/create_df_all_sp.R")
}

df_all_sp = df_all_sp %>%
  filter(!is.na(dept_code),         # suppression des départements nuls
         str_length(dept_code)==2,  # suppression des drom-com
         annee >= 2019,
         nom_espece %in% liste_principale) %>%         # suppression des données avant 2019
  mutate(an_sem = if_else(as.numeric(num_semaine) < 10,
                          paste0(annee, "-S0", num_semaine),
                          paste0(annee, "-S", num_semaine))) %>%
  left_join(reg_dep, by = c("dept_code" = "code_departement")) # ajout des départements

# Df de l'historique
if (file.exists("data/history/donnees_opj_hebdo_ok.csv")) {
  df_history = read.csv2(file = "data/history/donnees_opj_hebdo_ok.csv",
                         header = F, encoding = "latin-1")
}

# Df d'une espèce
df_sp = df_all_sp %>%
  filter(nom_espece == sp_name)

df_sp_ab = df_sp %>%
  filter(abondance != 0)

# Carte de france en objet sf
france <- read_sf(paste0("carte/contour-des-departements.geojson"))

#########################################
#-------- Calcul d'indicateurs ---------#
#########################################

# Nombre de jardins participant aux observations
nb_jardin = length(unique(df_all_sp$jardin_id))
# Nombre de jardins où un individu a été observé
nb_jardin_obs = length(unique(df_sp_ab$jardin_id))
# Abondance maximale (calculée en groupant sur les années et les départements)
nb_max_ab = df_sp %>%
  group_by(annee, nom_departement, nom_region) %>%
  summarise(sum_ab = sum(abondance)) %>%
  ungroup() %>%
  filter(sum_ab == max(sum_ab)) %>%
  as.data.frame()

#########################################
#----------- Nom de l'espèce -----------#
#########################################

#----- Carte d'abondance -----#

# Df abondance sur toutes les données
df_dep = df_sp %>% 
  group_by(dept_code) %>%
  summarise(n = sum(abondance),
            nb_participation = n_distinct(participation_id),
            nb_jard = n_distinct(jardin_id)) %>%
  mutate(ab_moy = n/nb_jard,
         ab_rel = n/nb_participation,
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
                             ab_rel > 0.6 ~ "Extrêmement abondant")) 

df_dep = df_dep[c(1:6, 29:30, 7:28, 31:96),]

cat_carte_all = c("0", "1-50", "51-100", "101-300", "301-500", "+ de 500")
cat_carte_all_moy = c("0", "1-2", "3-5", "6-10", "+ de 10")
cat_carte_tendance_moy = c("Pas de détection", "Peu abondant", "Abondant",
                           "Très abondant", "Extrêmement abondant")
couleurs = c("#7f7f7f", "#ffef6c", "#f7b905", "#ff7400", "#ff0000", "#950000")

#########################################
#------------ Observations -------------#
#########################################

# Nombre de fois où l'individu est observé
nb_obs_idv = nrow(df_sp_ab)
# Nombre total d'individus observés (somme de l'abondance)
nb_idv_cpt = sum(df_sp_ab$abondance)

#----- Graphiques -----#

# Df abondance par espèce
df_repartition = df_all_sp %>% 
  group_by(nom_espece) %>% 
  summarise(sum_ab = sum(abondance),
            rel_ab = sum(abondance)/sum(df_all_sp$abondance)) %>%
  arrange(sum_ab) %>%
  mutate(couleur = c(rep("#3138cc", 10), rep("#6893fc", 9), rep("#90d3ff", 9)),
         couleur = if_else(nom_espece == sp_name, color_flag, couleur))

#########################################
#-------- Variations annuelles ---------#
#########################################

# On calcule le nombre d'observations sur df_sp, sinon le summarise par ligne
# prendrait en compte un nombre d'observations x 28 (nombre d'espèces et donc
# de lignes à chaque observation)

# Semaine
df_nb_obs_date <- df_sp %>%
  mutate(date = as.Date(date_collection)) %>%
  group_by(date) %>%
  summarise(n = n()) %>%
  arrange(date)

#----- Abondance moyenne par département -----#

# Df abondance par année
df_dep_y = df_sp %>% 
  group_by(dept_code, annee) %>%
  summarise(n = sum(abondance),
            nb_jard = n_distinct(jardin_id)) %>%
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
nb_part_par_sem = df_all_sp %>%
  mutate(num_semaine = as.integer(num_semaine)) %>%
  group_by(annee, num_semaine) %>%
  summarise(nb_part = n_distinct(participation_id))

# Abondance relative
df_ab_rel <- df_sp %>%
  mutate(num_semaine = as.integer(num_semaine)) %>%
  group_by(annee, num_semaine, date_collection) %>%
  summarise(sum_ab = sum(abondance)) %>%        # Somme des abondances
  left_join(nb_part_par_sem, by = c("annee" = "annee",
                                    "num_semaine" = "num_semaine")) %>%
  mutate(sum_ab_rel = sum_ab/nb_part) %>%  # Division par le nombre de participations
  arrange(num_semaine) %>%
  group_by(annee, num_semaine) %>%
  mutate(col_sup = if_else(n() == 2 & sum_ab == 0, 1, 0)) %>%
  filter(col_sup == 0) %>%
  select(!col_sup) %>%
  ungroup()

# Phénologie
df_freq_rel <- df_sp_ab %>%
  mutate(num_semaine = as.integer(num_semaine)) %>%
  group_by(annee, num_semaine) %>%
  summarise(sum_obs = n()) %>%       # Somme des observations
  full_join(nb_part_par_sem, by = c("annee" = "annee",
                                    "num_semaine" = "num_semaine")) %>%
  mutate(freq_rel = if_else(is.na(sum_obs), 0, sum_obs/nb_part)) %>%   # Division par le nombre de participations
  arrange(num_semaine)

# Présence moyenne
df_date_wm = df_sp %>%
  filter(abondance !=0, annee != strftime(Sys.Date(), "%Y")) %>%
  mutate(semaine = as.integer(strftime(date_collection, '%V'))) %>%
  group_by(annee) %>%
  summarise(sum_sp = weighted.mean(semaine, abondance))

df_date_wm_sqrt = df_sp %>%
  filter(abondance !=0, annee != strftime(Sys.Date(), "%Y")) %>%
  mutate(semaine = as.integer(strftime(date_collection, '%V'))) %>%
  left_join(df_date_wm, by = c("annee" = "annee")) %>%
  mutate(minus = abondance*((semaine - sum_sp)^2) ) %>%
  group_by(annee, sum_sp) %>%
  summarise(sum_minus = sum(minus), 
            n = n()) %>%
  mutate(rmse = sqrt(sum_minus/n))

#########################################
#------------- Grégarité ---------------#
#########################################

# Moyenne d'abondance
df_moyenne_greg = df_all_sp %>%
  filter(abondance!= 0) %>%
  group_by(nom_espece) %>%
  summarise(m_abn = mean(abondance), n = n()) %>%
  mutate(sd = 1.96*sqrt(m_abn/n)) %>%
  arrange(desc(m_abn)) %>%
  as.data.frame()

# Espèce seule
df_gregarite = data.frame(nb_idv = as.numeric(names(summary(as.factor(df_sp_ab$abondance)))),
                          frequence = summary(as.factor(df_sp_ab$abondance))) %>%
  mutate(freq_prc = frequence/sum(frequence),
         class_idv = case_when(nb_idv < 2 ~ "1",
                               nb_idv >= 2 & nb_idv < 5 ~ "2 à 4",
                               nb_idv >= 5 & nb_idv < 10 ~ "5 à 9",
                               nb_idv >= 10 ~ "10 et +"))

# Toutes les espèces
df_gregarite_all = df_all_sp %>%
  filter(abondance!= 0) %>%
  mutate(ab_grega = factor(if_else(abondance == 1, "1 individu", "+ de 1 individu"),
                           levels = c("1 individu", "+ de 1 individu"))) %>%
  group_by(nom_espece, ab_grega) %>%
  summarise(n = n()) %>%
  group_by(nom_espece) %>%
  mutate(sum_n = sum(n)) %>%
  ungroup() %>%
  mutate(prop_grega = n/sum_n) %>%
  group_by(nom_espece) %>%
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

lst_param = list(bois, champ, prairie)

#----- Position + barycentre -----#

# Df des jardins positionnés sur la carte
df_jardin_point = df_sp %>%
  group_by(jardin_id, latitude, longitude) %>%
  summarise(sum_ab = sum(abondance)) %>%
  filter(!is.na(latitude)) %>%
  mutate(Présence = if_else(sum_ab == 0, "Espèce non observée", "Espèce observée"),
         alpha = if_else(sum_ab == 0, 0.7, 1)) %>%
  arrange(Présence)

# Fonction à appliquer aux df pour les barycentres
bary_function <- function(df,
                          gb1 = c("annee", "jardin_id", "latitude", "longitude"),
                          gb2 = c("annee")){
  
  df <- df %>%
    group_by(!!!syms(gb1)) %>%     # On groupe selon les paramètres de gb1
    summarise(sum_ab = sum(abondance)) %>%
    filter(!is.na(latitude)) %>%
    ungroup() %>%
    mutate(lat_pond = latitude*sum_ab,          # Calcul des latitudes et
           long_pond = longitude*sum_ab) %>%    # longitudes pondérées
    group_by(!!!syms(gb2)) %>%           # On groupe selon les paramètres de gb2
    # Pour chaque année, on somme les latitudes et longitudes pondérées
    # et l'abondance totale
    summarise(across(matches("*_pond"), \(x) sum(x, na.rm = TRUE)),
              across(matches("sum_ab"), sum)) %>%
    # On divise la latitude et la longitude pondérée de chaque année par 
    # la pondération (donc la somme des abondances)
    mutate(latitude = lat_pond/sum_ab,
           longitude = long_pond/sum_ab)
  
  return(df)
}

# Df barycentre de tous les jardins chaque année
df_bary_base<- df_all_sp %>%
  group_by(annee, jardin_id, latitude, longitude) %>%
  summarise(sum_ab = n()) %>%
  filter(!is.na(latitude)) %>%
  ungroup() %>%
  mutate(lat_pond = latitude*sum_ab,
         long_pond = longitude*sum_ab) %>%
  group_by(annee) %>%
  summarise(across(matches("*_pond"), \(x) sum(x, na.rm = TRUE)),
            across(matches("sum_ab"), sum)) %>%
  mutate(latitude = lat_pond/sum_ab,
         longitude = long_pond/sum_ab,
         nom_espece = "Jardins",
         color = "#0baaff") %>%
  as.data.frame()

# Df du barycentre pour une espèce
df_bary_one_sp <- cbind(bary_function(df = df_sp),
                        data.frame(nom_espece = sp_name,
                                   color = "red"))

# Df des barycentres pour toutes les espèces
df_bary_all_sp <- bary_function(df = df_all_sp,
                                gb1 = c("annee", "jardin_id", "latitude",
                                        "longitude", "nom_espece"),
                                gb2 = c("annee", "nom_espece")) %>%
  mutate(nom_esp_min = if_else(nom_espece == sp_name, sp_name, "Autres"))



#------------------------------------------------------------------------------#
#                                    TESTS                                     #
#------------------------------------------------------------------------------#

#########################################
#------------ Co-occurence -------------#
#########################################

df_co = df_all_sp %>%
  filter(participation_id %in% unique(df_sp_ab$participation_id))

df_occurence = df_all_sp %>%
  select(participation_id, an_sem, annee, nom_espece, abondance) %>%
  pivot_wider(names_from = nom_espece, values_from = abondance)%>%
  filter(!!sym(sp_name) != 0)%>%
  mutate(participation_id = as.character(participation_id),
         annee = as.character(annee)) %>%
  mutate_if(~ any(is.numeric(.)), ~ if_else(.==0, "NON", "OUI")) %>%
  select(!c(participation_id, an_sem, annee, !!sym(sp_name)))

df_oui = apply(df_occurence, 2, function(x){return(length(which(x=="OUI"))/length(x))})
df_oui = sort(round(df_oui*100, digits = 2), decreasing = TRUE)
df_oui = data.frame(nom = names(df_oui), corr = as.numeric(df_oui))

all_names = unique(df_all_sp$nom_espece)
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

df_coocc = df_all_sp %>%
  filter(nom_espece %in% vec_name) %>%
    group_by(nom_espece, an_sem) %>%
    summarise(sum_ab = sum(abondance)) %>%
    group_by(nom_espece) %>%
    mutate(sum_sp = sum(sum_ab),
           sum_ab_norm = sum_ab/sum_sp,
           nom_espece = factor(nom_espece, levels = vec_name)) %>%
    arrange(an_sem) %>%
    relocate(nom_espece, .before = an_sem)

# Nombre d'observations totales de chaque espèce
df_nbsp_all = df_all_sp %>%
  filter(abondance != 0) %>%
  group_by(nom_espece) %>%
  summarise(n = n())

if (file.exists("data/rdata/df_heatmap.rds") &
    Sys.Date()-as.Date(file.info("data/rdata/df_heatmap.rds")$ctime) <= 1) {
  # Lecture du fichier RDS
  df_heatmap = readRDS("data/rdata/df_heatmap.rds")
}else{
  df_heatmap = data.frame()
  
  for (name in rev(df_repartition$nom_espece)) {
    df_tmp = df_all_sp %>%
      select(participation_id, an_sem, annee, nom_espece, abondance) %>%
      arrange(factor(nom_espece, levels = rev(df_repartition$nom_espece))) %>%
      pivot_wider(names_from = nom_espece, values_from = abondance) %>%
      filter(!!sym(name) != 0) %>%
      mutate(participation_id = as.character(participation_id),
             annee = as.character(annee)) %>%
      mutate_if(~ any(is.numeric(.)), ~ if_else(.==0, 0, 1)) %>%
      select(!c(participation_id, an_sem, annee)) %>%
      summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)))
    df_tmp = df_tmp / (df_nbsp_all %>% filter(nom_espece == name))$n
    df_heatmap = rbind(df_heatmap, df_tmp)
  }
  
  rownames(df_heatmap) = rev(df_repartition$nom_espece)
  df_heatmap = as.matrix(df_heatmap)
  
  saveRDS(object = df_heatmap, file = "data/rdata/df_heatmap.rds")
}

#############################
#------- Histo test --------#
#############################



df_histo_test = df_all_sp %>%
  filter(abondance!= 0) %>%
  mutate(ab_grega = factor(case_when(abondance == 1 ~ "1",
                                     abondance <= 4 ~ "2 à 4",
                                     abondance <= 9 ~ "5 à 9",
                                     abondance > 9 ~ "+ de 10"),
                           levels = c("1", "2 à 4", "5 à 9", "+ de 10"))) %>%
  group_by(nom_espece, ab_grega) %>%
  summarise(n = n()) %>%
  group_by(nom_espece) %>%
  mutate(sum_n = sum(n)) %>%
  ungroup() %>%
  mutate(prop_grega = n/sum_n) %>%
  full_join(df_moyenne_greg %>% select(nom_espece, m_abn), by = c("nom_espece" = "nom_espece"))





