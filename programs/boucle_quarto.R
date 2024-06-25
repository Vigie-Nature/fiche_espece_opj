# Librairies
library(dplyr)
# library(fs)
library(here)
library(quarto)
library(stringr)

# Fonctions
readRenviron(".env")
source("fonctions/function_import_from_mosaic.R")
source("programs/generate_html.R")

# Départements avec numéro et région
reg_dep = read.csv2("data/departements-france.csv", sep=",")

#liste principale des papillons de l'observatoire
liste_principale <- c("Amaryllis", "Argus verts", "Aurores", "Belle-dame",
                      "Brun des pélargoniums", "Citrons", "Cuivré",
                      "Demi-deuils", "Flambés", "Gazé", "Hespérides orangées",
                      "Hespérides tachetées", "Lycènes bleus", "Machaons",
                      "Mégères", "Moro-sphinx", "Myrtil", "Paon du jour", 
                      "Petites tortues", "Piérides blanches", "Procris", 
                      "Robert-le-diable", "Silène", "Souci", "Sylvains",
                      "Tabac d'Espagne", "Tircis", "Vulcain")

# Data frame des espèces
df_sp_for_names = import_from_mosaic(query = read_sql_query("SQL/export_a_plat_OPJ.sql"),
                               database_name = "spgp") %>%
  filter(!is.na(dept_code),         # suppression des départements nuls
         str_length(dept_code)==2,  # suppression des drom-com
         annee >= 2019,
         nom_espece %in% liste_principale) %>%         # suppression des données avant 2018
  mutate(an_sem = if_else(as.numeric(num_semaine) < 10,
                          paste0(annee, "-S0", num_semaine),
                          paste0(annee, "-S", num_semaine)) ) %>%
  left_join(reg_dep, by = c("dept_code" = "code_departement")) # ajout des départements


time = Sys.time()
# Boucle sur les noms d'espèces
for (sp_name in unique(df_sp_for_names$nom_espece)) {
  filename = paste0("dashboard_espece_", sp_name, ".html")
  quarto_render(input = "dashboard_espece.qmd",
                execute_params = list("sp_name" = sp_name),
                output_file = filename)

  file.copy(from = filename,
            to = paste0("out/", filename), overwrite = TRUE)
  file.remove(filename)
}
print(Sys.time() - time)


time = Sys.time()
# Boucle sur les noms d'espèces
for (sp_name in unique(df_sp_for_names$nom_espece)) {
  filename = paste0("maquette_espece_", sp_name, ".html")
  quarto_render(input = "maquette_espece.qmd",
                execute_params = list("sp_name" = sp_name),
                output_file = filename)
  
  file.copy(from = filename,
            to = paste0("out/", filename), overwrite = TRUE)
  file.remove(filename)
}
print(Sys.time() - time)

# time = Sys.time()
# # Boucle sur les noms d'espèces
# for (sp_name in unique(df_sp_for_names$nom_espece)) {
#   filename = paste0("fiche_espece_", sp_name, ".html")
#   quarto_render(input = "fiche_espece.qmd",
#                 execute_params = list("sp_name" = sp_name),
#                 output_file = filename)
#   
#   file.copy(from = filename,
#             to = paste0("out/", filename), overwrite = TRUE)
#   file.remove(filename)
# }
# print(Sys.time() - time)

# Générer le fichier html
generate_html(sort(unique(df_sp_for_names$nom_espece)))


