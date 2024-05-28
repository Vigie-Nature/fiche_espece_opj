# HEADER --------------------------------------------
#
# Author:     Maël Pretet
# Copyright     Copyright 2024 - Maël Pretet
# Email:      mael.pretet1@mnhn.fr
#
# Date:     2024-03-26
#
# Script Name:    fonctions/function_graphics.R
#
# Script Description:   Fonctions graphiques pour les figures de 
#   la fiche espèce (script "dashboard_espece.qmd")
#
#
# ------------------------------------


#########################################
#------------ Observations -------------#
#########################################

gg_histo_plotly <- function(df_hp, x = "nom_espece", y = "rel_ab",
                            fill = "nom_espece", 
                            title = "Proportion d'abondance de chaque espèce parmi toutes les observations", 
                            ytxt = "% d'abondance",
                            limits, breaks, couleur){
  
  gg = df_hp %>%
    ggplot() +
    geom_bar(mapping = aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill),
                           text = paste0(nom_espece, " : ",
                                         scales::percent(!!sym(y), accuracy = 0.01))),
             stat = "identity") +
    scale_x_discrete(limits=limits) +
    ylab(ytxt) +
    ggtitle(title) +
    theme_cowplot() +
    theme(axis.title.y = element_blank(),
          title = element_text(size = 9),
          legend.position = "none" ) +
    scale_fill_manual(breaks = breaks,
                      values = couleur) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    coord_flip()
  
  return(ggplotly(gg, tooltip = "text"))
}

#########################################
#-------- Variations annuelles ---------#
#########################################

# Histogramme
gg_histo <- function(df_histo, x = "date", y = "sum_ab",
                     ytxt = "Abondance totale", dmin, dmax, title = ""){
  
  return(ggplot() +
           geom_bar(data = df_histo, aes(x = !!sym(x), y = !!sym(y)),
                    color = "#8A173A", fill="#ab0739", stat="identity") +
           theme_cowplot() +
           scale_x_date(date_labels = "%Y-%b", date_breaks = "9 months",
                        limits = c(dmin, dmax)) +
           theme(axis.title.x = element_blank(),
                 axis.text.x = element_text(angle = 0, size = 8),
                 axis.title.y =  element_text(size = 11, color = "#ab0739"),
                 axis.text.y = element_text(size = 8),
                 plot.title = element_text(face = "plain", size = 9)) +
           ggtitle(title) +
           ylab(ytxt) )
}

# Données sous forme de ligne
gg_line <- function(df_line, x = "date", y = "n",
                    xtxt = "Date de collection", ytxt = "Nombre de sessions",
                    color = "#ff795c", dmin, dmax, title = ""){
  ggplot(df_line, aes(x = !!sym(x), y = !!sym(y))) +
    geom_line(color = color) +
    theme_cowplot() +
    scale_x_date(date_labels = "%Y-%b", date_breaks = "9 months",
                 limits = c(dmin, dmax)) +
    theme(axis.title.x = element_text(size = 10),
          axis.text.x = element_text(angle = 0, size = 8),
          axis.title.y = element_text(size = 9, color = color),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(face = "plain", size = 9)) +
    ggtitle(title) +
    xlab(xtxt) +
    ylab(ytxt)
}

# Carte d'abondance
carte_ab <- function(shape_map, fill_map, fill_title, fill_color,
                     fill_cat, map_title = ""){
  
  carte <- ggplot(shape_map) + 
    geom_sf(aes(fill = fill_map), show.legend = "fill") +
    scale_fill_manual(values = fill_color, breaks = fill_cat, drop = FALSE)+
    labs(fill = fill_title) +
    ggtitle(map_title) +
    theme_light() +
    theme(title = element_text(size = 9))
  
  return(carte)
}

#########################################
#------------- Phénologie --------------#
#########################################

# Graphique en echarts4r
aes_echarts <- function(plot_e, xlab, ylab, title, line_color, one_y = TRUE){
  
  plot_e <- plot_e %>%
    # e_bar(sum_ab) %>%
    e_legend(top = "3%") %>%
    e_tooltip(e_tooltip_pointer_formatter("currency"),
              axisPointer = list(type = "cross")) %>%
    e_datazoom(x_index = 0, type = "slider", bottom = "3%") %>%
    e_toolbox_feature(feature = "saveAsImage") %>%
    e_toolbox_feature(feature = "dataView", readOnly = TRUE) %>%
    e_x_axis(name=xlab,
             nameLocation = "middle", nameGap= 27) %>%
    e_grid(bottom = "20%") %>%
    e_color(color = line_color) %>%
    e_y_axis(name=ylab, nameLocation = 'middle',
             nameGap= 50) %>%
    e_title(text = paste(title),
            textStyle = list(fontSize = list(14)))
  
  if (one_y) {
    plot_e <- plot_e %>%
      e_x_axis(max = 52) 
  }
  
  return(plot_e)
}



# Histogramme + line
histo_line <- function(df_histo, x_h = "date", y_h = "sum_ab",
                       df_ligne, x_l = "date", y_l = "n",
                       div, xtxt = "Date de collection", ytxt = "Abondance",
                       ytxtsec = "Nombre d'observations", title = ""){
  
  return(ggplot() +
    geom_bar(data = df_histo, aes(x = !!sym(x_h), y = !!sym(y_h)),
             color = "#8A173A", fill="#ab0739", stat="identity") +
    geom_line(data = df_ligne, aes(x = !!sym(x_l), y = !!sym(y_l)*div, group = 1),
              color = "#ff795c") +
    theme_cowplot() +
    scale_x_date(date_labels = "%Y-%b", date_breaks = "9 months") +
    theme(axis.text.x = element_text(angle = 0, size = 8),
          axis.title.y.left =  element_text(size = 12, color = "#ab0739"),
          axis.title.y.right = element_text(size = 12, color = "#ff795c"),
          plot.title = element_text(face = "plain", size = 12)) +
    xlab(xtxt) +
    ylab(ytxt) +
    ggtitle(title) +
    scale_y_continuous(sec.axis = sec_axis(~./div, name = ytxtsec)))
}



#########################################
#--------------- Cartes ----------------#
#########################################


# Carte animée avec plotly
gg_carte = function(an, df_sp, france){
  df_mois = data.frame(mois = c("01", "02", "03", "04", "05", "06",
                                "07", "08", "09", "10", "11", "12"),
                       jardin_id = 0,
                       latitude = NA,
                       longitude = NA,
                       sum_ab = 0)
  df_migration = df_sp %>%
    filter(annee == an) %>%
    mutate(mois = strftime(date_collection, "%m")) %>%
    group_by(mois, jardin_id, latitude, longitude) %>%
    summarise(sum_ab = sum(abondance)) %>%
    filter(sum_ab != 0) %>%
    dplyr::union(df_mois)
  
  if (an == as.numeric(strftime(Sys.Date(), "%Y")) ){
    df_migration = df_migration %>%
      filter(mois <= strftime(Sys.Date(), "%m"))
  }
  
  gg =ggplot(france) +
    geom_sf(fill = "#f0f0f0", color = "#a0a0a0") +
    geom_point(data = df_migration, aes(x = longitude, y=latitude, frame = mois), color = "red") +
    theme_minimal()
  
  return(gg %>%
           ggplotly()  %>%
           animation_opts(transition = 0, frame = 1000))
}

# Carte pour un mois d'une année (pour création gif)
gg_carte_mois = function(month, df_sp, france){
  df_mois = data.frame(mois = month,
                       jardin_id = 0,
                       latitude = NA,
                       longitude = NA,
                       sum_ab = 0)
  df_migration = df_sp %>%
    mutate(mois = strftime(date_collection, "%m")) %>%
    filter(mois == month) %>%
    group_by(mois, jardin_id, latitude, longitude) %>%
    summarise(sum_ab = sum(abondance)) %>%
    filter(sum_ab != 0) %>%
    dplyr::union(df_mois)
  
  gg = ggplot(france) +
    geom_sf(fill = "#f0f0f0", color = "#a0a0a0") +
    geom_point(data = df_migration, aes(x = longitude, y=latitude), color = "red") +
    theme_minimal() +
    ggtitle(month)
  
  return(gg)
}

