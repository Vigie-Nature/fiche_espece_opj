
ajout_espece <- function(sp_name){
  txt <- paste0('    <a href="../out/maquette_espece_', sp_name, '.html">
        <img src="../data/photos/', sp_name, '.jpg", width="300", height="300", title="', sp_name, '">
    </a>
')
}

generate_html <- function(lst_names){
  html_txt <- '<!doctype html>
<html>
<head>
    <title>Opération papillons des jardins</title>
    <meta name="description" content="Front page">
    <meta name="keywords" content="html">
</head>
<body>
'
  
  for (sp_name in lst_names) {
    html_txt <- paste0(html_txt, ajout_espece(sp_name) )
  }
  
  end_txt <- '</body>
</html>'
  html_txt <- paste0(html_txt,  end_txt)
  
  cat(html_txt,
      file = (con <- file("programs/accueil.html", "w", encoding="UTF-8")) )
  close(con)
}


