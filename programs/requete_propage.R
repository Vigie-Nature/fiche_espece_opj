
df_propage = import_from_mosaic(query = read_sql_query("SQL/export_a_plat_propage.sql"),
                                database_name = "espaces_verts")

df_propage = import_from_mosaic(query = read_sql_query("../../../Téléchargements/export_a_plat_propage.sql"),
                                database_name = "espaces_verts")
