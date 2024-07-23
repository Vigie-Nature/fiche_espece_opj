
df_propage = import_from_mosaic(query = read_sql_query("SQL/export_a_plat_propage.sql"),
                                database_name = "espaces_verts")

saveRDS(object = df_propage, file = "data/rdata/df_propage.rds")

boxplot(as.numeric(difftime(strptime(df_propage$heure_fin, format = "%H:%M:%S"),
                            strptime(df_propage$heure_debut, format = "%H:%M:%S"),
                            units = "mins")))


write.csv2(df_propage, file = "data/rdata/df_propage.csv",
           fileEncoding = "utf-8", row.names = FALSE, col.names = TRUE)
