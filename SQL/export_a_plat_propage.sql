select `releves`.`id` AS `id_releve`,
	`espaces_verts`.`protocols`.`name` AS `protocole`,
	`sites`.`structure_id` AS `structure_id`,
	`espaces_verts`.`structures`.`name` AS `structure_nom`,
	`releves`.`user_id` AS `user_id`,
	`espaces_verts`.`users`.`username` AS `user_pseudo`,
	`espaces_verts`.`users`.`email` AS `user_email`,
	`releves`.`observation_area_id` AS `site_id`,
	json_value(`releves`.`data`,'$.espaceVert.nom') AS `site`,
	`sites`.`geodata` AS `site_coordonnees`,
	`sites`.`departement` AS `site_departement`,
	`sites`.`postcode` AS `site_code_postal`,
	json_value(`releves`.`data`,'$.espaceVert.surface') AS `surface`,
	json_unquote(json_extract(`releves`.`data`,'$.espaceVert.objectifs')) AS `objectifs`,
	json_value(`releves`.`data`,'$.espaceVert.objectifsAutrePrecision') AS `objectifs_autre`,
	`frequentation`.`title` AS `frequentation`,
	json_unquote(json_extract(`releves`.`data`,'$.espaceVert.occupationsSolAnterieures')) AS `occupations_sol_anterieures`,
	json_value(`releves`.`data`,'$.espaceVert.occupationsSolAnterieuresAutrePrecision') AS `occupations_sol_anterieures_autre`,
	json_value(`releves`.`data`,'$.espaceVert.dateCreation') AS `site_creation`,
	json_unquote(json_extract(`releves`.`data`,'$.espaceVert.travauxAnterieurs')) AS `travaux_anterieurs`,
	json_value(`releves`.`data`,'$.espaceVert.travauxAnterieursAutrePrecision') AS `travaux_anterieurs_autre`,
	json_unquote(json_extract(`releves`.`data`,'$.espaceVert.travauxSolAnterieurs')) AS `travaux_sol_anterieurs`,
	json_value(`releves`.`data`,'$.espaceVert.travauxSolAnterieursAutrePrecision') AS `travaux_sol_anterieurs_autre`,
	`frequence_travail_sol`.`title` AS `frequence_travail_sol`,
	json_value(`releves`.`data`,'$.espaceVert.frequenceTravailSolAutrePrecision') AS `frequence_travail_sol_autre`,
	json_value(`releves`.`data`,'$.espaceVert.frequenceTravailSolRegulierPrecision') AS `frequence_travail_sol_regulier`,
	json_unquote(json_extract(`releves`.`data`,'$.espaceVert.amendements')) AS `amendements`,
	json_value(`releves`.`data`,'$.espaceVert.amendementsAutrePrecision') AS `amendements_autre`,
	`frequence_amendement`.`title` AS `frequence_amendement`,
	json_value(`releves`.`data`,'$.espaceVert.frequenceAmendementAutrePrecision') AS `frequence_amendement_autre`,
	json_value(`releves`.`data`,'$.espaceVert.frequenceAmendementRegulierPrecision') AS `frequence_amendement_regulier`,
	json_value(`releves`.`data`,'$.transect.nom') AS `transect_nom`,
	json_extract(`releves`.`data`,'$.transect.coordonnees') AS `transect_coordonnees`,
	cast(replace(replace(replace(json_unquote(json_value(`releves`.`data`,'$.transect.description')),'\\r',' '),'\\n',' '),'\\t',' ') as char charset utf8mb3) AS `transect_description`,
	`transect_habitat`.`title` AS `transect_habitat`,
	`transect_type_prairie`.`title` AS `transect_type_prairie`,
	`transect_paturage_prairie`.`title` AS `transect_paturage_prairie`,
	`transect_rythme_fauchage_prairie`.`title` AS `transect_rythme_fauchage_prairie`,
	`transect_paturage_friche`.`title` AS `transect_paturage_friche`,
	`transect_rythme_fauchage_friche`.`title` AS `transect_rythme_fauchage_friche`,
	`transect_rythme_arbustes_friche`.`title` AS `transect_rythme_arbustes_friche`,
	`transect_type_arbres_square`.`title` AS `transect_type_arbres_square`,
	`transect_type_plantes_square`.`title` AS `transect_type_plantes_square`,
	`transect_frequence_tontes`.`title` AS `transect_frequence_tontes`,
	`transect_type_arbres_gazon`.`title` AS `transect_type_arbres_gazon`,
	`transect_type_plantes_gazon`.`title` AS `transect_type_plantes_gazon`,
	`transect_type_plantations`.`title` AS `transect_type_plantations`,
	`transect_plantes_ornementales`.`title` AS `transect_plantes_ornementales`,
	`transect_plantes_aromatiques`.`title` AS `transect_plantes_aromatiques`,
	`transect_type_allees`.`title` AS `transect_type_allees`,
	`transect_type_arbres_cimetiere`.`title` AS `transect_type_arbres_cimetiere`,
	`transect_arbustes_infrastructures`.`title` AS `transect_arbustes_infrastructures`,
	`transect_rythme_fauchage_infrastructures`.`title` AS `transect_rythme_fauchage_infrastructures`,
	`transect_environnement`.`title` AS `transect_environnement`,
	`transect_type_arbres_lisiere`.`title` AS `transect_type_arbres_lisiere`,
	json_value(`releves`.`data`,'$.date') AS `releve_date`,
	json_value(`releves`.`data`,'$.heureDebut') AS `heure_debut`,
	json_value(`releves`.`data`,'$.heureFin') AS `heure_fin`,
	json_unquote(json_extract(`releves`.`data`,'$.observateurs')) AS `observateurs`,
	`temperature`.`title` AS `temperature`,
	`vent`.`title` AS `vent`,
	`couverture_nuageuse`.`title` AS `couverture_nuageuse`,
	`hauteur_vegetation`.`title` AS `hauteur_vegetation`,
	json_unquote(json_extract(`releves`.`data`,'$.milieux')) AS `milieux`,
	json_value(`releves`.`data`,'$.milieuxAutrePrecision') AS `milieux_autre`,
	`semis_sursemis`.`title` AS `semis_sursemis`,
	`fauche`.`title` AS `fauche`,
	json_unquote(json_extract(`releves`.`data`,'$.periodesFauches')) AS `periodes_fauches`,
	`frequence_fauches`.`title` AS `frequence_fauches`,
	json_unquote(json_extract(`releves`.`data`,'$.paturages')) AS `paturages`,
	json_value(`releves`.`data`,'$.paturagesAutrePrecision') AS `paturages_autre`,
	json_value(`releves`.`data`,'$.pressionPaturage') AS `pression_paturage`,
	json_value(`releves`.`data`,'$.dureeAnnuellePaturage') AS `duree_annuelle_paturage`,
	`exportation_residus`.`title` AS `exportation_residus`,
	json_unquote(json_extract(`releves`.`data`,'$.traitementsPhytosanitaires')) AS `traitements_phyto`,
	json_value(`releves`.`data`,'$.traitementsPhytosanitairesAutrePrecision') AS `traitements_phyto_autre`,
	json_unquote(json_extract(`releves`.`data`,'$.pressions')) AS `pressions`,
	json_value(`releves`.`data`,'$.pressionsAutrePrecision') AS `pressions_autre`,
	cast(replace(replace(replace(json_unquote(json_value(`releves`.`data`,'$.commentaire')),'\\r',' '),'\\n',' '),'\\t',' ') as char charset utf8mb3) AS `commentaire`,
	`taxon`.`title` AS `taxon`,
	if(`taxon`.`title` = 'Papillon indéterminé','Papillon indéterminé', json_value(`taxon_latin`.`data`,'$.nomScientifique')) AS `taxon_nom_scientifique`,
	json_value(`espaces_verts`.`observations`.`data`,'$.cdNomTaxref') AS `taxon_cdNom`,
	json_value(`espaces_verts`.`observations`.`data`,'$.abondance') AS `taxon_abondance`
	from (((((((((((((((((((((((((((((((((((((((`espaces_verts`.`participations` `releves` 
		left join `espaces_verts`.`observation_areas` `sites` on(`sites`.`id` = `releves`.`observation_area_id`)) 
		left join `espaces_verts`.`structures` on(`espaces_verts`.`structures`.`id` = `sites`.`structure_id`)) 
		left join `espaces_verts`.`protocols` on(`espaces_verts`.`protocols`.`id` = `releves`.`protocol_id`)) 
		left join `espaces_verts`.`users` on(`espaces_verts`.`users`.`id` = `releves`.`user_id`)) 
		left join `espaces_verts`.`observations` on(`espaces_verts`.`observations`.`participation_id` = `releves`.`id`))
		left join `espaces_verts`.`thesaurus_values` `frequentation` on(`frequentation`.`value` = json_value(`releves`.`data`,'$.espaceVert.frequentation'))) 
		left join `espaces_verts`.`thesaurus_values` `frequence_travail_sol` on(`frequence_travail_sol`.`value` = json_value(`releves`.`data`,'$.espaceVert.frequenceTravailSol'))) 
		left join `espaces_verts`.`thesaurus_values` `frequence_amendement` on(`frequence_amendement`.`value` = json_value(`releves`.`data`,'$.espaceVert.frequenceAmendement'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_habitat` on(`transect_habitat`.`value` = json_value(`releves`.`data`,'$.transect.habitat'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_type_prairie` on(`transect_type_prairie`.`value` = json_value(`releves`.`data`,'$.transect.typePrairie'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_paturage_prairie` on(`transect_paturage_prairie`.`value` = json_value(`releves`.`data`,'$.transect.paturagePrairie'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_rythme_fauchage_prairie` on(`transect_rythme_fauchage_prairie`.`value` = json_value(`releves`.`data`,'$.transect.rythmeFauchagePrairie'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_paturage_friche` on(`transect_paturage_friche`.`value` = json_value(`releves`.`data`,'$.transect.paturageFriche'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_rythme_fauchage_friche` on(`transect_rythme_fauchage_friche`.`value` = json_value(`releves`.`data`,'$.transect.rythmeFauchageFriche'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_rythme_arbustes_friche` on(`transect_rythme_arbustes_friche`.`value` = json_value(`releves`.`data`,'$.transect.arbustesFriche'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_type_arbres_square` on(`transect_type_arbres_square`.`value` = json_value(`releves`.`data`,'$.transect.typeArbresSquare'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_type_plantes_square` on(`transect_type_plantes_square`.`value` = json_value(`releves`.`data`,'$.transect.typePlantesSquare'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_frequence_tontes` on(`transect_frequence_tontes`.`value` = json_value(`releves`.`data`,'$.transect.frequenceTontes'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_type_arbres_gazon` on(`transect_type_arbres_gazon`.`value` = json_value(`releves`.`data`,'$.transect.typeArbresGazon'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_type_plantes_gazon` on(`transect_type_plantes_gazon`.`value` = json_value(`releves`.`data`,'$.transect.typePlantesGazon'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_type_plantations` on(`transect_type_plantations`.`value` = json_value(`releves`.`data`,'$.transect.typePlantations'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_plantes_ornementales` on(`transect_plantes_ornementales`.`value` = json_value(`releves`.`data`,'$.transect.plantesOrnementales'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_plantes_aromatiques` on(`transect_plantes_aromatiques`.`value` = json_value(`releves`.`data`,'$.transect.plantesAromatiques'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_type_allees` on(`transect_type_allees`.`value` = json_value(`releves`.`data`,'$.transect.typeAllees'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_type_arbres_cimetiere` on(`transect_type_arbres_cimetiere`.`value` = json_value(`releves`.`data`,'$.transect.typeArbresCimetiere'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_arbustes_infrastructures` on(`transect_arbustes_infrastructures`.`value` = json_value(`releves`.`data`,'$.transect.arbustesInfrastructures'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_rythme_fauchage_infrastructures` on(`transect_rythme_fauchage_infrastructures`.`value` = json_value(`releves`.`data`,'$.transect.rythmeFauchageInfrastructures'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_environnement` on(`transect_environnement`.`value` = json_value(`releves`.`data`,'$.transect.environnement'))) 
		left join `espaces_verts`.`thesaurus_values` `transect_type_arbres_lisiere` on(`transect_type_arbres_lisiere`.`value` = json_value(`releves`.`data`,'$.transect.typeArbresLisiere'))) 
		left join `espaces_verts`.`thesaurus_values` `temperature` on(`temperature`.`value` = json_value(`releves`.`data`,'$.temperature'))) 
		left join `espaces_verts`.`thesaurus_values` `vent` on(`vent`.`value` = json_value(`releves`.`data`,'$.vent'))) 
		left join `espaces_verts`.`thesaurus_values` `couverture_nuageuse` on(`couverture_nuageuse`.`value` = json_value(`releves`.`data`,'$.couvertureNuageuse'))) 
		left join `espaces_verts`.`thesaurus_values` `hauteur_vegetation` on(`hauteur_vegetation`.`value` = json_value(`releves`.`data`,'$.hauteurVegetation'))) 
		left join `espaces_verts`.`thesaurus_values` `semis_sursemis` on(`semis_sursemis`.`value` = json_value(`releves`.`data`,'$.semisSursemis'))) 
		left join `espaces_verts`.`thesaurus_values` `fauche` on(`fauche`.`value` = json_value(`releves`.`data`,'$.fauche'))) 
		left join `espaces_verts`.`thesaurus_values` `frequence_fauches` on(`frequence_fauches`.`value` = json_value(`releves`.`data`,'$.frequenceFauches'))) 
		left join `espaces_verts`.`thesaurus_values` `exportation_residus` on(`exportation_residus`.`value` = json_value(`releves`.`data`,'$.exportationResidus'))) 
		left join `espaces_verts`.`thesaurus_values` `taxon` on(`taxon`.`value` = json_value(`espaces_verts`.`observations`.`data`,'$.taxon'))) 
		left join `espaces_verts`.`thesaurus_values` `taxon_latin` on(`taxon_latin`.`value` = json_value(`espaces_verts`.`observations`.`data`,'$.taxon')))
	where `releves`.`protocol_id` = 2 and `sites`.`deleted_at` is null and `releves`.`deleted_at` is null and `espaces_verts`.`observations`.`deleted_at` is null