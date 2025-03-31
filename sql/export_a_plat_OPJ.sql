SELECT
  po.id as observation_id,
	pp.id as session_id,
	pp.startDate as session_date,
	year(pp.startDate) as session_year,
	t.value as session_week,
	soa.userId as user_id,
	soay.observationAreaId as jardin_id,
	ST_AsText(soa.geopoint) as geometry,
	st_x(soa.geopoint) as longitude,
	st_y(soa.geopoint) as latitude,
	soa.deptCode as dept_code,
	soa.postalCode as session_zip_code,
	t2.value as taxon,
	po.nbTaxons as taxon_count,
	t3.value as freq_passage,
	t4.value as type_environnement,
	t5.value as jardin_surface,
	t6.value as distance_bois,
	t7.value as distance_champs,
	t8.value as distance_prairie,
	t9.value as type_Engrais,
	t10.value as type_Insecticide,
	t11.value as type_Herbicide,
	t12.value as type_Fongicide,
	t13.value as type_AntiLimace,
	t14.value as type_BouillieBordelaise,
	t15.value as frequence_Engrais,
	t16.value as frequence_Insecticide,
	t17.value as frequence_Herbicide,
	t18.value as frequence_Fongicide,
	t19.value as frequence_AntiLimace,
	t20.value as frequence_BouillieBordelaise
FROM
	spgp.pj_observation po
LEFT JOIN spgp.pj_participation pp on
	pp.id = po.participationId
LEFT JOIN spgp.spj_observation_area_year soay on
	pp.observationAreaYearId = soay.id
LEFT JOIN spgp.spj_observation_area soa on
	soay.observationAreaId = soa.id
LEFT JOIN spgp.users usr on
  soa.userId = usr.id
LEFT JOIN thesaurus t on
	pp.weekId = t.id
LEFT JOIN thesaurus t2 on
	po.taxonId = t2.id
LEFT JOIN thesaurus t3 on
	pp.frequencePassageId = t3.id
LEFT JOIN thesaurus t4 on
	soay.environmentId = t4.id
LEFT JOIN thesaurus t5 on
	soay.surfaceId = t5.id
LEFT JOIN thesaurus t6 on
	soay.distanceBoisId = t6.id
LEFT JOIN thesaurus t7 on
	soay.distanceChampId = t7.id
LEFT JOIN thesaurus t8 on
	soay.distancePrairieId = t8.id
LEFT JOIN thesaurus t9 on
	soay.typeEngraisId = t9.id
LEFT JOIN thesaurus t10 on
	soay.typeInsecticideId = t10.id
LEFT JOIN thesaurus t11 on
	soay.typeHerbicideId = t11.id
LEFT JOIN thesaurus t12 on
	soay.typeFongicideId = t12.id
LEFT JOIN thesaurus t13 on
	soay.typeAntiLimaceId = t13.id
LEFT JOIN thesaurus t14 on
	soay.typeBouillieBordelaiseId = t14.id
LEFT JOIN thesaurus t15 on
	soay.frequenceEngraisId = t15.id
LEFT JOIN thesaurus t16 on
	soay.frequenceInsecticideId = t16.id
LEFT JOIN thesaurus t17 on
	soay.frequenceHerbicideId = t17.id
LEFT JOIN thesaurus t18 on
	soay.frequenceFongicideId = t18.id
LEFT JOIN thesaurus t19 on
	soay.frequenceAntiLimaceId = t19.id
LEFT JOIN thesaurus t20 on
	soay.frequenceBouillieBordelaiseId = t20.id