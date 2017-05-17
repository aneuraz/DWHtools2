#library(stringr)
#config <- getConfig("../../dwh_config")

#' username_from_login
#' @export
username_from_login <- function(login = NULL, config = NULL) {

  if(is.null(login)) {
    stop("a valid login is required")
  } else if (login == '') {
    stop("a valid login is required")
  }

  if (is.null(config)) {
    stop("a valid config object is required")
  }


  res <- oracleQuery(stringr::str_interp("SELECT NUM_USER
    			    FROM DWH_USER
              WHERE LOGIN = to_char('${login}')"), config)

  res$NUM_USER[1]
}

#username <- username_from_login("3252446", config)


#' get_cohorts_list
#' @export
get_cohorts_list <- function(username = NULL , only_num = FALSE, config = NULL) {

  if(is.null(username)) {
    stop("a valid username is required")
  } else if (username == '') {
    stop("a valid username is required")
  }

  if (is.null(config)) {
    stop("a valid config object is required")
  }

  if (!only_num) {
    columns = "c.result_instance_id as num_cohorte, m.name as titre_cohorte, r.real_set_size as n_patients"
  } else {
    columns = "c.result_instance_id as num_cohorte"
  }


  sql = stringr::str_interp("select distinct ${columns}
                    FROM
  i2b2demodata.qt_query_master m,
  i2b2demodata.qt_query_instance i,
  i2b2demodata.qt_query_result_instance r,
  i2b2demodata.qt_patient_set_collection c

  WHERE
  m.user_id = '${username}' AND
  m.query_master_id = i.query_master_id AND
  i.query_instance_id = r.query_instance_id AND
  r.result_instance_id = c.result_instance_id")


 res <- oracleQuery(sql, config)


  return(res)
}

#cohorts <- get_cohorts_list(username, only_num = FALSE, config)

#' get_num_temp_list
#' @export
get_num_temp_list <- function(username = NULL, only_num = FALSE, config = NULL) {

  if(is.null(username)) {
    stop("a valid username is required")
  } else if (username == '') {
    stop("a valid username is required")
  }

  if (is.null(config)) {
    stop("a valid config object is required")
  }

  if (!only_num) {
    columns = "num_temp, count(DISTINCT patient_num) as n_patient"
  } else {
    columns = "num_temp"
  }

  sql = stringr::str_interp("SELECT ${columns}
                            FROM dwh_resultat
                            WHERE num_user_resultat = ${username}
                            GROUP BY num_user_resultat, num_temp")

  res <- oracleQuery(sql, config)

  return(res)
}

#get_num_temp_list("13622243453")

#' patients_subquery
#' @export
patients_subquery <- function(num_type = "cohorte") {

  if(!(num_type %in% c('num_temp', 'cohorte'))) {
    stop('num_type must be either "num_type" or "cohorte"')
  }

  if (num_type == 'num_temp') {

    return("SELECT distinct PATIENT_NUM from i2b2demodata.qt_patient_set_collection
    where result_instance_id = ")

  } else if (num_type == 'cohorte') {

    return ("SELECT distinct patient_num
    from dwh_cohorte_resultat
    where statut = 1 and num_cohorte = ")
  } else {

    return(NULL)
  }
}

#patients_subquery("num_temp")
#patients_subquery("cohorte")

#' get_patients
#' @export
get_patients <- function(num = NULL, num_type = "cohorte", only_num = FALSE, count = FALSE, config = NULL) {

  if (is.null(num) | is.null(num_type)) {
    stop('valid num and num_type are required')
  }


  if (is.null(config)) {
    stop("a valid config object is required")
  }

  count_query <- ""
  test_count <- ""

  if (!only_num) {
    columns <- "p.PATIENT_NUM, birth_date as DATENAIS, SEX_CD as SEXE, zip_cd as CP, null as PAYS, null as PAYS_NAISSANCE, vital_status_cd as CODE_DECES, TO_CHAR(birth_date, 'YYYY') as ANNEE_NAIS"
  } else {
    columns <- "p.PATIENT_NUM"
  }

  subquery <- patients_subquery(num_type)

  if (count) {
    columns <- paste(columns, ", c.COUNT_UNIQUE")
    count_query <- "LEFT JOIN NEU_CONCEPTS_COUNTS c
    on p.PATIENT_NUM = c.PATIENT_NUM
    ";
    test_count <- "and c.COUNT_UNIQUE is not null";
  }

  sql = stringr::str_interp("SELECT ${columns}
  FROM i2b2demodata.PATIENT_DIMENSION p
  ${count_query}
  where p.PATIENT_NUM in ( ${subquery} ${num})
  ${test_count}")


  res <- oracleQuery(sql, config)

  return(res)

}

# patients <- get_patients('37',  config = config)
# patients <- get_patients('37', only_num = FALSE, count = TRUE, config = config)

#' get_concepts
#' @export
get_concepts <- function(num = NULL, num_type = NULL, config = NULL) {

  if (is.null(num) | is.null(num_type)) {
    stop('valid num and num_type are required')
  }

  if (is.null(config)) {
    stop("a valid config object is required")
  }


  subquery <- patients_subquery(num_type)

  sql <- stringr::str_interp("SELECT PATIENT_NUM, IEP, DATE_DOCUMENT, ORIGINE_DOC, AGE_PATIENT, CERTITUDE, CONTEXTE, e.CODE_THESAURUS,
  e.CODE, e.TFIDF_CODE_DOCUMENT, e.NB_CODE,
  t.NIVEAU, t.CODE_LIBELLE , t.NB_PATIENT, t.GENOTYPE, t.PHENOTYPE, t.CHEMIN_LIBELLE
  FROM DWH_ENRSEM e
  LEFT JOIN DWH_THESAURUS_ENRSEM t
  on e.CODE = t.CODE
  where PREF = 'Y'
  and CONTEXTE = 'texte_patient'
  and patient_num  in ( ${subquery} ${num})")

  res <- oracleQuery(sql, config)

  return(res)

}

# concepts <- get_concepts('13624005402', 'cohorte', config = config)

#' get_data
#' @export
get_data <- function(num = NULL, num_type = "cohorte", data_type = NULL, config = NULL) {

  if (is.null(num) | is.null(num_type)) {
    stop('valid num and num_type are required')
  }

  if (is.null(config)) {
    stop("a valid config object is required")
  }

  if (!(data_type %in% c('bio_num' , 'cim10'))) {
    stop('data_type must be either "bio_num" or "cim10"')
  }

  subquery <- patients_subquery(num_type)

  if (data_type == 'bio_num') {

    sql <- stringr::str_interp("SELECT O.PATIENT_NUM,
    O.ENCOUNTER_NUM as IEP,
    O.CONCEPT_CD as CODE,
    C.NAME_CHAR as CODE_LIBELLE,
    O.NVAL_NUM as VAL_NUMERIC,
    O.START_DATE as DATE_DOCUMENT,
    null as BORNE_INF,
    null as BORNE_SUP,
    null as ID_THESAURUS_DATA,
    C.NAME_CHAR as LIBELLE_PARENT,
    O.CONCEPT_CD as CODE_PARENT,
    VALUEFLAG_CD,
    (CASE WHEN VALUEFLAG_CD = 'L' THEN 1 ELSE 0 END) as inf,
    (CASE WHEN VALUEFLAG_CD = 'H' THEN 1 ELSE 0 END) as sup
    FROM i2b2demodata.OBSERVATION_FACT O, i2b2demodata.CONCEPT_DIMENSION C
    WHERE C.CONCEPT_CD LIKE 'LOINC%' AND
    C.CONCEPT_CD = O.CONCEPT_CD AND
    O.NVAL_NUM is not null AND
    O.VALUEFLAG_CD IN ('L', 'H', 'A') AND
    O.PATIENT_NUM in (${subquery} ${num} )
    ")

  } else if (data_type == 'cim10' ) {

    sql <- stringr::str_interp("SELECT O.PATIENT_NUM,
    O.ENCOUNTER_NUM as IEP,
    O.CONCEPT_CD as CODE,
    C.NAME_CHAR as CODE_LIBELLE,
    O.NVAL_NUM as VAL_NUMERIC,
    O.START_DATE as DATE_DOCUMENT,
    null as ID_THESAURUS_DATA,
    C.NAME_CHAR as LIBELLE_PARENT,
    O.CONCEPT_CD as CODE_PARENT
    FROM i2b2demodata.OBSERVATION_FACT O, i2b2demodata.CONCEPT_DIMENSION C
    WHERE C.CONCEPT_CD LIKE 'ICD%' AND
    C.CONCEPT_CD = O.CONCEPT_CD AND
    O.PATIENT_NUM in (${subquery} ${num} )
    ")
  }

  res <- oracleQuery(sql, config)

  return(res)
}

 # cim <- get_data('37', 'num_temp', 'cim10', config = config)
 # bio <- get_data('37', data_type='bio_num', config = config)

#' match_patient
#' @export
match_patient <- function(num = NULL, num_type = "cohorte", sexe = NULL, annee_nais = NULL, annee_range = NULL, count_unique = NULL, count_range = NULL, n_match = NULL, config = NULL) {

  subquery <- patients_subquery(num_type)


  sql <- stringr::str_interp(  "SELECT r.PATIENT_NUM FROM
  (SELECT C.PATIENT_NUM FROM NEU_CONCEPTS_COUNTS C
    LEFT JOIN i2b2demodata.PATIENT_DIMENSION p
    ON c.PATIENT_NUM = p.PATIENT_NUM
    WHERE p.PATIENT_NUM not in  ( ${subquery} ${num} ) AND
    p.SEX_CD = '${sexe}'
    and TO_NUMBER(TO_CHAR(p.BIRTH_DATE, 'YYYY'), '9999') between (${annee_nais} - ${annee_range}) and (${annee_nais} + ${annee_range})
    and c.COUNT_UNIQUE between (${count_unique} - ${count_unique}*${count_range}) and (${count_unique} + ${count_unique}*${count_range})
    ORDER BY c.PATIENT_NUM) r")

  res <- oracleQuery(sql, config)


  if (nrow(res)> n_match) {
    res <- res[sample(1:nrow(res), n_match),]
  } else {
    res <- res[sample(1:nrow(res), nrow(res)),]
  }

  return(res)

}

# set.seed(456)
# match_patient('37',
#               'num_temp',
#               sexe = patients$SEXE[1],
#               annee_nais = patients$ANNEE_NAIS[1],
#               annee_range = 40,
#               count_unique = 20,
#               count_range = 2 ,
#               n_match = 5,
#               config = config)
#

#' match_patients_from_num
#' @export
match_patients_from_num <- function(num = NULL, num_type = "cohorte", annee_range = NULL, count_range = NULL, n_match = NULL,match_save= FALSE, match_save_title = NULL, config = NULL) {



  patients = get_patients(num, num_type, only_num = FALSE, count = TRUE, config);

  # cl<-parallel::makeCluster(parallel::detectCores())

  # parallel::clusterEvalQ(cl, {library(dplyr); library(stringr); library(MASS); library(tidyr);library(broom);library(DWHtools);library(boot)})
  # parallel::clusterExport(cl, "match_patient")

  res_temp <- lapply(1:nrow(patients) , FUN= function(x, patients, num, num_type, annee_range, count_range, n_match, config) {
    match_patient(num, num_type, patients[x,'SEXE'], patients[x,'ANNEE_NAIS'],annee_range, patients[x,'COUNT_UNIQUE'],  count_range, n_match, config)
  }, patients= patients, num = num, num_type = num_type, annee_range = annee_range, count_range = count_range, n_match = n_match, config = config)
  #parallel::stopCluster(cl)

  result <- as.vector(unlist(res_temp))

  # data <- apply(patients, 1, function(p) {
  #   match_patient(num, num_type, p['SEXE'], p['ANNEE_NAIS'],annee_range, p['COUNT_UNIQUE'],  count_range, n_match, config)
  # })
  #
  # data <- as.vector(data)

  num_temp <- insert_patients_into_dwh_resultat(result, config)

  if(match_save) {
    num_save <- create_cohorte(match_save_title,
                   stringr::str_interp("cohort created by multimodal PheWAS during the analysis of the cohort ${num}"),
                   config$username, config)

    add_privilege_to_cohort(num_save, config$username, 'voir_stats', config)

    add_patients_to_cohort(num_save, result, config$username, config)

  }

  return (num_temp)

}


# controls <- match_patients_from_num(num = '37', num_type= 'num_temp',annee_range = 20,count_range = 2,n_match = 5, config = config)

#' insert_patients_into_dwh_resultat
#' @export
insert_patients_into_dwh_resultat <- function(patients,  config) {

  sql = "SELECT nextval('i2b2demodata.qt_query_master_query_master_id_seq'::regclass)"
  res <- oracleQuery(sql, config)
  master_id <- res$NEXTVAL[1]

  sql = stringr::str_interp("INSERT INTO i2b2demodata.qt_query_master (query_master_id,name,user_id, group_id, create_date)
  VALUES (${master_id},'Control TEST','demo','Demo',current_date)")
  oracleQuery(sql, config, update = T, data = F)


  sql = "SELECT nextval('i2b2demodata.qt_query_instance_query_instance_id_seq'::regclass)"
  res <- oracleQuery(sql, config)
  instance_id <- res$NEXTVAL[1]

sql = stringr::str_interp("INSERT INTO i2b2demodata.qt_query_instance (query_instance_id,query_master_id,user_id, group_id, start_date)
  VALUES (${instance_id},${master_id},'${config$username}','Demo',current_date)")
  oracleQuery(sql, config, update = T, data = F)

  sql = "SELECT nextval('i2b2demodata.qt_query_result_instance_result_instance_id_seq'::regclass)"
  res <- oracleQuery(sql, config)
  result_id <- res$NEXTVAL[1]

  sql = stringr::str_interp("INSERT INTO i2b2demodata.qt_query_result_instance (result_instance_id,query_instance_id,result_type_id,start_date,status_type_id)
  VALUES (${result_id},${instance_id},1,current_date,3)")
  oracleQuery(sql, config, update = T, data = F)

  dt <- data.frame(RESULT_INSTANCE_ID = result_id,
             PATIENT_NUM = patients)

  values <- apply(dt, 1 , paste, collapse = "','")
  values <- paste0("('",values, "')")
  values <- paste0(values, collapse = ',')
  # oracle
  #values <- paste0("INTO qt_patient_set_collection (result_instance_id, PATIENT_NUM) VALUES ", values)
  # values <- paste(values, collapse = "\n")

  sql <- stringr::str_interp("INSERT INTO i2b2demodata.qt_patient_set_collection (result_instance_id, PATIENT_NUM) VALUES
                             ${values}")

   print(sql)
   oracleQuery(sql, config, update = T, data = F)
   return(num_temp)

}

# num_control <- insert_patients_into_dwh_resultat(controls, config)

#' create_cohorte
#' @export
create_cohorte <- function(titre_cohorte, description_cohorte,username, config) {
  sql = "select dwh_seq.nextval num_cohorte from dual";
  res <- oracleQuery(sql, config)
  num_cohorte <- res$NUM_COHORTE[1]

  sql = stringr::str_interp("insert into dwh_cohorte  (num_cohorte , titre_cohorte ,description_cohorte ,date_cohorte,num_user_creation,num_datamart )
                            values (${num_cohorte},'${titre_cohorte}','${description_cohorte}',sysdate,${username},0)")

  oracleQuery(sql, config, update = F, data = F)

  return(num_cohorte)

}

#create_cohorte("cohorte_test", "cohorte de test", config$username, config )

#' add_privilege_to_cohort
#' @export
add_privilege_to_cohort <- function(num_cohorte, username, privilege, config)
{

  # privilege: c('voir_stat')
  sql = stringr::str_interp("insert into dwh_cohorte_user_droit  (num_cohorte , num_user_cohorte ,droit) values (${num_cohorte},${username},'${privilege}')")
  oracleQuery(sql, config, update = F, data = F)

  return(0)

}

#add_privilege_to_cohort(13672749875, config$username, 'voir_stat', config)

#' add_patients_to_cohort
#' @export
add_patients_to_cohort <- function(num_cohorte, patient_nums, username, config) {

  dt <- data.frame(NUM_COHORTE = num_cohorte,
                   PATIENT_NUM = patient_nums,
                   STATUT = 1,
                   DATE_AJOUT = 'sysdate',
                   USER_AJOUT = username)

  values <- apply(dt, 1 , paste, collapse = "','")
  values <- paste0("('",values, "')")
  values <- gsub("'sysdate'","sysdate",values)
  values <- paste0("into dwh_cohorte_resultat  (num_cohorte , patient_num ,statut,date_ajout,user_ajout) VALUES ", values)
  values <- paste(values, collapse = "\n")

  sql <- stringr::str_interp("INSERT ALL
                             ${values}
                             SELECT 1 FROM DUAL")

  oracleQuery(sql, config, update = T, data = F)

  return(0)
}

#add_patients_to_cohort(num_cohorte, patient_nums, config$username, config)

