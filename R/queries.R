#' username_from_login
#'
#' For Dr.Warehouse
#' Get the internal username given a login
#'
#' @param login a string login
#' @param config a config environment created by the function getConfig.
#' @return a character vector username.
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

#' get_cohorts_list
#'
#' For Dr.Warehouse and i2b2
#' Get the list of cohorts or patient sets for a given username.
#'
#' @param username a username (string)
#' @param only_num set to `TRUE` if you want only a vector of cohort numbers. Set to `FALSE`
#'  if you also need the name of the cohort and the number of patients (default = `FALSE`)
#' @param config a config environment created by the function getConfig.
#' @return a data.frame with the list of cohorts
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

  if(config$backend == 'i2b2_oracle' | config$backend == 'i2b2_postgres') {

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

  } else if (config$backend == 'drwh_oracle') {
    if (!only_num) {
      columns = "r.num_cohorte, c.titre_cohorte, count(distinct r.patient_num) as n_patients"
    } else {
      columns = "r.num_cohorte"
    }

    sql = stringr::str_interp("select ${columns}
                              from dwh_cohorte_resultat r
                              left join dwh_cohorte c
                              on c.num_cohorte = r.num_cohorte
                              where c.num_user_creation = ${username}
                              and r.statut = 1
                              group by r.num_cohorte, c.titre_cohorte")

  }

 res <- oracleQuery(sql, config)


  return(res)
}

#' get_num_temp_list
#'
#' For Dr.Warehouse
#' Get the list of recent queries results for a given username.
#'
#' @param username a username (string)
#' @param only_num set to `TRUE` if you want only a vector of num_temp numbers. Set to `FALSE`
#'  if you also need the number of patients (default = `FALSE`)
#' @param config a config environment created by the function getConfig.
#' @return a data.frame with the list of num_temps
#' @export
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

#' patients_subquery
#'
#' For i2b2 and Dr.Warehouse
#' Get the patients SQL subquery corresponding to the type cohort/num_temp and the backend Dr.Warehouse/i2b2
#'
#' @param num_type one of c('num_temp', 'cohorte')
#' @param backend one of c('i2b2_oracle','i2b2_postgres', 'drwh_oracle' )
#' @return SQL subquery
#' @export
patients_subquery <- function(num_type, backend) {

  if(!(num_type %in% c('num_temp', 'cohorte'))) {
    stop('num_type must be either "num_type" or "cohorte"')
  }

  if (num_type == 'num_temp') {

    if (backend == 'drwh_oracle') {
      return("SELECT distinct PATIENT_NUM from DWH_RESULTAT
    where num_temp = ")
    }

  } else if (num_type == 'cohorte') {

    if (backend == 'drwh_oracle') {
      return("SELECT distinct patient_num
    from dwh_cohorte_resultat
    where statut = 1 and num_cohorte = ")
    } else if (backend == 'i2b2_oracle' | backend == 'i2b2_postgres'){
      return("SELECT distinct PATIENT_NUM from i2b2demodata.qt_patient_set_collection
    where result_instance_id = ")

    }

  } else {

    return(NULL)
  }
}

#' get_patients
#'
#' For i2b2 and Dr.Warehouse
#' Get the demographic informations of the patients in a cohort/num_temp.
#'
#' @param num the identifier of the cohort/num_temp
#' @param num_type type of num: one of c('num_temp', 'cohorte')
#' @param only_num set to `TRUE` if you only need the patient_nums (default = `FALSE`)
#' @param count set to `TRUE` if you want to return the count of unique concepts for the patients
#'  contained in a precomputed table named 'NEU_CONCEPT_COUNT'
#' @param config
#' @export
get_patients <- function(num = NULL, num_type = NULL, only_num = FALSE, count = FALSE, config = NULL) {

  if (is.null(num) | is.null(num_type)) {
    stop('valid num and num_type are required')
  }


  if (is.null(config)) {
    stop("a valid config object is required")
  }

  count_query <- ""
  test_count <- ""

  if (config$backend == 'i2b2_oracle' | config$backend == 'i2b2_postgres') {
    if (!only_num) {
      columns <- "p.PATIENT_NUM, birth_date , SEX_CD as SEX,  vital_status_cd as  vital_status, TO_CHAR(birth_date, 'YYYY') as birth_year"
    } else {
      columns <- "p.PATIENT_NUM"
    }

    subquery <- patients_subquery(num_type, config$backend)

    if (count) {
      columns <- paste(columns, ", c.COUNT_UNIQUE as UNIQ_CONCEPTS")
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

  } else if (config$backend == 'drwh_oracle') {

    if (!only_num) {
      columns <- "p.PATIENT_NUM, p.DATENAIS as birth_date, p.SEXE as SEX, p.CODE_DECES as vital_status, TO_CHAR(p.DATENAIS, 'YYYY') as birth_year"
    } else {
      columns <- "p.PATIENT_NUM"
    }

    subquery <- patients_subquery(num_type, config$backend)

    if (count) {
      columns <- paste(columns, ", c.COUNT_UNIQUE as UNIQ_CONCEPTS")
      count_query <- "LEFT JOIN NEU_CONCEPTS_COUNTS c
      on p.PATIENT_NUM = c.PATIENT_NUM
      ";
      test_count <- "and c.COUNT_UNIQUE is not null";
    }

    sql = stringr::str_interp("SELECT ${columns}
                              FROM DWH_PATIENT p
                              ${count_query}
                              where p.PATIENT_NUM in ( ${subquery} ${num})
                              ${test_count}")

  }

  res <- oracleQuery(sql, config)

  return(res)

}

#' get_concepts
#' For Dr.Warehouse
#'
#' Get the concepts (e.g. UMLS concepts extracted from free text reports) for the patients in a cohort/num_temp.
#'
#' @param num the identifier of the cohort/num_temp
#' @param num_type type of num: one of c('num_temp', 'cohorte')
#' @param config a config environment created by the function getConfig.
#' @return a data.frame
#' @export
get_concepts <- function(num = NULL, num_type = NULL, config = NULL) {

  if (is.null(num) | is.null(num_type)) {
    stop('valid num and num_type are required')
  }

  if (is.null(config)) {
    stop("a valid config object is required")
  }

  if (config$backend == "i2b2_oracle" | config$backend == "i2b2_postgres") {
    return(NULL)
  }

  subquery <- patients_subquery(num_type, config$backend)

  sql <- stringr::str_interp("SELECT e.PATIENT_NUM,
e.IEP as ENCOUNTER_NUM,
e.DATE_DOCUMENT as DOCUMENT_DATE,
e.ORIGINE_DOC as DOCUMENT_ORIGIN,
e.CERTITUDE as CONCEPT_CERTITUDE,
e.CONTEXTE as CONCEPT_CONTEXT,
  e.CODE, thes.CODE_LIBELLE as CODE_LABEL_FR,
thes.GENOTYPE, thes.PHENOTYPE,
thes.CODE_LIBELLE_EN as CODE_LABEL,
thes.CODE_PERE as PARENT_CODE, thes.CODE_LIBELLE_PERE as PARENT_LABEL_FR,
thes.CODE_LIBELLE_PERE_EN as PARENT_LABEL
                             FROM DWH_ENRSEM e
                             LEFT JOIN (SELECT  t.NIVEAU, t.CODE_LIBELLE , t.NB_PATIENT, t.GENOTYPE, t.PHENOTYPE, t.CHEMIN_LIBELLE, t.CODE, t.CODE_LIBELLE_EN,
                             p.code_pere,
                             tt.code_libelle AS code_libelle_pere, tt.code_libelle_en AS code_libelle_pere_en
                             FROM dwh_thesaurus_enrsem t
                             LEFT JOIN (SELECT min(g.CODE_PERE) CODE_PERE, g.CODE_FILS
                             FROM DWH_THESAURUS_ENRSEM_GRAPH g
                             WHERE g.DISTANCE = 1
                             GROUP BY g.CODE_FILS) P
                             ON t.code = p.code_fils
                             LEFT JOIN (SELECT min(ttt.code_libelle) code_libelle, min(ttt.code_libelle_en) code_libelle_en, ttt.code
                             FROM dwh_thesaurus_enrsem ttt GROUP BY ttt.code) tt
                             ON tt.code = p.code_pere
                             WHERE t.PREF = 'Y') thes
                             ON e.CODE = thes.CODE
                             where e.CONTEXTE = 'texte_patient'
                             and e.patient_num  in ( ${subquery} ${num})")




  res <- oracleQuery(sql, config)

  return(res)

}


#' get_data
#'
#' For Dr.Warehouse and i2b2
#'
#' Get the data (e.g. ICD codes or laboratory test results) for the patients in a cohort/num_temp.
#'
#' @param num the identifier of the cohort/num_temp
#' @param num_type type of num: one of c('num_temp', 'cohorte')
#' @param data_type type of data: one of c('bio_num', 'cim10'). 'bio_num' for biological test results with numeric value.
#' 'cim10' for ICD10 codes
#' @param config a config environment created by the function getConfig.
#' @param ICD_prefix For i2b2. prefix of the CONCEPT_CDs for ICD codes (used to filter the CONCEPT_CDs) (default = 'CIM10')
#' @param BIO_prefix For i2b2. prefix of the CONCEPT_CDs for labtest codes (used to filter the CONCEPT_CDs) (default = 'LOINC')
#' @param use_scheme_key For i2b2. set to `TRUE` if you want to use a SCHEME_KEY column in OBSERVATION_FACT to filter the FACTS.
#' if `TRUE`, the value of ICD_prefix or BIO_prefix are used to filter the FACTS.
#' @param config a config environment created by the function getConfig.
#' @return a data.frame
#' @export
get_data <- function(num = NULL,
                     num_type = NULL,
                     data_type = NULL,
                     ICD_prefix = 'CIM10',
                     BIO_prefix = 'LOINC',
                     use_scheme_key = FALSE,
                     config = NULL) {

  if (is.null(num) | is.null(num_type)) {
    stop('valid num and num_type are required')
  }

  if (is.null(config)) {
    stop("a valid config object is required")
  }

  if (!(data_type %in% c('bio_num' , 'cim10'))) {
    stop('data_type must be either "bio_num" or "cim10"')
  }

  subquery <- patients_subquery(num_type, config$backend)
  if (use_scheme_key) {
    scheme_icd <- stringr::str_interp("O.SCHEME_KEY = '${ICD_prefix}:' AND")
    scheme_bio <- stringr::str_interp("O.SCHEME_KEY = '${BIO_prefix}:' AND")
  } else {
    scheme_icd <- ""
    scheme_bio <- ""
  }

  if (config$backend == 'i2b2_oracle' | config$backend == 'i2b2_postgres') {

    if (data_type == 'bio_num') {

      sql <- stringr::str_interp("SELECT O.PATIENT_NUM,
                                 O.ENCOUNTER_NUM,
                                 O.CONCEPT_CD as CODE,
                                 C.NAME_CHAR as CODE_LABEL,
                                 O.NVAL_NUM as VAL_NUMERIC,
                                 O.START_DATE as DOCUMENT_DATE,
                                 C.NAME_CHAR as PARENT_LABEL,
                                 O.CONCEPT_CD as PARENT_CODE,
                                 (CASE WHEN VALUEFLAG_CD = 'L' THEN 1 ELSE 0 END) as inf,
                                 (CASE WHEN VALUEFLAG_CD = 'H' THEN 1 ELSE 0 END) as sup
                                 FROM i2b2demodata.OBSERVATION_FACT O, i2b2demodata.CONCEPT_DIMENSION C
                                 WHERE C.CONCEPT_CD LIKE '${BIO_prefix}%' AND
                                 C.CONCEPT_CD = O.CONCEPT_CD AND
                                 ${scheme_icd}
                                 O.NVAL_NUM is not null AND
                                 O.VALUEFLAG_CD IN ('L', 'H', 'A') AND
                                 O.PATIENT_NUM in (${subquery} ${num} )
                                 ")

    } else if (data_type == 'cim10' ) {

      sql <- stringr::str_interp("SELECT O.PATIENT_NUM,
                                 O.ENCOUNTER_NUM,
                                 O.CONCEPT_CD as CODE,
                                 C.NAME_CHAR as CODE_LABEL,
                                 O.NVAL_NUM as VAL_NUMERIC,
                                 O.START_DATE as DOCUMENT_DATE,
                                 C.NAME_CHAR as PAREN_LABEL,
                                 O.CONCEPT_CD as PARENT_CODE
                                 FROM i2b2demodata.OBSERVATION_FACT O, i2b2demodata.CONCEPT_DIMENSION C
                                 WHERE C.CONCEPT_CD LIKE '${ICD_prefix}%' AND
                                 C.CONCEPT_CD = O.CONCEPT_CD AND
                                 ${scheme_icd}
                                 O.PATIENT_NUM in (${subquery} ${num} )
                                 ")
    }

  } else if (config$backend == 'drwh_oracle') {
    if (data_type == 'bio_num') {

      sql <- stringr::str_interp("SELECT d.PATIENT_NUM, d.IEP as ENCOUNTER_NUM, t.CODE,
t.CODE_LIBELLE as CODE_LABEL, d.VAL_NUMERIC, d.DATE_DOCUMENT as DOCUMENT_DATE,
                                  tt.code_libelle AS PARENT_LABEL,
                                 (CASE WHEN d.VAL_NUMERIC < d.BORNE_INF THEN 1 ELSE 0 END) AS inf,
                                 (CASE WHEN d.VAL_NUMERIC > d.BORNE_SUP THEN 1 ELSE 0 END) AS sup
                                 FROM DWH_DATA d
                                 LEFT JOIN DWH_THESAURUS_DATA t
                                 ON d.id_thesaurus_data = t.ID_THESAURUS_DATA
                                 LEFT JOIN DWH_THESAURUS_DATA tt
                                 ON t.id_thesaurus_parent = tt.ID_THESAURUS_DATA
                                 WHERE d.THESAURUS = 'STARE'
                                 AND d.VAL_NUMERIC IS NOT NULL
                                 AND d.BORNE_INF IS NOT NULL
                                 AND d.BORNE_SUP IS NOT NULL
                                 AND d.patient_num IN ( ${subquery} ${num} )")

    } else if (data_type == 'cim10' ) {

      sql <- stringr::str_interp("SELECT d.PATIENT_NUM, d.IEP as ENCOUNTER_NUM, t.CODE, t.CODE_LIBELLE as CODE_LABEL,
                                d.VAL_TEXTE as TEXT_VAL, d.DATE_DOCUMENT as DATE_DOCUMENT,
                                 tt.code_libelle AS PARENT_LABEL, tt.code as PARENT_CODE
                                 FROM DWH_DATA d
                                 LEFT JOIN DWH_THESAURUS_DATA t
                                 ON d.id_thesaurus_data = t.ID_THESAURUS_DATA
                                 LEFT JOIN DWH_THESAURUS_DATA tt
                                 ON t.id_thesaurus_parent = tt.ID_THESAURUS_DATA
                                 WHERE d.THESAURUS = 'cim10'
                                 AND d.patient_num in ( ${subquery} ${num} )")
    }
  }

  res <- oracleQuery(sql, config)

  return(res)
}

#' match_patient
#' For Dr.Warehouse and i2b2
#'
#' Match n (n_match) patients given the gender (sexe),
#' the birth_year (birth_year) within a range (birth_range)
#' the count of unique concepts (uniq_concepts) within a range (concepts_range)
#' excluding patients in a cohort/patient set
#'
#' @param num the identifier of the cohort/num_temp to exclude patients
#' @param num_type type of num: one of c('num_temp', 'cohorte')
#' @param sexe reference gender
#' @param birth_year reference birth_year
#' @param birth_range integer >= 0 to compute the range of authorized birth_years for matching.
#' the range will be: [birth_years - birth_range ; birth_year + birth_range]
#' @param uniq_concepts reference count of unique concepts
#' @param concepts_range. float > 0 to compute the range of authorized uniq_concepts for matching.
#' the range will be: [uniq_concepts - (uniq_concepts * concepts_range) ; uniq_concepts + (uniq_concepts * concepts_range)]
#' @param n_match: number of patients to match
#' @param config a config environment created by the function getConfig.
#' @return a vector of matched patient_nums
#' @export
match_patient <- function(num = NULL,
                          num_type = NULL,
                          sex = NULL,
                          birth_year = NULL,
                          birth_range = NULL,
                          uniq_concepts = NULL,
                          concepts_range = NULL,
                          n_match = NULL,
                          config = NULL) {

  subquery <- patients_subquery(num_type, config$backend)

  if (config$backend == 'i2b2_oracle' | config$backend == 'i2b2_postgres') {

    sql <- stringr::str_interp(  "SELECT r.PATIENT_NUM FROM
  (SELECT C.PATIENT_NUM FROM NEU_CONCEPTS_COUNTS C
                                 LEFT JOIN i2b2demodata.PATIENT_DIMENSION p
                                 ON c.PATIENT_NUM = p.PATIENT_NUM
                                 WHERE p.PATIENT_NUM not in  ( ${subquery} ${num} ) AND
                                 p.SEX_CD = '${sex}'
                                 and TO_NUMBER(TO_CHAR(p.BIRTH_DATE, 'YYYY'), '9999') between (${birth_year} - ${birth_range}) and (${birth_year} + ${birth_range})
                                 and c.COUNT_UNIQUE between (${uniq_concepts} - ${uniq_concepts}*${concepts_range}) and (${uniq_concepts} + ${uniq_concepts}*${concepts_range})
                                 ORDER BY c.PATIENT_NUM) r")

  } else if (config$backend == 'drwh_oracle') {

    sql <- stringr::str_interp("SELECT PATIENT_NUM from (
  SELECT c.PATIENT_NUM
                               from NEU_CONCEPTS_COUNTS c
                               LEFT JOIN dwh_patient p
                               on p.PATIENT_NUM = c.PATIENT_NUM
                               WHERE p.PATIENT_NUM NOT IN ( ${subquery} ${num} )
                               and p.SEXE = '${sex}'
                               and TO_NUMBER(TO_CHAR(p.DATENAIS, 'YYYY')) between (${birth_year} - ${birth_range}) and (${birth_year} + ${birth_range})
                               and c.COUNT_UNIQUE between (${uniq_concepts} - ${uniq_concepts}*${concepts_range}) and (${uniq_concepts} + ${uniq_concepts}*${concepts_range})
                               order by PATIENT_NUM) r
                               ")

  }

  res <- oracleQuery(sql, config)

  if(!is.null(res)) {
    if (nrow(res)> n_match) {
      res <- res[sample(1:nrow(res), n_match),]
    } else {
      res <- res[sample(1:nrow(res), nrow(res)),]
    }
  }


  return(res)

}

#' match_patients_from_num
#'
#' For Dr.Warehouse and i2b2
#' Match n (n_match) patients for each patient in a cohort/patient set.
#'
#' @param num the identifier of the cohort/num_temp
#' @param num_type type of num: one of c('num_temp', 'cohorte')
#' @param birth_range integer >= 0 to compute the range of authorized birth_years for matching.
#' the range will be: [birth_year - birth_range ; birth_year + birth_range]
#' @param concepts_range. float > 0 to compute the range of authorized uniq_concepts for matching.
#' the range will be: [uniq_concepts - (uniq_concepts * concepts_range) ; uniq_concepts + (uniq_concepts * concepts_range)]
#' @param n_match: number of patients to match
#' @param match_save For Dr.Warehouse (in i2b2 patient set is automatically saved). save the matched patients in a cohort (default = `FALSE`)
#' @param match_save_title title for the saved cohort.
#' @param config a config environment created by the function getConfig.
#' @return the identifier of the matched cohort of patients
#' @export
match_patients_from_num <- function(num = NULL,
                                    num_type = NULL,
                                    birth_range = NULL,
                                    concepts_range = NULL,
                                    n_match = NULL,
                                    match_save= FALSE,
                                    match_save_title = NULL,
                                    config = NULL) {


  patients = get_patients(num, num_type, only_num = FALSE, count = TRUE, config);

  cl<-parallel::makeCluster(parallel::detectCores())

  parallel::clusterEvalQ(cl, {library(dplyr); library(stringr); library(MASS); library(tidyr);library(broom);library(DWHtools2);library(boot)})
  parallel::clusterExport(cl, "match_patient")

  res_temp <- parallel::parLapply(cl, 1:nrow(patients) , fun= function(x, patients, num, num_type, birth_range, concepts_range, n_match, config) {
    match_patient(num, num_type, patients[x,'SEX'], patients[x,'BIRTH_YEAR'],birth_range, patients[x,'UNIQ_CONCEPTS'],  concepts_range, n_match, config)
  }, patients= patients, num = num, num_type = num_type, birth_range = birth_range, concepts_range = concepts_range, n_match = n_match, config = config)
  parallel::stopCluster(cl)

 #  res_temp = list()
 #  for (i in (1:nrow(patients))) {
 #    print(i)
 #    res_temp[i] = list(match_patient(num, num_type, patients[i,'SEXE'], patients[i,'ANNEE_NAIS'],birth_range, patients[i,'UNIQ_CONCEPTS'],  concepts_range, n_match, config)
 # ) }

  result <- as.vector(unlist(res_temp))

  # data <- apply(patients, 1, function(p) {
  #   match_patient(num, num_type, p['SEXE'], p['ANNEE_NAIS'],birth_range, p['UNIQ_CONCEPTS'],  concepts_range, n_match, config)
  # })
  #
  # data <- as.vector(data)

  if (config$backend == 'i2b2_oracle' | config$backend == 'i2b2_postgres') {

    num_temp <- insert_patients_into_query_result_instance(result, config)

  } else if (config$backend == 'drwh_oracle') {

    num_temp <- insert_patients_into_dwh_resultat(result, config)

    if(match_save) {
      num_save <- create_cohorte(match_save_title,
                                 stringr::str_interp("cohort created by multimodal PheWAS during the analysis of the cohort ${num}"),
                                 config$username, config)

      add_privilege_to_cohort(num_save, config$username, 'voir_stats', config)

      add_patients_to_cohort(num_save, result, config$username, config)

    }

  }

  return (num_temp)

}

#' insert_patients_into_dwh_resultat
#'
#' For Dr.Warehouse
#' inser patients in DWH_RESULTAT
#'
#' @param patients a vector of patient_nums
#' @param config a config environment created by the function getConfig.
#' @return num_temp
#' @export
insert_patients_into_dwh_resultat <- function(patients,  config) {


  sql = "SELECT DWH_TEMP_SEQ.nextval FROM DUAL";
  res <- oracleQuery(sql, config)
  num_temp <- res$NEXTVAL[1]

  dt <- data.frame(NUM_TEMP = num_temp,
             NUM_USER_RESULTAT = config$username,
             ID_DOCUMENT = 1,
             PATIENT_NUM = patients)

  values <- apply(dt, 1 , paste, collapse = "','")
  values <- paste0("('",values, "')")
  values <- paste0("INTO DWH_RESULTAT (NUM_TEMP, NUM_USER_RESULTAT, ID_DOCUMENT, PATIENT_NUM) VALUES ", values)
  values <- paste(values, collapse = "\n")

  sql <- stringr::str_interp("INSERT ALL
                             ${values}
                             SELECT 1 FROM DUAL")

   print(sql)
   oracleQuery(sql, config, update = T, data = F)
   return(num_temp)

}

#' insert_patients_into_query_result_instance
#'
#' For i2b2
#' inser patients in QUERY_RESULT_INSTANCE
#'
#' @param patients a vector of patient_nums
#' @param config a config environment created by the function getConfig.
#' @return result_id
#' @export
insert_patients_into_query_result_instance <- function(patients,  config) {

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

  if (config$backend == 'i2b2_oracle') {
    values <- paste0("INTO qt_patient_set_collection (result_instance_id, PATIENT_NUM) VALUES ", values)
    values <- paste(values, collapse = "\n")

    sql <- stringr::str_interp("INSERT ALL
                               ${values}
                               SELECT 1 FROM DUAL")

  } else if (config$backend == 'i2b2_postgres') {

    values <- paste0(values, collapse = ',')
    sql <- stringr::str_interp("INSERT INTO i2b2demodata.qt_patient_set_collection (result_instance_id, PATIENT_NUM) VALUES
                               ${values}")
  }

  oracleQuery(sql, config, update = T, data = F)
  return(result_id)

}

#' create_cohorte
#'
#' For Dr.Warehouse
#' creates a cohort
#'
#' @param titre_cohorte cohort title
#' @param description_cohorte description of the cohort
#' @param config a config environment created by the function getConfig.
#' @return num_cohorte
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

#' add_privilege_to_cohort
#' For Dr. Warehouse
#' @param num_cohorte cohort number
#' @param username username
#' @param privilege privilege
#' @param config a config environment created by the function getConfig.
#' @return 0
#' @export
add_privilege_to_cohort <- function(num_cohorte, username, privilege, config)
{

  # privilege: c('voir_stat')
  sql = stringr::str_interp("insert into dwh_cohorte_user_droit  (num_cohorte , num_user_cohorte ,droit) values (${num_cohorte},${username},'${privilege}')")
  oracleQuery(sql, config, update = F, data = F)

  return(0)

}

#' add_patients_to_cohort
#' For Dr. Warehouse
#' @param num_cohorte cohort number
#' @param patient_nums vector of patient_nums
#' @param username username
#' @param config a config environment created by the function getConfig.
#' @return 0
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

#' get_patients_counts_from_dwh
#'
#' For Dr. Warehouse
#'
#' creates a table with the count of unique concepts per patients.
#' @param config a config environment created by the function getConfig.
#'
#' @export
get_patients_counts_from_dwh <- function(config = config) {

  date_maj_counts <- oracleQuery('SELECT max(D_MAJ) as d_maj from NEU_CONCEPTS_COUNTS', config)
  date_maj_enrsem <- oracleQuery('SELECT max(D_MAJ) as d_maj from DWH_ENRSEM', config)

  if (as.Date(date_maj_counts$D_MAJ) < as.Date(date_maj_enrsem$D_MAJ)) {
    # drop old concepts counts
    oracleQuery('DROP TABLE NEU_CONCEPTS_COUNTS', config, data = F)

    # create new concept counts
    oracleQuery('CREATE TABLE NEU_CONCEPTS_COUNTS as
                (SELECT e.*, d.min_date, max_date, duree_suivi, count_doc, (SELECT max(D_MAJ) from DWH_ENRSEM) AS d_maj from
                (SELECT patient_num, count(DISTINCT certitude ||code) as uniq_concepts, count (certitude ||code) as count_total
                FROM dwh_enrsem
                WHERE contexte=\'texte_patient\'
                GROUP BY patient_num) e
                LEFT JOIN DWH_PATIENT p
                on p.PATIENT_NUM = e.PATIENT_NUM
                LEFT JOIN (SELECT patient_num, min(date_document) AS min_date, max(date_document) AS max_date, max(date_document) - min(date_document) AS duree_suivi, count(*) AS count_doc
                FROM DWH_DOCUMENT
                GROUP BY patient_num) d
                ON p.PATIENT_NUM = d.patient_num)', config, data = F)
  }

  }
