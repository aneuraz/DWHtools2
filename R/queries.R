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
patients_subquery <- function(num_type) {

  if(!(num_type %in% c('num_temp', 'cohorte'))) {
    stop('num_type must be either "num_type" or "cohorte"')
  }

  if (num_type == 'num_temp') {

    return("SELECT distinct PATIENT_NUM from DWH_RESULTAT
    where num_temp = ")

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
get_patients <- function(num = NULL, num_type = NULL, only_num = FALSE, count = FALSE, config = NULL) {

  if (is.null(num) | is.null(num_type)) {
    stop('valid num and num_type are required')
  }


  if (is.null(config)) {
    stop("a valid config object is required")
  }

  count_query <- ""
  test_count <- ""

  if (!only_num) {
    columns <- "p.PATIENT_NUM, p.DATENAIS, p.SEXE, p.CP, p.PAYS, p.PAYS_NAISSANCE, p.CODE_DECES, TO_CHAR(p.DATENAIS, 'YYYY') as ANNEE_NAIS"
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
  FROM DWH_PATIENT p
  ${count_query}
  where p.PATIENT_NUM in ( ${subquery} ${num})
  ${test_count}")


  res <- oracleQuery(sql, config)

  return(res)

}

#patients <- get_patients('13624005402', 'cohorte',  config = config)
#patients <- get_patients('13624005402', 'cohorte', only_num = FALSE, count = TRUE, config = config)

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
get_data <- function(num = NULL, num_type = NULL, data_type = NULL, config = NULL) {

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

    sql <- stringr::str_interp("SELECT d.PATIENT_NUM, d.IEP, t.CODE, t.CODE_LIBELLE, d.VAL_NUMERIC, d.DATE_DOCUMENT,
            d.BORNE_INF, d.BORNE_SUP, d.ID_THESAURUS_DATA , tt.code_libelle AS LIBELLE_PARENT,
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

    sql <- stringr::str_interp("SELECT d.PATIENT_NUM, d.IEP, t.CODE, t.CODE_LIBELLE, d.VAL_TEXTE, d.DATE_DOCUMENT,
                      d.ID_THESAURUS_DATA , tt.code_libelle AS LIBELLE_PARENT, tt.code as CODE_PARENT
                      FROM DWH_DATA d
                      LEFT JOIN DWH_THESAURUS_DATA t
                      ON d.id_thesaurus_data = t.ID_THESAURUS_DATA
                      LEFT JOIN DWH_THESAURUS_DATA tt
                      ON t.id_thesaurus_parent = tt.ID_THESAURUS_DATA
                      WHERE d.THESAURUS = 'cim10'
                      AND d.patient_num in ( ${subquery} ${num} )")
  }

  res <- oracleQuery(sql, config)

  return(res)
}

#cim <- get_data('13624005579', 'cohorte', 'cim10', config = config)
#bio <- get_data('13624005579', 'cohorte', 'bio_num', config = config)

#' match_patient
#' @export
match_patient <- function(num = NULL, num_type = NULL, sexe = NULL, annee_nais = NULL, annee_range = NULL, count_unique = NULL, count_range = NULL, n_match = NULL, config = NULL) {

  subquery <- patients_subquery(num_type)

  sql <- stringr::str_interp("SELECT PATIENT_NUM from (
  SELECT c.PATIENT_NUM
  from NEU_CONCEPTS_COUNTS c
  LEFT JOIN dwh_patient p
  on p.PATIENT_NUM = c.PATIENT_NUM
  WHERE p.PATIENT_NUM NOT IN ( ${subquery} ${num} )
  and p.SEXE = '${sexe}'
  and TO_NUMBER(TO_CHAR(p.DATENAIS, 'YYYY')) between (${annee_nais} - ${annee_range}) and (${annee_nais} + ${annee_range})
  and c.COUNT_UNIQUE between (${count_unique} - ${count_unique}*${count_range}) and (${count_unique} + ${count_unique}*${count_range})
  order by PATIENT_NUM) r
  ")

  res <- oracleQuery(sql, config)


  if (nrow(res)> n_match) {
    res <- res[sample(1:nrow(res), n_match),]
  } else {
    res <- res[sample(1:nrow(res), nrow(res)),]
  }

  return(res)

}

#set.seed(456)
# match_patient('13624005579',
#               'cohorte',
#               sexe = patients$SEXE[1],
#               annee_nais = patients$ANNEE_NAIS[1],
#               annee_range = 5,
#               count_unique = 120,
#               count_range = 0.2,
#               n_match = 5,
#               config = config)

#' match_patients_from_num
#' @export
match_patients_from_num <- function(num = NULL, num_type = NULL, annee_range = NULL, count_range = NULL, n_match = NULL,match_save= FALSE, match_save_title = NULL, config = NULL) {



  patients = get_patients(num, num_type, only_num = FALSE, count = TRUE, config);

  cl<-parallel::makeCluster(parallel::detectCores())

  parallel::clusterEvalQ(cl, {library(dplyr); library(stringr); library(MASS); library(tidyr);library(broom);library(DWHtools);library(boot)})
  parallel::clusterExport(cl, "match_patient")

  res_temp <- parallel::parLapply(cl, 1:nrow(patients) , fun= function(x, patients, num, num_type, annee_range, count_range, n_match, config) {
    match_patient(num, num_type, patients[x,'SEXE'], patients[x,'ANNEE_NAIS'],annee_range, patients[x,'COUNT_UNIQUE'],  count_range, n_match, config)
  }, patients= patients, num = num, num_type = num_type, annee_range = annee_range, count_range = count_range, n_match = n_match, config = config)
  parallel::stopCluster(cl)

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


#controls <- match_patients_from_num(num = '13624005402', num_type= 'cohorte',annee_range = 5,count_range = 0.3,n_match = 5, config = config)

#' insert_patients_into_dwh_resultat
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

#num_control <- insert_patients_into_dwh_resultat(controls, config)

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

#' get_patients_counts_from_dwh
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
                (SELECT patient_num, count(DISTINCT certitude ||code) as count_unique, count (certitude ||code) as count_total
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

  # retrieve counts
  # oracleQuery('SELECT c.PATIENT_NUM, c.COUNT_UNIQUE, c.COUNT_D_TOTAL, p.DATENAIS, p.SEXE, p.CP, p.PAYS, p.PAYS_NAISSANCE, p.CODE_DECES, TO_NUMBER(TO_CHAR(p.DATENAIS, \'YYYY\')) as ANNEE_NAIS
  #             from NEU_CONCEPTS_COUNTS c
  #             LEFT JOIN dwh_patient p
  #             on p.PATIENT_NUM = c.PATIENT_NUM', config)

  }
