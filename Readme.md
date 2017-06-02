This package helps to perform SQL queries in 2 different clinical datawarehouses: i2b2 and Dr.Warehouse. 
It contains 2 generic functions that allow to perform any SQL query as well as more specific queries allowing to query specific information. For example, the function `get_patients` will return demographic information from a patient cohort. 

## Setup

We will assume here that you have already configured the tools needed for the connection to the database. The package can handle Oracle and Postgres back-ends. 

### Install from github

```r
if(!require(devtools)) install.packages('devtools')
devtools::install_github('aneuraz/DWHtools2')
```

### Config file

The first thing to do to use this package is to create a text file containing the information for the connection to the database. 
Here is a description of this file: 

- driverClass: The Oracle JDBC driver class that implements the java.sql.Driver interface
- classPath: The path to the driver
- connectPath: The connection path to the database 
- dbuser: The database user 
- dbpass: The database user password
- username: The username of the user in the web application (not necessarily the same as the database user)
- backend: The backend of the CDW. one of 'drwh_orcale', 'i2b2_oracle', 'i2b2_postgres'

An example for Oracle: 

```
driverClass="oracle.jdbc.OracleDriver"
classPath="<INSTANTCLIENT_DIRECTORY>/ojdbc6.jar"
connectPath="jdbc:oracle:thin:@<URL>:<PORT>/<DBNAME>"
dbuser="<USER>"
dbpass="<PWD>"
username="DWHUSER"
backend="drwh_oracle"
```

## Usage

Load the package: 
```r
library(DWHtools2)
```

First, you need to get your config information:
```r
config <- getConfig('<NAME_OF_YOUR_CONFIG_FILE>')
```

### Generic queries

Then, using the `oracleQuery` function (please use this function even if you are using Postgres), you can write a test query: 
```r
query <- "SELECT count(PATIENT_NUM) FROM DWH_PATIENT" # set the query
res <- oracleQuery(query = query, config = config) # run the query
res # print the results 
```

### Specific queries
You can also use pre-configured queries using specific functions.

#### get_cohorts_list

This function allows you to get the list of cohorts saved by a user: 

```r
cohorts_list <- get_cohorts_list(username = <USERNAME>, only_num = FALSE, config = config) 
```

#### get_patients 

`get_patients` extracts the demographic information for a cohort of patients 

```r
get_patients(num = <COHORT_NUM>, num_type = 'cohorte', config = config)
```

#### get_data

Using `get_data` lets you extract ICD codes or laboratory test results for a selected cohort. 

```r
get_data(num = <COHORT_NUM>,      # cohort or patient set
         num_type = 'cohorte',    # type of num (i.e. 'cohorte' or 'num_temp')
         data_type = 'bio_num',   # type of data to extract (i.e. 'bio_num' or 'cim10')
         ICD_prefix = 'CIM10',    # prefix for ICD codes concepts
         BIO_prefix = 'LOINC',    # prefix for labtest results 
         config = NULL)           # config object
```



