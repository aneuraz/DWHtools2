This package helps to perform SQL queries in 2 different clinical
datawarehouses: i2b2 and Dr.Warehouse. It contains 2 generic functions
that allow to perform any SQL query as well as more specific queries
allowing to query specific information. For example, the function
`get_patients` will return demographic information from a patient
cohort.

Setup
-----

We will assume here that you have already configured the tools needed
for the connection to the database. The package can handle Oracle and
Postgres back-ends.

### Install from github

    if(!require(devtools)) install.packages('devtools')
    devtools::install_github('aneuraz/DWHtools2')

### Config file

The first thing to do to use this package is to create a text file
containing the information for the connection to the database. Here is a
description of this file:

-   driverClass: The Oracle JDBC driver class that implements the
    java.sql.Driver interface
-   classPath: The path to the driver
-   connectPath: The connection path to the database
    "jdbc:oracle:thin:@//H61-ENTREPOT.NCK.APHP.FR:1521/imagine"
-   dbuser: The database user
-   dbpass: The database user password
-   username: The username of the user in the web application (not
    necessarily the same as the database user)
-   backend: The backend of the CDW. one of 'drwh\_orcale',
    'i2b2\_oracle', 'i2b2\_postgres'

An example for Oracle:

    driverClass="oracle.jdbc.OracleDriver"
    classPath="<INSTANTCLIENT_DIRECTORY>/ojdbc6.jar"
    connectPath="jdbc:oracle:thin:@<URL>:<PORT>/<DBNAME>"
    dbuser="<USER>"
    dbpass="<PWD>"
    username="DWHUSER"
    backend="drwh_oracle"

Usage
-----

Load the package:

    library(DWHtools2)

First, you need to get your config information:

    config <- getConfig('<NAME_OF_YOUR_CONFIG_FILE>')
