#' Return a connection object to delta_neutral database
#'
#' Use this function to get a connection to the delta_neutral database

db_connection <- function(){
    db_conn <- 
        RMariaDB::dbConnect(
            drv = RMariaDB::MariaDB()
            ,user = 'root'
            , password = 'w0rthless1'
            , dbname = 'delta_neutral'
            , host='localhost'
        )
    db_conn
}