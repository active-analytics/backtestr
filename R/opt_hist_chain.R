#' Retrieve all option chain from opt_hist table
#' 
#' Use this function to get all the option prices for a given trade date,
#' underlying, expiration combination.
#' @export
#' 
opt_hist_chain <- function(trade_date
                         , underlying
                         , expiration){
    
    # constructing the query string, would like to find a 
    # more robust way of doing this
    chr_query <- 
        paste0("select * from opt_hist where trade_date='", trade_date 
               ,"' and underlying='",  underlying
               ,"' and expiration='", expiration, "';")
    
    # getting the data from the database
    df_data <- db_opt_hist(chr_query)
    
    
    df_data <- df_data %>% arrange(strike)
    
    df_data
}