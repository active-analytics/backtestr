#' Retrieve all options for a given chain
#' 
#' Use this function to get all the option prices for a given trade date,
#' underlying, expiration combination.
#' @importFrom magrittr %>%
#' @export
#' 
option_chain <- function(trade_date
                       , underlying
                       , expiration
                       , exclude_zero_bid = FALSE){
    
    # constructing the query string, would like to find a 
    # more robust way of doing this
    chr_query <- 
        paste0("select * from option_price where DataDate='", trade_date 
               ,"' and UnderlyingSymbol='",  underlying
               ,"' and Expiration='", expiration, "';")
    
    # getting the data from the database
    df_data <- db_option(chr_query, exclude_zero_bid)
    
    df_data
}