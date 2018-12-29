#' Retrieve all options for a given underlying 
#' 
#' Use this function to get all the option prices for a given trade date and
#' underlying
#' @export
#' 
option_underlying <- function(trade_date
                              , underlying
                              , exclude_zero_bid = FALSE){
    
    # constructing the query string, would like to find a 
    # more robust way of doing this
    chr_query <- 
        paste0("select * from option_price where DataDate='", trade_date 
               ,"' and UnderlyingSymbol='",  underlying
               ,"';")
    
    # getting the data from the database
    df_data <- db_option(chr_query, exclude_zero_bid)
    
    df_data
}