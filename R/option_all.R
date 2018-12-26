#' Retrieve all options for a given trade date.
#' 
#' Use this function to get all the option prices for a given trade date.
#' @keywords option
#' @export
#' 
option_all <- function(trade_date
                       , exclude_zero_bid = FALSE){
    
    # constructing the query string, would like to find a 
    # more robust way of doing this
    chr_query <- 
        paste0(
            "select * from option_price where DataDate='"
             , trade_date 
             , "';"
        )
    
    # getting the data from the database
    df_data <- db_option(chr_query, exclude_zero_bid)
    
    df_data
}