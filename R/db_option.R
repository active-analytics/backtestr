#' Retrieve all results from a query
#' 
#' Retrieves database entries for a specific query
#' @export



db_option <- function(chr_query 
                      , exclude_zero_bid){
    
    # getting the database connection
    db_conn <- db_connection()
    
    # code to retreive data from database
    rs <- RMariaDB::dbSendQuery(db_conn, chr_query)
    data <- RMariaDB::dbFetch(rs, -1)
    RMariaDB::dbHasCompleted(rs)
    df_data <- tibble::as_tibble(data) # converting to tibble
    RMariaDB::dbClearResult(rs)
    RMariaDB::dbDisconnect(db_conn)
    
    # selecting only the columns that we want
    df_data <-
        df_data %>% 
        dplyr::select(
            underlying_symbol = UnderlyingSymbol
            , underlying_price = UnderlyingPrice
            #, flags = Flags
            #, option_symbol = OptionSymbol
            , type = Type
            , expiration = Expiration
            , data_date = DataDate
            , strike = Strike
            #, last = Last
            , bid = Bid
            , ask = Ask
            , volume = Volume
            , open_interest = OpenInterest
            , t1_open_interest = T1OpenInterest
            , iv_mean = IVMean
            #, iv_bid = IVBid
            #, iv_ask = IVAsk
            #, delta = Delta
            #, gamma = Gamma
            #, theta = Theta
            #, vega = Vega
            #, aka = AKA
        ) %>% 
        dplyr::mutate(
            mid = (bid + ask) / 2
            #, delta = abs(delta)
        )
    
    if (exclude_zero_bid){
        df_data <- 
            df_data %>% dplyr::filter(bid > 0)
    }
    
    df_data <- 
        df_data %>% dplyr::select(underlying_symbol:ask, mid, volume:iv_mean)
    
    df_data
    
}

