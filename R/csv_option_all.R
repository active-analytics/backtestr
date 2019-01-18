#' Reads daily option file from a specific path
#' 
#' Reads daily option file from a specific path
#' @export



csv_option_all <- function(chr_path
                           , exclude_zero_bid = FALSE){

    df_data <- 
        readr::read_csv(
            chr_path
            , col_types = column_specification("delta_neutral_option")
        )
    
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

