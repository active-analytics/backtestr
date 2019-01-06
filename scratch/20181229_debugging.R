df_option_all <-  
    option_chain(
        trade_date = lubridate::ymd(20140908)
        , underlying = "SPY"
        , expiration = lubridate::ymd(20140912)
    )

implied_forward(
    df_option_all 
)