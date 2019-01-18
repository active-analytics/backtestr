#' Creates column specification for Delta-Neutral EOD file
#' 
#' Creates column specification for Delta-Neutral EOD file
#' @export


column_specification <- function(file_type){

col_spec <- readr::cols()  

#------------------------------------------
# delta neutral data ######################
#------------------------------------------ 
# L3 Option Data
if (file_type=="delta_neutral_option"){
  col_spec <- readr::cols(
    UnderlyingSymbol = readr::col_character(),
    UnderlyingPrice = readr::col_double(),
    Flags = readr::col_character(),
    OptionSymbol = readr::col_character(),
    Type = readr::col_character(),
    Expiration = readr::col_date(format="%m/%d/%Y"),
    DataDate = readr::col_date(format="%m/%d/%Y"),
    Strike = readr::col_double(),
    Last = readr::col_double(),
    Bid = readr::col_double(),
    Ask = readr::col_double(),
    Volume = readr::col_integer(),
    OpenInterest = readr::col_integer(),
    T1OpenInterest = readr::col_integer(),
    IVMean = readr::col_double(),
    IVBid = readr::col_double(),
    IVAsk = readr::col_double(),
    Delta = readr::col_double(),
    Gamma = readr::col_double(),
    Theta = readr::col_double(),
    Vega = readr::col_double(),
    AKA = readr::col_character()
  )
}  



## NOTE: For some reason this doesn't work when I use the correct readr::col_date
#        format.  I'm not sure why, don't have time to look into it now
#        10/29/2018.
# L3 Stock Data
if (file_type=="delta_neutral_stock"){
    col_spec <- readr::cols(
        symbol = readr::col_character(),
        quotedate = readr::col_character(), #readr::col_date(format="%m/%d/%Y"),
        open = readr::col_double(),
        high = readr::col_double(),
        low = readr::col_double(),
        close = readr::col_double(),
        volume = readr::col_integer(),
        adjustedclose = readr::col_double()
    )
}  


#----------#
# opt_hist #
#----------#
if (file_type=="opt_hist"){
  col_spec <- readr::cols(
    underlying_symbol = readr::col_character(),
    underlying_price = readr::col_double(),
    flags = readr::col_character(),
    option_symbol = readr::col_character(),
    type = readr::col_character(),
    expiration = readr::col_date(format="%Y-%m-%d"),
    data_date = readr::col_date(format="%Y-%m-%d"),
    strike  = readr::col_double(),
    last = readr::col_double(),
    bid = readr::col_double(),
    ask = readr::col_double(),
    mid = readr::col_double(),
    volume = readr::col_integer(),
    open_interest = readr::col_integer(),
    t1_open_interest = readr::col_integer(),
    iv_mean = readr::col_double(),
    iv_bid = readr::col_double(), 
    iv_ask = readr::col_double(),
    delta = readr::col_double(),
    gamma = readr::col_double(),
    theta = readr::col_double(),
    vega = readr::col_double(),
    aka = readr::col_character(),
    my_implied_vol = readr::col_double(),
    my_delta  = readr::col_double(),
    my_vega = readr::col_double(),
    my_theta = readr::col_double()
  )
}

#---------------------------------------------
# option history CSV #########################
#---------------------------------------------
if (file_type=="option_history"){
  col_spec <- readr::cols(
    trade_date  = readr::col_date(format="%Y-%m-%d"),
    option_symbol = readr::col_character(),
    underlying = readr::col_character(),
    type = readr::col_character(),
    expiration = readr::col_date(format="%Y-%m-%d"),
    strike  = readr::col_double(),
    upx = readr::col_character(),
    bid = readr::col_double(),
    ask = readr::col_double(),
    implied_vol = readr::col_double(),
    delta = readr::col_double(),
    vega = readr::col_double(),
    theta = readr::col_double(),
    volume = readr::col_integer(),
    open_interest = readr::col_integer()
  )   
}

col_spec
}


