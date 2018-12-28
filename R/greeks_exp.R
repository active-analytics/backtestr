#' Cacluate the option greeks on expiration day
#' 
#' Use this function to calculate the option greeks on expiration day
#' @export
#' 

greeks_exp <- function(df_opt_hist){
    
    # option payoff function
    payoff <- function(type, strike , underlying_price){
        if(type == "put"){return(max(strike - underlying_price, 0))}
        if(type == "call"){return(max(underlying_price -strike, 0))}
    }
    
    
    # calculating option payoffs    
    df_payoff_inputs <- 
        df_opt_hist %>% 
        dplyr::select(type, strike, underlying_price)
    
    
    # setting the expiration bid, ask, and mid
    df_opt_hist$bid <- 
        purrr::pmap_dbl(df_payoff_inputs, payoff)
    df_opt_hist$ask <- 
        purrr::pmap_dbl(df_payoff_inputs, payoff)
    df_opt_hist$mid <- 
        purrr::pmap_dbl(df_payoff_inputs, payoff)
    
    exp_delta <- function(payoff){
        my_delta <- 0
        if(payoff > 0){my_delta <- 1}
        my_delta
    }
    
    # setting greek values
    df_opt_hist <- 
        df_opt_hist %>% 
        dplyr::mutate(
            implied_vol = 0
            , delta = purrr::map_dbl(mid, exp_delta)
            , vega = 0
            , theta = 0
        )
    
    df_opt_hist
}