#' Calculates the greeks for each option in a chain
#' 
#' Use this function to calculate greeks for each options in a chain
#' @export
#' 
#'
greeks <- function(df_otm, d2x, impl_fwd){
    
    ########################    
    ## implied volatility ##
    ########################
    # creating the tibble containing the inputs for calculating 
    # implied volatility with fOption::GBSVolatility
    df_implied_vol_inputs <- 
        tibble::tibble(
            price = df_otm$mid,
            TypeFlag = stringr::str_sub(df_otm$type,1,1),
            S = impl_fwd,
            X = df_otm$strike,
            Time = d2x/252,
            r = 0,
            b = 0,
            tol = 0.0001,
            maxiter = 1000  
        )
    
    # calculating implied volatilities iteratively using purrr:mpap_dbl
    # appending them onto df_otm using dplyr::mutate
    df_otm_with_greeks <- 
        df_otm %>%  
        dplyr::mutate(
            implied_vol = df_implied_vol_inputs %>% 
                purrr::pmap_dbl(fOptions::GBSVolatility)
        )
    
    ###########
    ## delta ##
    ###########
    # creating the tibble containing the inputs for calculating 
    # delta with fOption::GBSGreeks
    df_delta_inputs <- 
        tibble::tibble(
            Selection = "delta",
            TypeFlag = stringr::str_sub(df_otm$type,1,1),
            S = impl_fwd,
            X = df_otm$strike,
            Time = d2x/252,
            r = 0,
            b = 0,
            sigma = df_otm_with_greeks$implied_vol
        )
    
    # calculating delta iteratively using purrr:mpap_dbl
    # appending them onto df_otm using dplyr::mutate
    df_otm_with_greeks <- 
        df_otm_with_greeks %>%  
        dplyr::mutate(
            delta = df_delta_inputs %>% 
                purrr::pmap_dbl(fOptions::GBSGreeks)
        )
    
    # changing delta to abs(delta)
    df_otm_with_greeks <- 
        df_otm_with_greeks %>% 
        dplyr::mutate(delta = abs(delta))
    
    
    ##########
    ## vega ##
    ##########
    # creating the tibble containing the inputs for calculating 
    # delta with fOption::GBSGreeks
    df_vega_inputs <- 
        tibble::tibble(
            Selection = "vega",
            TypeFlag = stringr::str_sub(df_otm$type, 1, 1),
            S = impl_fwd,
            X = df_otm$strike,
            Time = d2x/252,
            r = 0,
            b = 0,
            sigma = df_otm_with_greeks$implied_vol
        )
    
    
    # calculating vega iteratively using purrr:mpap_dbl
    # appending them onto df_otm using dplyr::mutate
    df_otm_with_greeks <- 
        df_otm_with_greeks %>%  
        dplyr::mutate(
            vega = df_vega_inputs %>% 
                purrr::pmap_dbl(fOptions::GBSGreeks)
        )
    
    # changing to 1% delta
    df_otm_with_greeks <- 
        df_otm_with_greeks %>% 
        dplyr::mutate(vega = vega * 0.01)
    
    
    ###########
    ## theta ##
    ###########
    # creating the tibble containing the inputs for calculating 
    # delta with fOption::GBSGreeks
    df_theta_inputs <- 
        tibble::tibble(
            Selection = "theta",
            TypeFlag = stringr::str_sub(df_otm$type,1,1),
            S = impl_fwd,
            X = df_otm$strike,
            Time = d2x/252,
            r = 0,
            b = 0,
            sigma = df_otm_with_greeks$implied_vol
        )
    
    # calculating theta iteratively using purrr:mpap_dbl
    # appending them onto df_otm using dplyr::mutate
    df_otm_with_greeks <- 
        df_otm_with_greeks %>%  
        dplyr::mutate(
            theta = df_theta_inputs %>% 
                purrr::pmap_dbl(fOptions::GBSGreeks)
        )
    
    # changing to one trade-day delta
    df_otm_with_greeks <- 
        df_otm_with_greeks %>% 
        dplyr::mutate(theta = theta / 252)
    

    df_otm_with_greeks
}