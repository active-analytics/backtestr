#' Calculate the implied forward price from an option chain
#' 
#' Use this function to get calculate the forward price from an option chain.
#' @export
#' 
#' 
implied_forward <- function(df_full_chain){
    df_forward_calc_data <- 
        df_full_chain %>% 
        dplyr::filter(ask > 0) %>% # see BUG FIX NOTE
        dplyr::select(type, strike, mid)
        # BUG FIX NOTE: there were some prices with both bid/ask = 0 for
        # SPY weeklys that expire on 9/12/2014 for trade-date 9/8/2014, 
        # when you get a unit testing framework up and running, make sure
        # to try to create a test for this.s
    
    # separating calls
    df_forward_calc_calls <- 
        df_forward_calc_data %>%
        dplyr::select(type, strike, mid) %>% 
        dplyr::filter(type == "call") %>% 
        dplyr::rename(call_price = mid)
    
    # separating puts
    df_forward_calc_puts <- 
        df_forward_calc_data %>%
        dplyr::select(type, strike, mid) %>% 
        dplyr::filter(type == "put") %>%
        dplyr::rename(put_price = mid)
    
    # calculating implied forward at each strike price
    df_forward_calc <- 
        df_forward_calc_puts %>% 
        dplyr::inner_join(df_forward_calc_calls, by = "strike") %>% 
        dplyr::mutate(price_diff = call_price - put_price) %>% 
        dplyr::mutate(implied_forward = strike + call_price - put_price) %>% 
        dplyr::mutate(abs_price_diff = abs(price_diff)) %>% 
        dplyr::arrange(abs_price_diff) %>% 
        dplyr::top_n(-5, abs_price_diff)
    
    # may want to put a check here to see if df_forward_calc 
    # has no elements in it 
    # (this is what was happening to TTT before I was allowing 
    # for options with zero open interest)
    
    dbl_implied_forward <- mean(df_forward_calc$implied_forward)  
    
    dbl_implied_forward

}