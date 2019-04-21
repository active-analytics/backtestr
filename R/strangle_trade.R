#' Get strangle trades a given underlying, execution date, and expiration
#' 
#' Use this function identify strangle trades for a given chain on a give
#' execution date
#' @export
#' 
strangle_trade <- function(underlying, execution
                           , expiration, delta_target){
    
    df_chain <- opt_hist_chain(execution, underlying, expiration)
    
    
    df_put <-
        df_chain %>% 
        dplyr::filter(type == "put")  %>% 
        dplyr::filter(
            abs(delta - delta_target) == min(abs(delta - delta_target))
        )
    
    
    df_call <-
        df_chain %>% 
        dplyr::filter(type == "call")  %>% 
        dplyr::filter(
            abs(delta - delta_target) == min(abs(delta - delta_target))
        )
    
    
    df_strangle <- dplyr::bind_rows(df_put, df_call)
    
    
    df_strangle    
}