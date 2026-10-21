##Check for the allowed number of observations
simple_check <- function(data) {
  ###condition 1. N > 10
    data <- data %>%
      group_by(cm, outcome, subgroup) %>%
      mutate (
        flag = any(N < 10)
      ) %>%
      mutate(
        estimate = ifelse(flag, NA, estimate), 
        inc = ifelse(flag, NA, inc), 
        N = ifelse(flag, NA, N)
      ) %>%
      ungroup() %>%
      filter(!is.na(estimate))
    

    ###condition 3. for proportions only
    if (all(data$estimate <= 1, na.rm = TRUE) & all(!is.na(data$estimate))) {
      
      data <- data %>%
        mutate(
          N_estimate = N * estimate,
          N_estimate_min = N * (1 - estimate)
        )  %>%
        group_by(cm, outcome, subgroup) %>%
        mutate(
          flag = any(N_estimate < 10 | N_estimate_min < 10)
        ) %>%
        mutate(
          estimate = ifelse(flag, NA, estimate), 
          inc = ifelse(flag, NA, inc), 
          N = ifelse(flag, NA, N)
        ) %>%
        ungroup() %>%
        select(-c(flag, N_estimate, N_estimate_min)) %>%
        filter(!is.na(estimate))
  
    }
    
    ### delete incomplete gradients
    data <- data %>%
      group_by(cm, outcome, subgroup) %>%
      mutate(
        estimate = if (n() < 20) NA else estimate #needs to have all 20 bins
      ) %>%
      ungroup() %>%
      filter(!is.na(estimate))
    
    
    data <-
      data %>%
      mutate(bin = recode(as.character(bin),
                           "1" = "0% - 5%", "2" = "5% - 10%", "3" = "10% - 15%", "4" = "15% - 20%",
                           "5" = "20% - 25%", "6" = "25% - 30%", "7" = "30% - 35%", "8" = "35% - 40%",
                           "9" = "40% - 45%", "10" = "45% - 50%", "11" = "50% - 55%", "12" = "55% - 60%",
                           "13" = "60% - 65%", "14" = "65% - 70%", "15" = "70% - 75%", "16" = "75% - 80%",
                           "17" = "80% - 85%", "18" = "85% - 90%", "19" = "90% - 95%", "20" = "95% - 100%"))

  return(data)
}

complex_check <-  function(data, variable_names) {
  
  for (name in variable_names) {
    #define variable names
    N <- paste0("N_", name)
    estimate <- paste0("estimate_", name)
    inc <- paste0("inc_", name)
    
    ###condition 1. N > 10
    data <- data %>%
      group_by(cm, subgroup) %>%
      mutate (
        flag = any(!!sym(N) < 10)
      ) %>%
      mutate(
        !!estimate := ifelse(flag, NA, get(estimate)), 
        !!inc := ifelse(flag, NA, get(inc)), 
        !!N := ifelse(flag, NA, get(N))
      ) %>%
      ungroup() %>%
      select(-flag) %>%
      filter(!is.na(!!sym(estimate)))
    

    ###condition 3. for proportions only
    if (all(data[[estimate]] <= 1, na.rm = TRUE) & all(!is.na(data[[estimate]]))) {
      
      data <- data %>%
        mutate(
          N_estimate = !!sym(N) * !!sym(estimate),
          N_estimate_min = !!sym(N) * (1 - !!sym(estimate))
        )  %>%
        group_by(cm, subgroup) %>%
        mutate(
          flag = any(N_estimate < 10 | N_estimate_min < 10)
        ) %>%
        mutate(
          !!estimate := ifelse(flag, NA, get(estimate)), 
          !!inc := ifelse(flag, NA, get(inc)), 
          !!N := ifelse(flag, NA, get(N))
        ) %>%
        ungroup() %>%
        select(-c(flag, N_estimate, N_estimate_min)) %>%
        filter(!is.na(estimate))
    }
  }
  
  ### delete incomplete gradients
  data <- data %>%
    group_by(cm, subgroup) %>%
    mutate(
      estimate = if (n() < 20) NA else estimate #needs to have all 20 bins
    ) %>%
    ungroup() %>%
    filter(!is.na(estimate))
  
  data <- data %>%
    select (where (~ !all(is.na(.)))) ###select non-empty columns
  if ("bin" %in% names(data)) {
    data <- data %>%
      mutate( bin = recode(as.character(bin),
                          "1" = "0% - 5%", "2" = "5% - 10%", "3" = "10% - 15%", "4" = "15% - 20%",
                          "5" = "20% - 25%", "6" = "25% - 30%", "7" = "30% - 35%", "8" = "35% - 40%",
                          "9" = "40% - 45%", "10" = "45% - 50%", "11" = "50% - 55%", "12" = "55% - 60%",
                          "13" = "60% - 65%", "14" = "65% - 70%", "15" = "70% - 75%", "16" = "75% - 80%",
                          "17" = "80% - 85%", "18" = "85% - 90%", "19" = "90% - 95%", "20" = "95% - 100%"))
  }
    return(data)
}




bmi_p_check <- function(data) {
  
  ###condition 1. N > 10
  data <- data %>%
    group_by(cm) %>%
    mutate (
      flag = any(N < 10)
    ) %>%
    mutate(
      p10 = ifelse(flag, NA, p10), 
      p25 = ifelse(flag, NA, p25), 
      p50 = ifelse(flag, NA, p50), 
      p75 = ifelse(flag, NA, p75), 
      p90 = ifelse(flag, NA, p90), 
      inc = ifelse(flag, NA, inc), 
      N = ifelse(flag, NA, N)
    ) %>%
    ungroup() %>%
    select(-flag) %>%
    filter(!is.na(N))
  

  for (per in c("10", "25", "50", "75", "90")) {
    p <- paste0("p", per)
    
    ###condition 1. N > 10
    data <- data %>%
      group_by(cm) %>%
      mutate (
        flag = any(get(p) < 10)
      ) %>%
      mutate(
        !!p := ifelse(flag, NA, get(p))
      ) %>%
      ungroup() %>%
      select(-flag) %>%
      filter(!is.na(N))
    
  }
  
  #delete incomplete gradients 
  data <- data %>%
    group_by(cm) %>%
    mutate(
      N = if (n() < 20) NA else N #needs to have all 20 bins
    ) %>%
    ungroup() %>%
    filter(!is.na(N))
  
  data <- data %>%
    select (where (~ !all(is.na(.)))) ###select non-empty columns
  if ("bin" %in% names(data)) {
    data <- data %>%
      mutate( bin = recode(as.character(bin),
                           "1" = "0% - 5%", "2" = "5% - 10%", "3" = "10% - 15%", "4" = "15% - 20%",
                           "5" = "20% - 25%", "6" = "25% - 30%", "7" = "30% - 35%", "8" = "35% - 40%",
                           "9" = "40% - 45%", "10" = "45% - 50%", "11" = "50% - 55%", "12" = "55% - 60%",
                           "13" = "60% - 65%", "14" = "65% - 70%", "15" = "70% - 75%", "16" = "75% - 80%",
                           "17" = "80% - 85%", "18" = "85% - 90%", "19" = "90% - 95%", "20" = "95% - 100%"))
  }
  return(data)
}
