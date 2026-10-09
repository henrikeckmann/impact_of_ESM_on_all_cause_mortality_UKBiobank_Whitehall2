library(irr)
library(tidyr)

cors_df = function(variable = "avgaccel", type = "mod"){
  if (type != "mod" & type != "naive"){
    print("Invalid type. Type must be either 'mod' or 'naive'")
    return(NULL)
  }
  
  if (variable != "avgaccel" & variable != "ig"){
    print("Invalid variable. Type must be either 'avgaccel' or 'ig'")
    return(NULL)
  }
  
  df = as.data.frame(tibble(.rows=6))
  df$Var1 = c(
    rep(paste0(variable, "_enmo"), 3),
    rep(paste0(variable, "_mad_", type), 2),
    paste0(variable, "_mims_", type)
  )
  df$Var2 = c(
      paste0(variable, "_mad_", type),
      paste0(variable, "_mims_", type),
      paste0(variable, "_counts_", type),
      paste0(variable, "_mims_", type),
      paste0(variable, "_counts_", type),
      paste0(variable, "_counts_", type)
  )
  for (i in 1:nrow(df)){
    if (variable == "ig"){
      icc_results = icc(
        data.frame(
          descriptives_df[df[i,1]],
          descriptives_df[df[i,2]]
        ),
        model = "twoway",
        type = "consistency",
        unit = "single",
      )
      
      df[i, "icc"] = format(round(icc_results$value, 2), nsmall = 2)
      df[i, "icc_lci"] = format(round(icc_results$lbound, 2), nsmall = 2)
      df[i, "icc_uci"] = format(round(icc_results$ubound, 2), nsmall = 2)
    }
    cor_results = cor.test(
      pull(descriptives_df[df[i,1]]),
      pull(descriptives_df[df[i,2]])
    )
    df[i, "cor"] = format(round(cor_results$estimate, 2), nsmall = 2)
    df[i, "cor_lci"] = format(round(cor_results$conf.int[1], 2), nsmall = 2)
    df[i, "cor_uci"] = format(round(cor_results$conf.int[2], 2), nsmall = 2)
  }
  return(df)
}

cors_df_withspearman = function(variable = "avgaccel", type = "mod"){
  if (type != "mod" & type != "naive"){
    print("Invalid type. Type must be either 'mod' or 'naive'")
    return(NULL)
  }
  
  if (variable != "avgaccel" & variable != "ig"){
    print("Invalid variable. Type must be either 'avgaccel' or 'ig'")
    return(NULL)
  }
  
  df = as.data.frame(tibble(.rows=6))
  df$Var1 = c(
    rep(paste0(variable, "_enmo"), 3),
    rep(paste0(variable, "_mad_", type), 2),
    paste0(variable, "_mims_", type)
  )
  df$Var2 = c(
      paste0(variable, "_mad_", type),
      paste0(variable, "_mims_", type),
      paste0(variable, "_counts_", type),
      paste0(variable, "_mims_", type),
      paste0(variable, "_counts_", type),
      paste0(variable, "_counts_", type)
  )
  for (i in 1:nrow(df)){
    if (variable == "ig"){
      icc_results = icc(
        data.frame(
          descriptives_df[df[i,1]],
          descriptives_df[df[i,2]]
        ),
        model = "twoway",
        type = "consistency",
        unit = "single",
      )
      
      df[i, "icc"] = format(round(icc_results$value, 2), nsmall = 2)
      df[i, "icc_lci"] = format(round(icc_results$lbound, 2), nsmall = 2)
      df[i, "icc_uci"] = format(round(icc_results$ubound, 2), nsmall = 2)
    }
    cor_results = cor.test(
      pull(descriptives_df[df[i,1]]),
      pull(descriptives_df[df[i,2]])
    )
    df[i, "cor"] = format(round(cor_results$estimate, 2), nsmall = 2)
    df[i, "cor_lci"] = format(round(cor_results$conf.int[1], 2), nsmall = 2)
    df[i, "cor_uci"] = format(round(cor_results$conf.int[2], 2), nsmall = 2)
    
    cor_results_sm = cor.test(
      pull(descriptives_df[df[i,1]]),
      pull(descriptives_df[df[i,2]]),
      method = "spearman"
    )
    df[i, "rho"] = format(round(cor_results_sm$estimate, 2), nsmall = 2)
  }
  return(df)
}


metric_cors_df = function(){
  if (!(exists("descriptives_df"))){
    print("Error. Need to have descriptives_df loaded.")
    return(NULL)
  }
  
  df = as.data.frame(tibble(.rows=7))
  df$AvAcc = c(
    "avgaccel_enmo",
    "avgaccel_mad_mod",
    "avgaccel_mims_mod",
    "avgaccel_counts_mod",
    "avgaccel_mad_naive",
    "avgaccel_mims_naive",
    "avgaccel_counts_naive"
  )
  df$IG = c(
    "ig_enmo",
    "ig_mad_mod",
    "ig_mims_mod",
    "ig_counts_mod",
    "ig_mad_naive",
    "ig_mims_naive",
    "ig_counts_naive"
  )
  for (i in 1:nrow(df)){
    cor_results = cor.test(
      pull(descriptives_df[df[i,1]]),
      pull(descriptives_df[df[i,2]])
    )
    df[i, "cor"] = format(round(cor_results$estimate, 2), nsmall = 2)
    df[i, "cor_lci"] = format(round(cor_results$conf.int[1], 2), nsmall = 2)
    df[i, "cor_uci"] = format(round(cor_results$conf.int[2], 2), nsmall = 2)
  }
  return(df)
}

cors_df_o60 = function(variable = "avgaccel", type = "mod"){
  if (type != "mod" & type != "naive"){
    print("Invalid type. Type must be either 'mod' or 'naive'")
    return(NULL)
  }
  
  if (variable != "avgaccel" & variable != "ig"){
    print("Invalid variable. Type must be either 'avgaccel' or 'ig'")
    return(NULL)
  }
  
  df = as.data.frame(tibble(.rows=6))
  df$Var1 = c(
    rep(paste0(variable, "_enmo"), 3),
    rep(paste0(variable, "_mad_", type), 2),
    paste0(variable, "_mims_", type)
  )
  df$Var2 = c(
    paste0(variable, "_mad_", type),
    paste0(variable, "_mims_", type),
    paste0(variable, "_counts_", type),
    paste0(variable, "_mims_", type),
    paste0(variable, "_counts_", type),
    paste0(variable, "_counts_", type)
  )
  for (i in 1:nrow(df)){
    if (variable == "ig"){
      icc_results = icc(
        data.frame(
          descriptives_df_o60[df[i,1]],
          descriptives_df_o60[df[i,2]]
        ),
        model = "twoway",
        type = "consistency",
        unit = "single",
      )
      
      df[i, "icc"] = format(round(icc_results$value, 2), nsmall = 2)
      df[i, "icc_lci"] = format(round(icc_results$lbound, 2), nsmall = 2)
      df[i, "icc_uci"] = format(round(icc_results$ubound, 2), nsmall = 2)
    }
    cor_results = cor.test(
      pull(descriptives_df_o60[df[i,1]]),
      pull(descriptives_df_o60[df[i,2]])
    )
    df[i, "cor"] = format(round(cor_results$estimate, 2), nsmall = 2)
    df[i, "cor_lci"] = format(round(cor_results$conf.int[1], 2), nsmall = 2)
    df[i, "cor_uci"] = format(round(cor_results$conf.int[2], 2), nsmall = 2)
  }
  return(df)
}

metric_cors_df_o60 = function(){
  if (!(exists("descriptives_df"))){
    print("Error. Need to have descriptives_df loaded.")
    return(NULL)
  }
  
  df = as.data.frame(tibble(.rows=7))
  df$AvAcc = c(
    "avgaccel_enmo",
    "avgaccel_mad_mod",
    "avgaccel_mims_mod",
    "avgaccel_counts_mod",
    "avgaccel_mad_naive",
    "avgaccel_mims_naive",
    "avgaccel_counts_naive"
  )
  df$IG = c(
    "ig_enmo",
    "ig_mad_mod",
    "ig_mims_mod",
    "ig_counts_mod",
    "ig_mad_naive",
    "ig_mims_naive",
    "ig_counts_naive"
  )
  for (i in 1:nrow(df)){
    cor_results = cor.test(
      pull(descriptives_df_o60[df[i,1]]),
      pull(descriptives_df_o60[df[i,2]])
    )
    df[i, "cor"] = format(round(cor_results$estimate, 2), nsmall = 2)
    df[i, "cor_lci"] = format(round(cor_results$conf.int[1], 2), nsmall = 2)
    df[i, "cor_uci"] = format(round(cor_results$conf.int[2], 2), nsmall = 2)
  }
  return(df)
}
