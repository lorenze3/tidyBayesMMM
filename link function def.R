link<-function (fit, data, post, flatten = TRUE, symbols, ...) 
{ 
  nest_commas <- function(f, symbols) {
    if (class(f) == "call" || class(f) == "(") {
      if (as.character(f[[1]]) == "[") {
        if (as.character(f[[2]]) %in% symbols) {
          the_call <- as.list(f)
          f <- as.call(unlist(list(the_call[1:2], quote(TRUE), 
                                   the_call[3:length(the_call)]), recursive = FALSE))
        }
      }
      else {
        for (i in 1:length(f)) f[[i]] <- nest_commas(f[[i]], 
                                                     symbols)
      }
    }
    return(f)
  }
  use_orig_data <- FALSE
  if (missing(data)) {
    data <- fit@data
    use_orig_data <- TRUE
  }
  if (missing(post)) 
    post <- extract.samples(fit@stanfit)
  if (!is.null(fit@formula_parsed$link_funcs)) {
    n_links <- length(fit@formula_parsed$link_funcs)
  }
  else {
    stop("No link functions found. Maybe there were no linear models in the formula list?")
  }
  out <- list()
  if (missing(symbols)) {
    symbols <- names(fit@formula_parsed$link_funcs)
  }
  for (j in n_links:1) {
    if (!(names(fit@formula_parsed$link_funcs)[j] %in% symbols)) 
      next
    if (use_orig_data == TRUE) 
      n_cases <- fit@formula_parsed$link_funcs[[j]]$N
    else n_cases <- length(data[[1]])
    symbols_so_far <- names(out)
    f <- fit@formula_parsed$link_funcs[[j]]$func
    for (jj in symbols_so_far) {
      if (length(dim(out[[jj]])) > 1) {
        f <- nest_commas(f, jj)
      }
    }
    l <- sapply(1:n_cases, function(i) {
      the_env <- post
      the_env <- unlist(list(the_env, data, i = i), recursive = FALSE)
      if (j < n_links) {
        if (length(out) > 0) 
          for (k in 1:length(out)) {
            lv_name <- names(out)[k]
            the_env[[lv_name]] <- out[[k]]
          }
      }
      eval(f, envir = the_env)
    })
    if (!is.null(fit@formula_parsed$link_funcs[[j]]$inv_link)) {
      l <- do.call(fit@formula_parsed$link_funcs[[j]]$inv_link, 
                   list(l))
    }
    out[[names(fit@formula_parsed$link_funcs)[j]]] <- l
  }
  outn <- list()
  for (j in length(out):1) outn[[names(out)[j]]] <- out[[j]]
  out <- outn
  n_links <- length(symbols)
  if (flatten == TRUE && n_links == 1) 
    out <- out[[1]]
  return(out)
}




function (fit, data, n = 1000, ...) 
{
  .local <- function (fit, data, n = 1000, post, refresh = 0.1, 
                      replace = list(), flatten = TRUE, ...) 
  {
    ag <- attr(fit, "generation")
    if (!is.null(ag)) 
      if (ag == "ulam2018") 
        return(link_ulam(fit, data = data, post = post, 
                         n = n, simplify = flatten, ...))
    if (missing(post)) 
      post <- extract.samples(fit, n = n)
    if (missing(data)) {
      data <- fit@data
    }
    else {
    }
    if (length(replace) > 0) {
      for (i in 1:length(replace)) {
        post[[names(replace)[i]]] <- replace[[i]]
      }
    }
    lm <- fit@formula_parsed$lm
    lik <- fit@formula_parsed$lik
    n_lm <- length(lm)
    f_do_lm <- TRUE
    n_lik <- length(lik)
    lm_names <- c()
    lm_lik <- c()
    if (n_lm > 0) {
      for (i in 1:n_lm) {
        lm_names <- c(lm_names, lm[[i]]$parameter)
        if (n_lik > 0) {
          for (j in 1:n_lik) {
            if (lik[[j]]$N_name == lm[[i]]$N_name) {
              lm_lik <- c(lm_lik, lik[[j]]$likelihood)
            }
          }
        }
      }
    }
    else {
      stop("There appear to be no linear models here")
      n_lm <- 1
      f_do_lm <- FALSE
    }
    n_samples <- dim(post[[1]])[1]
    if (is.null(n_samples)) 
      n_samples <- length(post[[1]])
    if (n == 0) 
      n <- n_samples
    if (n_samples < n) 
      n <- n_samples
    lm_out <- list()
    liks <- fit@formula_parsed$likelihood
    n_cases_list <- list()
    for (i in 1:n_lm) {
      n_cases <- 1
      K <- 1
      N_name <- lm[[i]]$N_name
      outcome <- NA
      for (j in 1:length(liks)) {
        if (liks[[j]]$N_name == N_name) 
          outcome <- liks[[j]]$outcome
      }
      if (!is.na(outcome)) {
        n_cases <- length(data[[outcome]])
        if (n_cases > 0) {
          K <- max(fit@data[[outcome]])
          if (lm_lik[[i]] == "ordered_logistic") {
            the_cuts <- as.character(lik[[i]]$pars[[2]])
            if (!is.null(post[[the_cuts]])) {
              K <- dim(post[[the_cuts]])[2] + 1
            }
          }
        }
        else {
          n_cases <- length(data[[1]])
          data[[outcome]] <- rep(1, n_cases)
        }
      }
      n_cases_list[[i]] <- n_cases
      lm_out[[lm_names[i]]] <- array(0, dim = c(n, n_cases))
      if (lm_lik[[i]] == "ordered_logistic") {
        lm_out[[lm_names[i]]] <- array(0, dim = c(n, 
                                                  n_cases, K))
      }
    }
    if (TRUE) {
      idx_vars <- list()
      if (!is.null(fit@formula_parsed$vprior)) {
        if (length(fit@formula_parsed$vprior) > 0) 
          for (i in 1:length(fit@formula_parsed$vprior)) {
            idx_vars[[i]] <- fit@formula_parsed$vprior[[i]]$group
          }
      }
      if (length(idx_vars) > 0) {
        for (i in 1:length(idx_vars)) {
          do_fill <- FALSE
          the_idx <- idx_vars[[i]]
          the_effects <- fit@formula_parsed$vprior[[i]]$pars_out
          if (is.null(data[[the_idx]])) {
            message(concat("Index '", the_idx, 
                           "' not found in data. Assuming all corresponding varying effects equal to zero: ", 
                           the_effects))
            data[[the_idx]] <- rep(1, n_cases_list[[1]])
            do_fill <- TRUE
          }
          if (any(data[[the_idx]] == 0)) {
            the_dims <- length(data[[the_idx]])
            data[[the_idx]] <- rep(1, the_dims)
            do_fill <- TRUE
          }
          if (do_fill == TRUE) {
            for (j in 1:length(the_effects)) {
              the_dims <- dim(post[[the_effects[[j]]]])
              post[[the_effects[[j]]]] <- array(0, dim = the_dims)
            }
          }
        }
      }
    }
    init <- list()
    ref_inc <- floor(n * refresh)
    ref_next <- ref_inc
    rhs <- list()
    for (k in 1:n_lm) {
      if (f_do_lm == TRUE) {
        rhs0 <- fit@formula_parsed$lm[[k]]$RHS
        rhs0 <- gsub("[i]", "", rhs0, fixed = TRUE)
      }
      else {
        parout <- "ll"
        rhs0 <- "0"
        flik <- as.character(liks[[1]][[3]][[1]])
        if (flik == "dnorm") 
          rhs0 <- as.character(liks[[1]][[3]][[2]])
        if (flik == "dbinom") 
          rhs0 <- as.character(liks[[1]][[3]][[3]])
        if (flik == "dpois") 
          rhs0 <- as.character(liks[[1]][[3]][[2]])
      }
      if (length(fit@formula_parsed$impute_bank) > 0) {
        for (kk in 1:length(fit@formula_parsed$impute_bank)) {
          name_original <- names(fit@formula_parsed$impute_bank)[kk]
          name_merge <- concat(name_original, "_merge")
          n_cases <- length(data[[name_original]])
          var_merged <- matrix(NA, nrow = n, ncol = n_cases)
          name_impute <- concat(name_original, "_impute")
          missingness <- fit@formula_parsed$impute_bank[[kk]]$missingness
          for (a_case in 1:n_cases) {
            if (a_case %in% missingness) {
              idx <- which(missingness == a_case)
              if (length(missingness) > 1) 
                var_merged[, a_case] <- post[[name_impute]][, 
                                                            idx]
              else var_merged[, a_case] <- post[[name_impute]]
            }
            else {
              var_merged[, a_case] <- rep(data[[name_original]][a_case], 
                                          n)
            }
          }
          post[[name_merge]] <- var_merged
          init[[name_merge]] <- rep(0, n_cases)
        }
      }
      rhs[[k]] <- rhs0
    }
    for (i in 1:n) {
      if (refresh > 0) {
        if (i == ref_next) {
          msg <- paste("[", i, "/", n, "]\r", 
                       collapse = " ")
          cat(msg)
          ref_next <- i + ref_inc
          if (ref_next > n) 
            ref_next <- n
        }
      }
      for (j in 1:length(post)) {
        par_name <- names(post)[j]
        dims <- dim(post[[par_name]])
        if (length(dims) == 1) 
          init[[par_name]] <- post[[par_name]][i]
        if (length(dims) == 2) 
          init[[par_name]] <- post[[par_name]][i, ]
        if (length(dims) == 3) 
          init[[par_name]] <- post[[par_name]][i, , ]
      }
      for (k in n_lm:1) {
        e <- list(as.list(data), as.list(init))
        e <- unlist(e, recursive = FALSE)
        r <- eval(parse(text = rhs[[k]]), envir = e)
        flink <- fit@formula_parsed$lm[[k]]$link
        if (flink == "log") 
          r <- exp(r)
        if (flink == "logit") 
          r <- logistic(r)
        if (!is.null(lm_lik)) {
          if (lm_lik[k] == "ordered_logistic") {
            cuts <- init[["cutpoints"]]
            v <- predict_ordlogit(r, cuts)
            r <- v
          }
        }
        if (lm_lik[k] == "ordered_logistic") {
          lm_out[[lm_names[k]]][i, , ] <- r
        }
        else {
          lm_out[[lm_names[k]]][i, ] <- r
        }
        init[[lm_names[k]]] <- r
      }
    }
    if (refresh > 0) 
      cat("\n")
    if (flatten == TRUE) 
      if (length(lm_out) == 1) 
        lm_out <- lm_out[[1]]
    return(lm_out)
  }
  .local(fit, data, n, ...)
}