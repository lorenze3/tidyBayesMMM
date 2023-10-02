function (fit, data, post, flatten = TRUE, symbols, ...) 
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