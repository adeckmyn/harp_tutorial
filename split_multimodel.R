split_multimodel <- function(.fcst, keep_full = TRUE, ...) {
  UseMethod("split_multimodel")
}

split_multimodel.default <- function(df, keep_full = TRUE, df_name) {
  sub_models <- unique(
    gsub(
      "_mbr[[:digit:]]+[[:graph::]]*$",
      "",
      grep("_mbr[[:digit:]]+", colnames(fcst$awesome_multimodel_eps), value = TRUE)
    )
  )
  if (length(sub_models) > 1) {
    get_df <- function(x) {
      cols <- grep(
        paste(sub_models[sub_models != x], collapse = "|"),
        colnames(df),
        value = TRUE,
        invert = TRUE
      )
      df[cols]
    }
    dfs <- lapply(sub_models, get_df)
    names(dfs) <- sub_models
    if (keep_full) {
      dfs[[df_name]] <- df
      dfs <- dfs[c(df_name, sub_models)]
    }
  } else {
    dfs <- list(df)
    names(dfs) <- df_name
  }
  structure(dfs, class = "harp_fcst")
}

split_multimodel.harp_fcst <- function(.fcst, keep_full = TRUE) {
  .fcst <- mapply(
    function(x, y, z) split_multimodel(x, keep_full = z, df_name = y),
    .fcst,
    names(.fcst),
    MoreArgs  = list(z = keep_full),
    SIMPLIFY  = FALSE,
    USE.NAMES = FALSE
  )
  Reduce(c, .fcst)
}
