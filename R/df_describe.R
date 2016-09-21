#' @export
df.describe.factors <- function(df){
  df.names <- names(df)
  df.pos <- seq(from = 1, length.out = length(df.names))
  df.factor <- lapply(df, is.factor) %>% unlist()
  df.names.f <- df.names[df.factor]
  df.pos.f <- df.pos[df.factor]
  res <- list()
  if (length(df.names.f) > 0) {
    res <- lapply(
      1:length(df.names.f),
      function(idx) {
        col.pos <- df.pos.f[idx]
        levs <- levels(df[, col.pos])
        data.frame(
          col.name = df.names.f[idx],
          col.pos = col.pos,
          col.level = levs,
          col.level.idx = seq(from = 1, length.out = length(levs)),
          row.names = NULL,
          stringsAsFactors = FALSE
        )
      }
    )
  }

  res %>%
    dplyr::bind_rows()
}

#' @export
df.describe.basic <- function(df){
  df.names <- names(df)
  df.store <- lapply(df, storage.mode) %>% unlist()
  df.mode <- lapply(df, mode) %>% unlist()
  df.class <- lapply(df, function(x) paste(class(x), collapse = ", ")) %>% unlist()
  df.nlevels <- lapply(
    df,
    function(x) ifelse(is.factor(x), nlevels(x), 0)
  ) %>% unlist()
  #df.navals <- lapply(df, function(x) sum(is.na(x))) %>% unlist()
  res <- data.frame(
    col.name = df.names,
    col.pos = seq(from = 1, length.out = length(df.names)),
    R.storage.mode = df.store,
    R.mode = df.mode,
    R.class = df.class,
    nlevels = df.nlevels,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  res
}
