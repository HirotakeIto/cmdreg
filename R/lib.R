. = NULL

#' @param x object
#' @export
hello = function(x) print(x)

#' @param df data.frame
#' @param fm formula
#' @export
is_no_df_have_values = function(df, fm) df[all.vars(fm)] %>% stats::na.omit() %>% dim() %>% .[[1]] == 0

#' @param df data.frame
#' @param all_vars list of character
#' @export
is_no_df_have_values_vars = function(df, all_vars) df[all_vars] %>% stats::na.omit() %>% dim() %>% .[[1]] == 0

#' @param df data.frame
#' @param fm formula
#' @export
have_using_columns_completely = function(dfx, fm) is_no_df_have_values(df = dfx, fm = fm) == FALSE

#' @param df data.frame
#' @param fm formula
#' @export
have_all_columns_using = function(dfx, fm) min(all.vars(fm) %in% names(dfx)) == 1

#' @param df data.frame
#' @param fm formula
#' @export
can_do_analysis = function(dfx, fm) have_using_columns_completely(dfx, fm) & have_all_columns_using(dfx, fm)

#' @param x formula
#' @export
as.character.formula = function(x) Reduce( paste, deparse(x) )
