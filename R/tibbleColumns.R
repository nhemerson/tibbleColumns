#' Linear Model Summary Tibble
#'
#' This function spits out a lm summary
#' @param df,dep
#' @import tidyverse broom
#' @export
#' @examples 
#' mtcars %>% prop_column_group(cyl)

lm_summary_tibble <- function(df, dep){
  
  dep <- enquo(dep)
  
  l <- lm(paste0(quo_name(dep),"~."), data = df)
  ls <- summary(l)
  tidy(ls) %>% mutate(R2 = ls$r.squared)
}


#' A tidy t.test that allows to pass a tibble to it
#'
#' Allows to pass a tibble data_frame to the base R t.test 
#' function over two numeric columns. Then extracts the output 
#' statistics and outputs a tibble.
#' @param df1,df2
#' @import tidyverse
#' @export
#' @examples 
#' ttest_tibble(t1$num,t2$num)

ttest_tibble <- function(df1,df2){
  df1 <- as.data.frame(df1)
  df2 <- as.data.frame(df2)
  ttest <- t.test(df1, df2)
  
  tibble(
    t_stat = round(ttest[[1]],4),
    df = round(ttest[[2]],4),
    p_value = round(ttest[[3]],4),
    conf_int = paste(c(round(ttest$conf.int[[1]],4),round(ttest$conf.int[[2]],4)), collapse = " -> "),
    conf_level = attr(ttest$conf.int, "conf.level")
  )
}


#' Proportion Column by Group
#'
#' Groups one column, adds a column for count of each group 
#' and adds a column for proportion of total based on count
#' @param df,group
#' @export
#' @examples 
#' mtcars %>% prop_column_group(cyl)

prop_column_group <- function(df, group){
  group <- enquo(group)
  df %>% count(!!group) %>% rename(Count = n) %>% mutate(Percent = round(Count/sum(.$Count) *100,3))
}


#' General Proportion Column
#'
#' This function creates a proportion column
#' @param df,col
#' @export
#' @examples 
#' mtcars %>% count(cyl, disp) %>% arrange(desc(n)) %>% prop_column(n)

prop_column <- function(df, col) {
  col <- enquo(col)
  prop_col <- paste0("Perc_", quo_name(col))
  df %>% mutate(!!prop_col := round((!!col)/(sum(!!col))*100,3))
}


#' General Proportion Column
#'
#' This function creates a proportion column
#' @param df,col
#' @import tidyverse
#' @export
#' @examples 
#' mtcars %>% count(cyl, disp) %>% arrange(desc(n)) %>% prop_column(n)

prop_column <- function(df, col) {
  col <- enquo(col)
  prop_col <- paste0("Perc_", quo_name(col))
  df %>% mutate(!!prop_col := round((!!col)/(sum(!!col))*100,3))
}


#' General X over X change Column
#'
#' Creates a change column based on integer or numeric column
#' @param df,col1,col2,XoX
#' @import tidyverse
#' @export
#' @examples 
#' change_XoX_column(mtcars, drat, wt, "MoM")

change_XoX_column <- function(df, col1, col2, XoX) {
  col1 <- enquo(col1)
  col2 <- enquo(col2)
  
  xox_col <- paste0("Change_", quo_name(XoX))
  
  df %>% mutate(!!xox_col := round(((!!col2) - (!!col1))/(!!col1)*100,3))
}


#' General X over X Change Column by Group
#'
#' Creates a change column based on a group. This function is specific 
#' as the data must have three columns at most. A category group column 
#' group a/b, device type, segment, a calendar group month, year, day 
#' and a numeric column to aggregate users, visits, clicks etc.. The 
#' data columns MUST be in that order as well. Category Group, Calendar 
#' Group, Numeric Aggregate.
#' @param df,col1,col2,XoX
#' @import tidyverse
#' @export
#' @examples 
#' tb %>% select(Type, Month, Users) %>% change_XoX_column_group(Dec,Jan,"MoM")

change_XoX_column_group <- function(df, col1, col2, XoX){
  
  col1 <- enquo(col1)
  col2 <- enquo(col2)
  
  xox_col <- paste0("Change_", quo_name(XoX))
  
  df2 <- df %>% group_by_if(is.character) %>% summarise_if(is.numeric,sum)
  
  names(df2)[2:3] <- c("V1", "V2")
  
  df3 <- df2 %>% spread(V1, V2)
  
  names(df3)[2:3] <- c(quo_name(col1), quo_name(col2))
  
  df3 %>% select(!!col1, !!col2) %>% mutate(!!xox_col := ((!!col2) - (!!col1))/(!!col1)*100) %>% ungroup()
}
