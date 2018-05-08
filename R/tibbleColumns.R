#' Linear Model Summary Tibble
#'
#' This function returns a tidy tibble output of the most important parts
#' of a lm summary a la the broom package.
#' @param df,dep a dataframe and a dependent variable name
#' @import tidyverse broom
#' @export lm_summary_tibble
#' @examples
#' mtcars %>% select(mpg,cyl,wt) %>% lm_summary_tibble(mpg)

lm_summary_tibble <- function(df, dep){
  
  dep <- enquo(dep)
  
  l <- lm(paste0(quo_name(dep),"~."), data = df)
  ls <- summary(l)
  tidy(ls) %>% mutate(R2 = ls$r.squared)
}


#' A tidy t.test summary tibble
#'
#' Allows to pass a tibble data_frame to the base R t.test
#' function over two numeric columns. Then extracts the output
#' statistics and outputs a tibble.
#' @param df1,df2 two tibble dataframes
#' @import tidyverse
#' @export ttest_tibble
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
#' @param df,group a dataframe and a group column name
#' @export prop_column_group
#' @examples
#' mtcars %>% prop_column_group(cyl)

prop_column_group <- function(df, group){
  group <- enquo(group)
  df %>% count(!!group) %>% rename(Count = n) %>% mutate(Percent = round(Count/sum(.$Count) *100,3))
}


#' General Proportion Column
#'
#' This function creates a proportion column
#' @param df,col a data frame and a column name
#' @export prop_column
#' @examples
#' mtcars %>% count(cyl, disp) %>% arrange(desc(n)) %>% prop_column(n)

prop_column <- function(df, col) {
  col <- enquo(col)
  prop_col <- paste0("Perc_", quo_name(col))
  df %>% mutate(!!prop_col := round((!!col)/(sum(!!col))*100,3))
}


#' General Proportion Column
#'
#' This function creates a proportion column based on a column
#' specified.
#' @param df,col a data frame and a column name
#' @import tidyverse
#' @export prop_column
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
#' @param df,col1,col2,XoX a data frame two columnnames and XoX name for new column
#' @import tidyverse
#' @export change_XoX_column
#' @examples
#' change_XoX_column(mtcars, drat, wt, "MoM")

change_XoX_column <- function(df, col1, col2, XoX) {
  col1 <- enquo(col1)
  col2 <- enquo(col2)
  
  df <- select(df,!!col1,!!col2)
  
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
#' @param df,col1,col2,XoX a data frame two columnnames and XoX name for new column
#' @import tidyverse
#' @export change_XoX_column_group
#' @examples
#' tb %>% select(Type, Month, Users) %>% change_XoX_column_group(Dec,Jan,"MoM")

change_XoX_column_group <- function(df, col1, col2, XoX){
  
  col1 <- enquo(col1)
  col2 <- enquo(col2)
  
  xox_col <- paste0("Change_", quo_name(XoX))
  
  df2 <- df %>% group_by_if(is.character) %>% summarise_if(is.numeric,sum)
  
  names(df2)[2:3] <- c("V1", "V2")
  
  df3 <- df2 %>% spread(V1, V2)
  
  df3 %>% select(!!col1, !!col2) %>% mutate(!!xox_col := ((!!col2) - (!!col1))/(!!col1)*100) %>% ungroup()
}


#' Tibble a data frame state within a pipe series
#'
#' Create a tibble for the state of a data frame within a pipe series and
#' assign it as an object to the global environment.
#' @param df,name a data frame and a name for created tibble object
#' @param suppress prevents the function from creating the tibble in the parent environment
#' @import tidyverse
#' @export tibble_out
#' @examples
#' mtcars %>% group_by(cyl) %>% prop_column_group(cyl) %>% tibble_out("grouped") %>% filter(Count >9)

tibble_out <- function(df,name,suppress=FALSE){
  if (suppress){return(df)}
  nam <<- tbl_df(df)
  assign(name,nam,envir=.GlobalEnv)
  rm(nam, envir = .GlobalEnv)
  df
}

#' Shorter alias for tibble_out fucnction - Tibble a data frame state within a pipe series
#'
#' Create a tibble for the state of a data frame within a pipe series and 
#' assign it as an object to the global environment.
#' @param df,name a data frame and a name for created tibble object
#' @import tidyverse
#' @export tbl_out
#' @examples 
#' mtcars %>% group_by(cyl) %>% prop_column_group(cyl) %>% tbl_out("grouped") %>% filter(Count >9)

tbl_out <- function(df,name,suppress=FALSE){
  if (suppress){return(df)}
  nam <<- tbl_df(df)
  assign(name,nam,envir=.GlobalEnv)
  rm(nam, envir = .GlobalEnv)
  df
}


#' Run a function outside of the pipe sequence
#'
#' Run a function outside of the pipe sequence and create a tibble in global environment for it 
#' while passing previous state of data frame through to the next pipe step.
#' @param df,fun,name a data frame, a function to run and a name for created tibble object
#' @import tidyverse
#' @export tbl_module
#' @examples 
#' mtcars %>% tbl_out("cars") %>% tbl_module(filter(.,hp > 150),"fastCars") %>% tbl_lookup(cyl) %>% tbl_out("cylList")

tbl_module <- function(df,fun, name){
  nam <<- fun
  assign(name,nam,envir=.GlobalEnv)
  rm(nam, envir = .GlobalEnv)
  tbl_df(df)
}

#' Get a list of groups in certain column
#'
#' Designate a column and get a single column listing the groups in that column.
#' @param df,... a data frame and a column or columns to get group list
#' @import tidyverse
#' @export tbl_lookup
#' @examples 
#' mtcars %>% tbl_out("cars") %>% tbl_module(filter(.,hp > 150),"fastCars") %>% tbl_lookup(cyl) %>% tbl_out("cylList")

tbl_lookup <- function(df,...){
  group_var <- quos(...)
  df %>% count(!!!group_var) %>% select(-n)
}


#' Replace all na's in data frame with a zero
#'
#' Replace NA's with a zero for dataframes. Defaults with a 0 unless something else typed.
#' @param df,replace_with a data frame and what to replace with
#' @import tidyverse
#' @export replace_all_na
#' @examples 
#' mtcars %>% tbl_out("cars") %>% sjmisc::set_na(na = 0) %>% replace_all_na()

replace_all_na <- function(df,replace_with = NULL){
  if(is.null(replace_with)){
    df[is.na(df)] <- 0
    df
  }else{
    df[is.na(df)] <- replace_with
    df
  }
}


#' New binary numeric column mirroring logical column
#'
#' Look at a logi cal TRUE FALSE column and make new column with binary represenation
#' @param df,col a data frame and logical TRUE FALSE colum to change to binary
#' @import tidyverse
#' @export bool_to_binary
#' @examples 
#' iris %>% mutate(Setosa = str_detect(.$Species, "setosa")) %>% bool_to_binary(.,Setosa)

bool_to_binary <- function(df,col){
col <- enquo(col)
#col <- paste0(quo_name(col))

col2 <- paste0(quo_name(col),"_Binary")

df %>% mutate(!!col2 := case_when(
  !!col == TRUE ~ 1,
  !!col == FALSE ~ 0
))
} 
