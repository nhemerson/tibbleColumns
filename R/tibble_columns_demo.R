#install.packages("devtools")
library(devtools)
install_github("nhemerson/tibbleColumns")
library(tidyverse)
library(tibbleColumns)

#prop_column_group
mtcars %>% prop_column_group(cyl)

#prop_column
mtcars %>% count(cyl, mpg) %>% prop_column(n)

#change_XoX_column
tc <- tibble(
  Month = c("Jan", "Dec"),
  Users = c(102, 909)
)

tc %>% spread(Month, Users) %>% change_XoX_column(Dec, Jan, "MoM")

#change_XoX_column_group
tcg <- tibble(
  Month = c("Jan", "Jan","Dec", "Dec"),
  Type = c("Red", "Blue", "Red", "Blue"),
  Users = c(102, 909, 201, 479)
)

tcg %>% select(Type, Month, Users) %>% change_XoX_column_group(Dec, Jan, "MoM")

#testoutput
t1 <- tibble(
    type = c("Blue", "Blue", "Blue", "Blue"),
    num = runif(4,0,5)
  )

t2 <- tibble(
  type = c("Red","Red", "Red", "Red"),
  num = runif(4,0,5)
)

ttest_tibble(t1$num,t2$num)


#lm output
mtcars %>% select(mpg,cyl,wt) %>% lm_summary_tibble(mpg)


#tibble_out
mtcars %>% group_by(cyl) %>% prop_column_group(cyl) %>% tibble_out("grouped") %>% filter(Count >9)
