# tibbleColumns
Helpful tidyverse functions that output tibbles

<b>How to install:</b>

library(devtools) <br>
install_github("nhemerson/tibbleColumns") <br>
library(tidyverse) <br>
library(tibbleColumns) 

#prop_column_group <br>
mtcars %>% prop_column_group(cyl)

#prop_column<br>
mtcars %>% count(cyl, mpg) %>% prop_column(n)

#change_XoX_column<br>
tc <- tibble(<br>
  Month = c("Jan", "Dec"),<br>
  Users = c(102, 909)<br>
)

tc %>% spread(Month, Users) %>% change_XoX_column(Dec, Jan, "MoM")

#change_XoX_column_group<br>
tcg <- tibble(<br>
  Month = c("Jan", "Jan","Dec", "Dec"),<br>
  Type = c("Red", "Blue", "Red", "Blue"),<br>
  Users = c(102, 909, 201, 479)<br>
)

tcg %>% select(Type, Month, Users) %>% change_XoX_column_group(Dec, Jan, "MoM")

#testoutput<br>
t1 <- tibble(<br>
    type = c("Blue", "Blue", "Blue", "Blue"),<br>
    num = runif(4,0,5)<br>
  )

t2 <- tibble(<br>
  type = c("Red","Red", "Red", "Red"),<br>
  num = runif(4,0,5)<br>
)

ttest_tibble(t1$num,t2$num)


#lm output<br>
mtcars %>% select(mpg,cyl,wt) %>% lm_summary_tibble(mpg)
