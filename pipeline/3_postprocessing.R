library(dplyr)
library(readr)
library(magrittr)
library(stringr)
# library(forcats)


question_types <- c(36,8,11,21,27)

is_not_empty <- function(string) {
  if(is.na(string) | string == "" | string == ".") {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

is_not_empty = Vectorize(is_not_empty)

col_names <- c("Group ID", "ID", 
               "FF", "First Name", 
               "Last Name", "LF",
               "DoB", "Sex", "Race",
               "Reg No.", "First Name", "Last Name",
               "DoB", "Sex", "Race",
               "Record ID", "type","Same")

#[Group ID, Reg No., FF, First Name, Last Name, LF, 
# DoB, Race, Reg No., First Name, Last Name, 
#DoB, Race, Record ID, type, Answer]


(starred_data <- read_csv("./data_intermediate/all_starred_race.csv", 
                          col_types = cols(.default = "c", `Group ID` = "i")) %>%
  mutate_all(funs(ifelse(is_not_empty(.),.,""))) %>%
    mutate(src = str_extract(`Record ID`, "[AB]")) %>%
      rename(fname = `First Name`,
             lname = `Last Name`))

# (fname_freq <- read_csv("./frequencies/fname_freq.csv"))
# (lname_freq <- read_csv("./frequencies/lname_freq.csv"))

# starred_data <- 
#   starred_data %>%
#     left_join(fname_freq, by = c("fname","src")) %>%
#       mutate(FF = ifelse(!is.na(n),n,1)) %>%
#         select(-n)
# 
# starred_data <- 
#   starred_data %>%
#     left_join(lname_freq, by = c("lname","src")) %>%
#     mutate(LF = ifelse(!is.na(n),n,1)) %>%
#     select(-n)

(starred_data <- 
  starred_data %>%
    mutate(`Record ID` = str_extract(`Record ID`,"[0-9]+")))

starred_data <- 
  starred_data %>%
  select(`Group ID`, `Reg No.`, FF, fname, lname, LF,
         DoB,Sex, Race, `Reg No._1`, `First Name_1`, `Last Name_1`, DoB_1,Sex_1, Race_1, 
         `Record ID`, type, Same) %>%
  arrange(as.numeric(`Group ID`))

starred_data %>%
  select(`Group ID`,type,`Record ID`) %>%
  group_by(type,`Record ID`) %>%
  unique() %>% count(type,`Record ID`)

attention_test = c(1,7,13,19,25,31)
# choose 6 questions
starred_data_2 <-
  starred_data%>%
   filter(as.numeric(`Record ID`)  %in% question_types)

page_numbers = c(1,2,3,4,5)

starred_data_2 <-
starred_data_2 %>%
  group_by(`Record ID`) %>% 
  mutate(n = sort(rep(1:(n()/2),2))) %>% select(n, everything()) %>% 
  ungroup() %>% arrange(n , `Group ID`)  %>%
  filter(!(n>5))
 # filter(`n` %in% page_numbers )



randomize_table <- function(ord_tbl, sampling = T){
  if(sampling){
    gid <- ord_tbl %>% pull(`Group ID`) %>% unique() %>% base::sample(size = length(.), replace = F)
  } else {
    gid <- ord_tbl %>% pull(`Group ID`) %>% unique()
  }
  lookup <- tibble(`Group ID` = gid, n_ord = 1:length(gid))
  if(sampling){
    ord_tbl %>% left_join(lookup) %>% arrange(n_ord) %>% select(-n_ord)
  } else {
    ord_tbl %>% left_join(lookup) %>% mutate(`Group ID` = n_ord) %>% select(-n_ord)
  }
  
}

set.seed(1)

starred_data_2 <- 
starred_data_2 %>% 
  group_by(n) %>% 
  do(randomize_table(.)) %>%
  ungroup() %>% 
  select(-n) 


starred_data_2 <- randomize_table(starred_data_2, F)

# %>% %T>% View()
#sample_n(1) %$%
#  Group ID


# mutate(n = sort(rep(1:(n()/2),2))) %>% select(n, everything()) %>% 

#(unordered_questions <- 
#    starred_data_2 %>%
#    sample_n(.,size = nrow(.)) %$%
#    
#%>%
#    unique())


#make the names standard

names(starred_data_2) <- col_names
starred_data_2%>%
  write_csv(paste0(sprintf("./data_output/samples/sample_%02d",1),".csv"))

