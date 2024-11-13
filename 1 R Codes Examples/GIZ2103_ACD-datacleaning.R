#' ---
#' title: "Quest data"
#' subtitle: "Assesment of capacity development of partner institutions (PME)"
#' author: ""
#' date: "May 2023"
#' output:
#'    html_document:
#'      number_sections: true
#'      toc: true
#'      theme: simplex
#'      toc_float: yes
#' ---

#' # Script preparation

#' Set options
knitr::opts_chunk$set(warning=FALSE, message=FALSE,  echo=FALSE)
options(scipen = 999)
options(java.parameters = "-Xmx4096m")
options(knitr.table.format = "html")

#' Function
create_var <- function(...) {
  vars <- list(...)
  vars[is.na(vars)] <- 0
  options <- c()
  for (i in 1:length(vars)) {
    if (vars[[i]] == 1) {
      options <- c(options, paste0("Option_", i))
    }
  }
  if (length(options) == 0) {
    return(NA)
  } else {
    return(paste(options, collapse = " + "))
  }
}

#' R libraries
library(tidyverse)
library(openxlsx)
library(kableExtra)
library(knitr)

#' Labels to format with
cbook <- read.xlsx("../Input/GIZ2103_Codebook_assesment.xlsx", sheet = 3) 
var.names <- cbook %>% select(Variable.eng, Variable.fr, Variable.cln) %>% unique
categories.val <- cbook %>% select(Variable.cln, Value.eng, Value.fr, Value.cln, Label.cln) %>% unique
questions <- cbook %>% select(Variable.cln, Question) %>% na.omit

q1.lbl <- categories.val %>% filter(Variable.cln == "Q1_Gender") %>% select(-Variable.cln, -Value.eng, -Value.fr)
q2.lbl <- categories.val %>% filter(Variable.cln == "Q2_Country") %>% select(-Variable.cln, -Value.eng, -Value.fr)
q3.lbl <- categories.val %>% filter(Variable.cln == "Q3_Institution") %>% select(-Variable.cln, -Value.eng, -Value.fr)
q5.lbl <- categories.val %>% filter(Variable.cln == "Q5_1") %>% select(-Variable.cln, -Value.eng, -Value.fr)
q6_10.lbl <- categories.val %>% filter(Variable.cln == "Q6_1") %>% select(-Variable.cln,  -Value.eng, -Value.fr)

#' 
#' # Data Loading and Cleaning (ENG)
quest <- read.xlsx("../Data/rawData_eng.xlsx")
names(quest) <- plyr::mapvalues(names(quest), var.names$Variable.eng, var.names$Variable.cln)

NA.col <- colnames(quest)[!(colSums(is.na(quest)) < nrow(quest))]
quest$index[is.na(quest$Q1_Gender)]

vars <- names(quest)[grepl("Q1|Q2|Q3|Q5|Q6|Q7|Q8|Q9|Q10", names(quest)) & !grepl("_Observation|_Overall|_Other", names(quest))]

data <- quest %>%
          filter(index != 15 ) %>% 
          select(!all_of(NA.col)) %>% 
            mutate_all(., .funs = ~  plyr::mapvalues(., categories.val$Value.eng, categories.val$Value.cln)) %>% 
            mutate_at(.vars = vars[grepl("Q1_", vars)], .funs = ~ factor(., level = q1.lbl$Value.cln, q1.lbl$Label.cln))  %>% 
            mutate_at(.vars = vars[grepl("Q2_", vars)], .funs = ~ factor(., level = q2.lbl$Value.cln, q2.lbl$Label.cln))  %>% 
            mutate_at(.vars = vars[grepl("Q3_", vars)], .funs = ~ factor(., level = q3.lbl$Value.cln, q3.lbl$Label.cln))  %>% 
            mutate_at(.vars = vars[grepl("Q6|Q7|Q8|Q9|Q10_", vars)], .funs = ~ factor(., level = q6_10.lbl$Value.cln, q6_10.lbl$Label.cln))  %>% 
            mutate(leng = "ENG")
            
#' 
#' # Data Loading and Cleaning (fr)
quest <- read.xlsx("../Data/rawData_fr.xlsx")
names(quest) <- plyr::mapvalues(names(quest), var.names$Variable.fr, var.names$Variable.cln)

quest$index[is.na(quest$Q1_Gender)]

vars <- names(quest)[grepl("Q1|Q2|Q3|Q5|Q6|Q7|Q8|Q9|Q10", names(quest)) & !grepl("_Observation|_Overall|_Other", names(quest))]

df <- quest %>%
        filter(Q0_Consent != "non" ) %>%
        select(!all_of(NA.col)) %>% 
          mutate_all(., .funs = ~  plyr::mapvalues(., categories.val$Value.fr, categories.val$Value.cln)) %>% 
          mutate_at(.vars = vars[grepl("Q1_", vars)], .funs = ~ factor(., level = q1.lbl$Value.cln, q1.lbl$Label.cln))  %>% 
          mutate_at(.vars = vars[grepl("Q2_", vars)], .funs = ~ factor(., level = q2.lbl$Value.cln, q2.lbl$Label.cln))  %>% 
          mutate_at(.vars = vars[grepl("Q3_", vars)], .funs = ~ factor(., level = q3.lbl$Value.cln, q3.lbl$Label.cln))  %>% 
          mutate_at(.vars = vars[grepl("Q6|Q7|Q8|Q9|Q10_", vars)], .funs = ~ factor(., level = q6_10.lbl$Value.cln, q6_10.lbl$Label.cln))  %>% 
          mutate(leng = "FR")

data <- rbind(data, df) %>% 
          mutate(Q4_Category = mapply(create_var, Q4_option_1, Q4_option_2, Q4_option_3),
                 Q4_1_Service =  mapply(create_var, Q4_1_1, Q4_1_2, Q4_1_3, Q4_1_4, Q4_1_5, Q4_1_6, Q4_1_7),
                 Q4_2_Service =  mapply(create_var, Q4_2_1, Q4_2_2, Q4_2_3, Q4_2_4, Q4_2_5, Q4_2_6, Q4_2_7),
                 Q4_3_Service =  mapply(create_var, Q4_3_1, Q4_3_2, Q4_3_3, Q4_3_4, Q4_3_5))

write.xlsx(data, "../Data/GIZ2301_Clean-Data_v0.3.xlsx")

#' 
#' # Validation rules
df <- data %>% 
          mutate(Q4_1_7 = ifelse(Q4_option_1 == 1 & rowSums(data[, names(data)[grepl("Q4_1", names(data)) & !grepl("_Service|_Other", names(data))]]) == 0, 1, Q4_1_7 ),
                 Q4_option_1 = ifelse(Q4_option_1 == 0 & rowSums(data[, names(data)[grepl("Q4_1", names(data)) & !grepl("_Service|_Other", names(data))]]) > 0, 1, Q4_option_1),
                 Q4_2_7 = ifelse(Q4_option_2 == 1 & rowSums(data[, names(data)[grepl("Q4_2", names(data)) & !grepl("_Service|_Other", names(data))]]) == 0, 1, Q4_2_7 ),
                 Q4_option_2 = ifelse(Q4_option_2 == 0 & rowSums(data[, names(data)[grepl("Q4_2", names(data)) & !grepl("_Service|_Other", names(data))]]) > 0, 1, Q4_option_2),
                 Q4_3_5 = ifelse(Q4_option_3 == 1 & rowSums(data[, names(data)[grepl("Q4_3", names(data)) & !grepl("_Service|_Other", names(data))]]) == 0, 1, Q4_3_5 ),
                 Q4_option_3 = ifelse(Q4_option_3 == 0 & rowSums(data[, names(data)[grepl("Q4_3", names(data)) & !grepl("_Service|_Other", names(data))]]) > 0, 1, Q4_option_3)) %>% 
          mutate_at(.vars = names(data)[grepl("Q4_o", names(data))], .funs =  ~ ifelse(is.na(.), 0, .)) %>% 
          filter(!((Q4_option_1 == 1 & is.na(Q5_1))| (Q4_option_2 == 1 & is.na(Q5_2))| (Q4_option_3 == 1 & is.na(Q5_3)))) %>% 
          mutate(Q5_1 = ifelse(!is.na(Q5_1) & is.na(Q4_option_1)|Q4_option_1 == 0, NA, Q5_1),
                 Q5_2 = ifelse(!is.na(Q5_2) & (is.na(Q4_option_2)|Q4_option_2 == 0), NA, Q5_2),
                 Q5_3 = ifelse(!is.na(Q5_3) & (is.na(Q4_option_3)|Q4_option_3 == 0 ), NA, Q5_3)) 

write.xlsx(df, "../Data/GIZ2301_Validated-Data_v1.0.xlsx")
 