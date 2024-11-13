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

#' Script preparation

#' Set options
knitr::opts_chunk$set(warning=FALSE, message=FALSE,  echo=FALSE)
options(scipen = 999)
options(java.parameters = "-Xmx4096m")
options(knitr.table.format = "html")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#' R libraries
library(tidyverse)
library(openxlsx)
library(kableExtra)
library(knitr)

#' Functions
move_to_last <- function(df, n) df[c(setdiff(seq_len(nrow(df)), n), n), ]

freq.table <- function(df, break.var){
  aux <- df %>% 
    group_by_at(break.var) %>%
    summarise(N = n()) %>%  
    mutate(P = scales::percent(N/sum(N), accuracy = 0.1))
  aux <- move_to_last(aux, which(is.na(aux[,1])))
  
  return(kbl(aux %>%
               arrange(desc(N))) %>%
           kable_styling(bootstrap_options = c( "hover", "condensed", "striped")))
}

tables.multiple <- function(df, Q, brk.var = NULL){
  vars <- names(data)[grepl(Q, names(data))]
  categories <- as.vector(unique(df[, brk.var]))
  cat( paste0("<br> **", questions$Variable.cln[questions$Variable.cln == questions$Variable.cln[grepl(paste0(Q, "_Overall"), questions$Variable.cln)]], " - ", questions$Question[questions$Variable.cln == questions$Variable.cln[grepl(paste0(Q, "_Overall"), questions$Variable.cln)]], "** <br> \n<br>") )
  
  if(Q == "Q5"){ lbl <- c(q5.lbl$Label.cln, NA)
  } else {lbl <- c(q6_10.lbl$Label.cln, NA)}
  
  aux <- df %>% 
    { if (is.null(brk.var)) select(.,vars) else select(.,c(brk.var, vars))} %>% 
    gather_(Q, "answer", paste0( Q,"_", seq(1,ifelse(is.null(brk.var), length(colnames(.)), length(colnames(.))-1) ,1))) %>%
    { if (is.null(brk.var)) group_by_at(.,.vars = c(Q,"answer")) else group_by_at(.,.vars = c(Q,"answer", brk.var))} %>% 
    summarise(N = n()) %>% ungroup %>% mutate(answer = factor(answer, level = lbl[!is.na(lbl)])) %>% complete_(., c(Q, "answer", brk.var)) %>%
    { if (is.null(brk.var)) group_by_at(.,.vars = c(Q)) else group_by_at(.,.vars = c(Q, brk.var))} %>% 
    mutate(P = scales::percent(N/sum(N, na.rm = T), accuracy = 0.1)) %>% 
    select(-N) %>% 
    { if (is.null(brk.var)) spread(.,answer, P) %>% select_at(c(Q, lbl[!is.na(lbl)], colnames(.)[grepl("<NA>", colnames(.))] ))
      else unite(.,variable, c(brk.var, answer), sep = " ") %>%
        spread(variable , P) %>% 
        select_at(c(Q, paste0(categories, rep(paste0(" ",lbl), each = length(categories)))[paste0(categories, rep(paste0(" ",lbl), each = length(categories))) %in% colnames(.)  ]   )) } 
  aux[is.na(aux)]  <- "0%"
  
  
  vec <-  questions$Variable.cln %in% (aux[,1] %>% as.data.frame() %>% t() %>% as.vector())
  tags <- questions[vec,]
  aux <- aux  %>% mutate_at(.vars = 1, .funs = ~plyr::mapvalues(.,tags$Variable.cln, tags$Question))
  
  if (is.null(brk.var)) {
    print(kbl(aux, col.names = c(Q, lbl)) %>% kable_styling(bootstrap_options = c("hover", "condensed", "striped")))
  }else{
    for (i in categories[!is.na(categories)]) {
      
      kw0 <- i
      myHeader <- c(" " = 1, kw0 = length(lbl))
      names(myHeader) <- c(" ", kw0)
      
      if (kw0 != "No") {
        print(kbl(aux %>% select_at(.vars = names(aux)[grepl(i, names(aux))]), col.names = c(Q, lbl)) %>%
                add_header_above(header = myHeader) %>% 
                kable_styling(bootstrap_options = c("hover", "condensed", "striped")))
        cat("<br> \n<br>")
        
      }
    }
  }
}

Q4.tables <- function(df, Q, brk.var = NULL){
  vars <- names(df)[grepl(Q, names(df))]
  if (Q == "Q4_o") {
    cat( paste0("<br> **", questions$Variable.cln[questions$Variable.cln == questions$Variable.cln[grepl(paste0("Q4", "_Category"), questions$Variable.cln)]], " - ", questions$Question[questions$Variable.cln == questions$Variable.cln[grepl(paste0("Q4", "_Category"), questions$Variable.cln)]], "** <br> \n<br>") )
  }else{
    cat( paste0("<br> **", questions$Variable.cln[questions$Variable.cln == questions$Variable.cln[grepl(paste0("Q4_option", str_sub(Q, 3, nchar(Q))), questions$Variable.cln)]], " - ", questions$Question[questions$Variable.cln == questions$Variable.cln[grepl(paste0("Q4_option", str_sub(Q, 3, nchar(Q))), questions$Variable.cln)]], "** <br> \n<br>") )
  }
  
  if (is.null(brk.var)) {
    aux <- data %>%
      select_at(vars) %>% 
      gather("Q4", "answer") %>% 
      group_by(Q4) %>% filter(answer == "Yes") %>% 
      summarise(N = n()) %>% 
      mutate(P = scales::percent(N/nrow(data), accuracy = 0.1))
    
  }else{
    totals <- table(df[,brk.var]) %>% as.data.frame()
    names(totals) <- c(brk.var, "total")
    
    aux <- df %>%
      select_at(c(brk.var, vars)) %>% 
      gather("Q4", "answer", -brk.var) %>% 
      group_by_("Q4", brk.var ) %>% filter(answer == "Yes") %>% 
      summarise(N = n()) %>%  group_by_(brk.var ) %>% left_join(totals, brk.var) %>% 
      mutate(P = scales::percent(N/total, accuracy = 0.1)) %>% select(-N, -total) %>% 
      spread(brk.var, P)
    
    if(sum(grepl("NA", colnames(aux))) == 1){ 
      aux <- aux %>% select_at(., vars(!contains('<NA>')))}
  }
  aux[is.na(aux)]  <- "0%"
  
  vec <-  questions$Variable.cln %in% (aux[,1] %>% as.data.frame() %>% t() %>% as.vector())
  tags <- questions[vec,]
  
    print(kbl(aux %>% mutate_at(.vars = 1, .funs = ~plyr::mapvalues(.,tags$Variable.cln, tags$Question))) %>%
          kable_styling(bootstrap_options = c("hover", "condensed", "striped")))
}

#' Labels and categories
cbook <- read.xlsx("../Input/GIZ2103_Codebook_assesment.xlsx", sheet = 3) 
var.names <- cbook %>% select(Variable.eng, Variable.fr, Variable.cln) %>% unique
categories.val <- cbook %>% select(Variable.cln, Value.eng, Value.fr, Value.cln, Label.cln) %>% unique()
questions <- cbook %>% select(Variable.cln, Question) %>% na.omit

q1.lbl <- categories.val %>% filter(Variable.cln == "Q1_Gender") %>% select(-Variable.cln, -Value.eng, -Value.fr)
q2.lbl <- categories.val %>% filter(Variable.cln == "Q2_Country") %>% select(-Variable.cln, -Value.eng, -Value.fr)
q3.lbl <- categories.val %>% filter(Variable.cln == "Q3_Institution") %>% select(-Variable.cln, -Value.eng, -Value.fr)
q4.lbl <- categories.val %>% filter(Variable.cln == "Q4_option_1") %>% select(-Variable.cln, -Value.eng, -Value.fr)
q5.lbl <- categories.val %>% filter(Variable.cln == "Q5_1") %>% select(-Variable.cln, -Value.eng, -Value.fr)
q6_10.lbl <- categories.val %>% filter(Variable.cln == "Q6_1") %>% select(-Variable.cln,  -Value.eng, -Value.fr)


#' Load data
quest <- read.xlsx("../Data/GIZ2301_Validated-Data_v1.0.xlsx") 
vars <- names(quest)[grepl("id+$|Q1|Q2|Q3|Q4|Q5|Q6|Q7|Q8|Q9|Q10", names(quest)) & !grepl("_Other|_Observation|_Service|_Category", names(quest))]

data <- quest %>% select(all_of(vars)) %>% 
  mutate_at(.vars = vars[grepl("Q1_", vars)], .funs = ~ factor(., level = q1.lbl$Label.cln[1:2], q1.lbl$Label.cln[1:2]))  %>% 
  mutate_at(.vars = vars[grepl("Q2_", vars)], .funs = ~ factor(., level = q2.lbl$Label.cln, q2.lbl$Label.cln))  %>% 
  mutate_at(.vars = vars[grepl("Q3_", vars)], .funs = ~ factor(., level = q3.lbl$Label.cln, q3.lbl$Label.cln))  %>% 
  mutate_at(.vars = vars[grepl("Q4_", vars)], .funs = ~ factor(., level = q4.lbl$Value.cln, q4.lbl$Label.cln))  %>% 
  mutate_at(.vars = vars[grepl("Q5_", vars)], .funs = ~ factor(., level = q5.lbl$Label.cln, q5.lbl$Label.cln))  %>% 
  mutate_at(.vars = vars[grepl("Q6|Q7|Q8|Q9|Q10_", vars)], .funs = ~ factor(., level = q6_10.lbl$Label.cln, q6_10.lbl$Label.cln)) 


#' # General Tables
#' 
#' ## Basic information
#+ results = 'asis'
for (i in names(data)[grepl("Q1_|Q2_|Q3_", names(data))]) {
  cat( paste0("<br> **",i, " - ", questions$Question[questions$Variable.cln == i]), "** <br> \n<br>")
  print(freq.table(data, i))
  cat("<br> \n<br>")
  
}

#' ## Type of capacity development measure
#+ results = 'asis'
vars <- c("Q4_o", "Q4_1", "Q4_2", "Q4_3")
for (i in vars) {
  print(Q4.tables(data, i))
  cat("<br> \n<br>")
}

#' ## Relevancy and effectiveness of capacity development measures
#+ results = 'asis'
vars <- paste0( "Q", seq(5,10,1))
for (i in vars) {
  print(tables.multiple(data, i))
  cat("<br> \n<br>")
  
}

#' # Breakdown by gender
#' ## Type of capacity development measure
#' 
#+ results = 'asis'
vars <- c("Q4_o", "Q4_1", "Q4_2", "Q4_3")
for (i in vars) {
  print(Q4.tables(data, i, "Q1_Gender"))
  cat("<br> \n<br>")
}

#' ## Relevancy and effectiveness of capacity development measures
#' 
#+ results = 'asis'
vars <- paste0( "Q", seq(5,10,1))
for (i in vars) {
  print(tables.multiple(data, i, "Q1_Gender"))
  cat("<br> \n<br>")
}

#' # Breakdown by Country
#' ## Type of capacity development measure
#' 
#+ results = 'asis'
vars <- c("Q4_o", "Q4_1", "Q4_2", "Q4_3")
for (i in vars) {
  print(Q4.tables(data, i, "Q2_Country"))
  cat("<br> \n<br>")
}

#' ## Relevancy and effectiveness of capacity development measures
#' 
#+ results = 'asis'
vars <- paste0( "Q", seq(5,10,1))
for (i in vars) {
  print(tables.multiple(data, i, "Q2_Country"))
  cat("<br> \n<br>")
}

#' # Breakdown by Institution
#' ## Type of capacity development measure
#' 
#+ results = 'asis'
vars <- c("Q4_o", "Q4_1", "Q4_2", "Q4_3")
for (i in vars) {
  print(Q4.tables(data, i, "Q3_Institution"))
  cat("<br> \n<br>")
}

#' ## Relevancy and effectiveness of capacity development measures
#' 
#+ results = 'asis'
vars <- paste0( "Q", seq(5,10,1))
for (i in vars) {
  print(tables.multiple(data, i, "Q3_Institution"))
  cat("<br> \n<br>")
}

#' # Breakdown by Measures
#' ## Relevancy and effectiveness of capacity development measures
#' ### Q4_option_1
#+ results = 'asis'
vars <- paste0( "Q", seq(5,10,1))
for (i in vars) {
  print(tables.multiple(data, i, "Q4_option_1"))
  cat("<br> \n<br>")
}

#' ### Q4_option_2
#+ results = 'asis'
vars <- paste0( "Q", seq(5,10,1))
for (i in vars) {
  print(tables.multiple(data, i, "Q4_option_2"))
  cat("<br> \n<br>")
}

#' ### Q4_option_3
#+ results = 'asis'
vars <- paste0( "Q", seq(5,10,1))
for (i in vars) {
  print(tables.multiple(data, i, "Q4_option_3"))
  cat("<br> \n<br>")
}

