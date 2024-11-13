#' ---
#' title: "Tracer Studies"
#' subtitle: "GIZ2103"
#' author: "DevStat"
#' date: "Jul 2023"
#' output:
#'    html_document:
#'      number_sections: true
#'      toc: true
#'      theme: simplex
#'      toc_float: yes
#' ---

#' 
#' # Script options
#' 

#' Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(haven)
library(knitr)
library(kableExtra)  
library(ggh4x)
library(countrycode)
library(ggimage)
library(ggborderline)
library(gmodels)

doc.ver <- "v1.5"

data.folder <- "../Data/"

L.Migration <- c("Local Population", "Migrants from GER", "Migrants from third countries")
L.Gender <- c("Female","Male")

ccPalette <- c("#fdc400", "#f6b859", "#009ac2", "#83c8d1", "#ffdd00", "#f8e946", "#89ae10", "#bfd688")

not.valid <- c()
#not.valid <- c("IQ376")

countries.without.jobfair <- c("SB")

#' # Functions

freq.tables.by <- function(df, by.var){
  aux <- df %>% group_by_at(by.var) %>% summarise(N = n()) %>% mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ",")) %>% arrange(desc(N))
  aux2 <- df %>% ungroup() %>% summarise(N = n()) %>% mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ","))
  aux2[,by.var] <- "Total"
  return(aux %>% rbind(aux2))
}

freq.tables.byVar1 <- function(df, by.var){
  aux <- df %>% group_by_at(c(by.var[2:length(by.var)], by.var[1])) %>% summarise(N = n()) %>% 
                  mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ","))
  aux2 <- df %>% group_by_at(c(by.var[2:length(by.var)])) %>% summarise(N = n()) %>% 
                 group_by_at(c(by.var[2:length(by.var)])) %>%  
                  mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ","))
  aux2[,by.var[1]] <- "Total"
  aux <- aux %>% rbind(aux2) %>% gather(key, value, N:P) %>% unite(variable,c(by.var[1],"key"), sep = " ") %>% spread(variable,value)
  return(aux)
}

freq.tables.byVar1.vertical <- function(df, by.var){
  df[,by.var[1]] <- as.character(df[,by.var[1]])
  df[,by.var[2]] <- as.character(df[,by.var[2]])
  aux <- df %>% group_by_at(c(by.var[2:length(by.var)], by.var[1])) %>% summarise(N = n()) %>% group_by_at(by.var[1]) %>% 
    mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ","))
  aux2 <- df %>% group_by_at(by.var[2:length(by.var)]) %>% summarise(N = n()) %>% 
    mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ","))
  aux2[,by.var[1]] <- "Total"  
  aux3 <- aux %>% rbind(aux2) %>% group_by_at(c(by.var[1])) %>% summarise(N = sum(N)) %>% 
    group_by_at(c(by.var[1])) %>%  
    mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ","))
  aux3[,by.var[2]] <- "Total"
  aux <- aux %>% rbind(aux2) %>% rbind(aux3) %>% gather(key, value, N:P) %>% unite(variable,c(by.var[1],"key"), sep = " ") %>% spread(variable,value)
  return(aux)
}

head.byGender <- function(t, by.var){
  kable(t, col.names = c(by.var[2],rep(c("n","%"),3)), align = c("l", rep("r",6)), format.args = list(decimal.mark = ',', big.mark = ".")) %>%
  add_header_above(c("", "Female" = 2, "Male" = 2, "Total" = 2)) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
}


freq.tables.byVar1and2 <- function(df, by.var){
  aux <- df %>% group_by_at(c(by.var[1],by.var[3:length(by.var)], by.var[2])) %>% summarise(N = n()) %>%  mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ","))
  aux2 <- df %>% group_by_at(c(by.var[1],by.var[3:length(by.var)])) %>% summarise(N = n()) %>% 
            group_by_at(c(by.var[1],by.var[3:length(by.var)])) %>%  
            mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ","))
  aux2[,by.var[2]] <- "Total"
  aux3 <- df %>% group_by_at(c(by.var[3:length(by.var)], by.var[2])) %>% summarise(N = n()) %>%  mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ","))
  aux3[,by.var[1]] <- "Total"
  aux4 <- df %>% group_by_at(c(by.var[3:length(by.var)])) %>% summarise(N = n()) %>% group_by_at(c(by.var[3:length(by.var)])) %>%  mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ","))
  aux4[,by.var[1]] <- "Total"
  aux4[,by.var[2]] <- "Total"
  aux <- aux %>% rbind(aux2) %>% rbind(aux3) %>% rbind(aux4) %>% ungroup() 
  aux <- aux %>% mutate(Gender = factor(Gender, levels = c(L.Gender, "Total")), Migration = factor(Migration, levels = c(L.Migration, "Total"))) %>% complete( !!! rlang::syms(as.list(by.var)))
  aux <- aux %>% gather(key, value, N:P) %>% unite(variable,c(by.var[1],by.var[2],"key"), sep = " ") %>% spread(variable,value)
  return(aux)
}

freq.tables.byVar1and2.vertical <- function(df, by.var){
  aux <- df %>% group_by_at(c(by.var[1],by.var[3:length(by.var)], by.var[2])) %>% summarise(N = n()) %>%  
                group_by_at(c(by.var[1],by.var[2])) %>% mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ","))
  aux2 <- df %>% group_by_at(c(by.var[1],by.var[3:length(by.var)])) %>% summarise(N = n()) %>% 
                 group_by_at(c(by.var[1])) %>% mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ","))
  aux2[,by.var[2]] <- "Total"
  aux3 <- df %>% group_by_at(c(by.var[3:length(by.var)], by.var[2])) %>% summarise(N = n()) %>%  group_by_at(c(by.var[2])) %>% mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ","))
  aux3[,by.var[1]] <- "Total"
  aux4 <- df %>% group_by_at(c(by.var[3:length(by.var)])) %>% summarise(N = n()) %>% ungroup() %>%  mutate(P = scales::percent(N/sum(N), accuracy = 0.1, decimal.mark = ","))
  aux4[,by.var[1]] <- "Total"
  aux4[,by.var[2]] <- "Total"
  aux <- aux %>% rbind(aux2) %>% rbind(aux3) %>% rbind(aux4) %>% ungroup() 
  aux <- aux %>% mutate(Gender = factor(Gender, levels = c(L.Gender, "Total")), Migration = factor(Migration, levels = c(L.Migration, "Total"))) %>% complete( !!! rlang::syms(as.list(by.var)))
  aux <- aux %>% gather(key, value, N:P) %>% unite(variable,c(by.var[1],by.var[2],"key"), sep = " ") %>% spread(variable,value)
  return(aux)
}


head.freq.byMigrationandGender <- function(t){
  k <- kbl(t, col.names = c("",rep(c("n","%"),12)), align = c("l", rep("r",18)), format.args = list(decimal.mark = ',', big.mark = ".")) %>%
    add_header_above(c("", "Female" = 2, "Male" = 2, "Total" = 2, 
                           "Female" = 2, "Male" = 2, "Total" = 2, 
                           "Female" = 2, "Male" = 2, "Total" = 2, 
                           "Female" = 2, "Male" = 2, "Total" = 2)) %>% 
    add_header_above(c("", "Local Population" = 6, "Migrants from GER" = 6, "Migrants from third countries" = 6, "Total" = 6)) %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  return(k)
}

head.mean.byMigrationandGender <- function(t){
  k <- kbl(t, col.names = c("",rep(c("n","Mean"),12)), align = c("l", rep("r",18)), digits = 2, format.args = list(decimal.mark = ',', big.mark = ".")) %>%
    add_header_above(c("", "Female" = 2, "Male" = 2, "Total" = 2, 
                           "Female" = 2, "Male" = 2, "Total" = 2, 
                           "Female" = 2, "Male" = 2, "Total" = 2, 
                           "Female" = 2, "Male" = 2, "Total" = 2)) %>% 
    add_header_above(c("", "Local Population" = 6, "Migrants from GER" = 6, "Migrants from third countries" = 6, "Total" = 6)) %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  return(k)
}

#' 
#' ## Graphics
#' 

viz.data <- function(df, by.var){
  if (length(by.var) == 2) {
    aux <- freq.tables.byVar1(df,by.var) %>% 
      gather("var2",  "value", -1) %>% 
      separate(var2, into = c("var2", "unit"), sep = " ") %>% 
      mutate(value = as.numeric(gsub("%|,", "", value)),
             value = ifelse(unit == "P", value/100, value)) 
    
    names(aux) <- c(paste0("var" , seq(1, length(by.var),1)), "unit",  "value")
  }else{
    aux <- freq.tables.byVar1and2(df,by.var) %>% 
      gather("var3",  "value", -1) %>% 
      mutate(var2 = trimws(str_extract(var3, " Male| Female| Total"), which = "both"),
             unit = str_extract(var3,"N.*"),
             unit = ifelse(is.na(unit), "P", unit),
             var3 = trimws(gsub("Male P|Female P|Total P|Male N|Female N|Total N", "", var3), which = "both" ),
             value = as.numeric(gsub("%|,", "", value)),
             value = ifelse(unit == "P", value/100, value)) %>% 
      relocate(var2, .after = var3) %>% 
      relocate(unit, .before = value)
    names(aux) <- c(paste0("var" , seq(1, length(by.var),1)), "unit",  "value")
    
  }
  return(aux)
}

viz.data2 <- function(mydata){
  aux <- mydata %>% gather("var2",  "value", -1) %>% 
    mutate(var3 = trimws(str_extract(var2, " Male| Female| Total"), which = "both"),
           unit = str_extract(var2,"N.*"),
           unit = ifelse(is.na(unit), "P", unit),
           var2 = trimws(gsub("Male P|Female P|Total P|Male N|Female N|Total N", "", var2), which = "both" ),
           var2 = plyr::mapvalues(var2, c("Local Population", "Migrants from GER", "Migrants from third countries"),  
                                        c("Local \nPopulation", "Migrants \nfrom GER", "Migrants from \nthird countries")),
           value = as.numeric(gsub("%|,", "", value)),
           value = ifelse(unit == "P", value/100, value))%>% 
    relocate(unit, .before = value) %>% 
    relocate(var3, .after = var2) %>% rename(var1 = Service)
  
  return(aux)
}   

finalize <- function(mydata){
  DomBot <- unique(select(mydata, var1, Tot, DomSize)) %>% ungroup() %>% 
    mutate(Bottom = Tot - cumsum(DomSize))
  mydata <- inner_join(mydata, select(DomBot, var1, Bottom))
  mydata <- mutate(mydata, Pos = Bottom + CUM - count/2)
  return(mydata)
}


graph <- function(df, by.var){
  
  p <- list()
  data <- viz.data(df,by.var) %>% filter(unit == "N")
  
  if(by.var[[2]] == "Migration"){
    data$var1 = plyr::mapvalues(data$var1, c("Local Population", "Migrants from GER", "Migrants from third countries"),  
                                           c("Local \nPopulation", "Migrants \nfrom GER", "Migrants from \nthird countries"))
  }

  p[[1]] <- ggplot(data, aes(x = var2, y = value, fill = var2) ) +
    geom_bar(stat = "identity", position =  position_dodge2(), color = "#a2a2a2", alpha =0.9) + 
    facet_grid(.~var1, scales = "free_x",  space = "free_x", switch = "x") +
    geom_text(aes(label = value), vjust = -0.2, color = "#535353", size = 3) + labs(title = "") + ylab("") +
    theme_bw() + 
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.placement = "outside",
          plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = ccPalette[c(2,4,8,7)]) +
    scale_x_discrete(NULL, guide = "axis_nested") 
  
  ## multiple donuts
  data <- viz.data(df,by.var) %>% 
    filter(var2 != "Total") %>% 
    spread(unit, value) %>% 
    rename(category = var2,
           fraction = P,
           count = N) %>% ungroup() 
  
  if(by.var[[2]] == "Migration"){
    data$var1 = plyr::mapvalues(data$var1, c("Local Population", "Migrants from GER", "Migrants from third countries"),  
                                c("Local \nPopulation", "Migrants \nfrom GER", "Migrants from \nthird countries"))
  }
  
  data <- data %>% 
    mutate(label = paste0(category, "\n", scales::percent(fraction/10, accuracy = .1, decimal.mark = ",")), 
           total = sum(count, na.rm = T)) %>% 
    group_by(var1) %>% 
    mutate(group.total = sum(count, na.rm = T), 
           label2 =   paste0(var1, "\n", scales::percent(group.total/total, accuracy = .1, decimal.mark = ",")))
  
  aux <- data %>% group_by(var1, label2) %>% 
    summarise(count = sum(count, na.rm = T))%>% 
    ungroup %>% 
    mutate(Tot = sum(count, na.rm = T)) %>% 
    group_by(var1) %>% 
    mutate(CUM = cumsum(ifelse(is.na(count), 0, count)), DomSize = max(CUM, na.rm = T))
  aux <- finalize(aux)
  
  aux2 <- data %>% ungroup %>% select(var1, category, count, label) %>%
    mutate(Tot = sum(count, na.rm = T)) %>% 
    group_by(var1) %>% 
    mutate(CUM = cumsum(ifelse(is.na(count), 0, count)), DomSize = max(CUM, na.rm = T))
  aux2 <- finalize(aux2)
  
  if (sum(grepl("Migrants", aux$var1)) >= 1) {
    
    p[[2]] <- ggplot() +
      geom_col(data = aux, width = 1.8, aes(x = 1.7, y = count, fill = var1), color = "white", size=1)  +
      geom_col(data = aux2,  width = 0.6, aes(x = 3, y = count, fill = var1, alpha = rev(category)), color = "white", size=1) +  
      xlim(0, 3.5) + labs(x = NULL, y = NULL) +
      geom_text( x = 1.5, aes(y=Pos, label=label2), colour = "#535353", data = aux ,size=3.5)  + 
      geom_text( x = 3.8, aes(y=Pos, label=label), colour = "#535353", data = aux2 ,size=3.5)  + 
      theme_bw()+
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_blank()) +
      scale_fill_manual(values = ccPalette[c(7,1,3)] ) + 
      labs(title = "") + 
      coord_polar(theta="y")+
      scale_alpha_discrete(range = c(0.6,1)) 
    
  }else{
    
    p[[2]] <- ggplot() +
      geom_col(data = aux, width = 1.8, aes(x = 1.7, y = count, fill = var1), color = "white", size=1)  +
      geom_col(data = aux2,  width = 0.6, aes(x = 3, y = count, fill = var1, alpha = rev(category)), color = "white", size=1) +  
      xlim(0, 3.5) + labs(x = NULL, y = NULL) +
      geom_text( x = 1.5, aes(y=Pos, label=label2), colour = "#535353", data = aux ,size=3.5)  + 
      geom_text( x = 3.8, aes(y=Pos, label=label), colour = "#535353", data = aux2 ,size=3.5)  + 
      theme_bw()+
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_blank()) +
      scale_fill_manual(values = c(ccPalette[c(7,1,3)],"#f29910",ccPalette[c(4:6,2,8)])) + 
      labs(title = "") + 
      coord_polar(theta="y")+
      scale_alpha_discrete(range = c(0.6,1)) 
  }
 
  
  return(p)
}


bar.plot <- function(mydata, n = c(2,4), faced = F, flip = F, perc = F){
  
  if("Local Population" %in% unique(mydata$var3)){
    mydata$var3 <- plyr::mapvalues( mydata$var3, c("Local Population", "Migrants from GER", "Migrants from third countries"),  
                                                 c("Local \nPopulation", "Migrants \nfrom GER", "Migrants from \nthird countries"))
  }
  
  mydata$var1 <- str_to_sentence(mydata$var1) 
  
  if(flip){
    mydata <- mydata %>% arrange(value) %>% mutate(var1 = factor(var1, levels=unique(var1)))
  } else {
    mydata <- mydata %>% arrange(desc(value)) %>% mutate(var1 = factor(var1, levels=unique(var1)))
  }
  
  p <- ggplot(mydata, aes(x = var1, y = value, fill = var3) ) +
        geom_bar(stat = "identity", position = position_dodge(), color = "#a2a2a2", alpha =0.9, width = 0.8) + 
        labs(title = "") + ylab("") +
        theme_bw() 
  
    if(flip){
      p <- p  +
        coord_flip() + 
        theme(axis.text.x =  element_blank(),
              axis.ticks.x = element_blank()) +
        geom_blank(aes(y=value*1.2)) 
        
        if (perc) {
          p <- p +  geom_text(aes(label =  scales::percent(value, accuracy = 1)), position = position_dodge(width = 0.8), hjust= -0.25, color = "#535353", size = 3.5)
        }else{
          p <- p +  geom_text(aes(label = value), position = position_dodge(width = 0.8), hjust= -0.25, color = "#535353", size = 3.5)
        }
      
      
    } else {
      p <- p  +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
              axis.text.y =  element_blank(),
              axis.ticks.y = element_blank()) +
        geom_blank(aes(y=value*1.1))
      
        if (perc) {
        p <- p + geom_text(aes(label =  scales::percent(value, accuracy = 1)), position = position_dodge(width = 0.8), vjust= -0.25, color = "#535353", size = 3.5)
        }else{
          p <- p + geom_text(aes(label = value), position = position_dodge(width = 0.8), vjust= -0.25, color = "#535353", size = 3.5)
        }
  }
   if(faced){
     p <- p + facet_grid(.~var2, scales = "free_x",  space = "free_x") 
   }
  if (length(n) > 1) {
      p <- p + theme(legend.title = element_blank())
  } else{
      p <- p + theme(legend.position = "none")
  }

  p <- p + theme(strip.background = element_blank(),
                 strip.placement = "outside",
                 plot.title = element_text(hjust = 0.5))
  
 p <- p  +
    scale_fill_manual(values = ccPalette[n]) +
    scale_x_discrete(NULL, guide = "axis_nested") + 
    scale_y_continuous(labels = scales::percent)
 
 return(p)
}

circular.barplot <- function(mydata){
  df <- mydata %>% 
    gather("var2",  "value", -1) %>% 
    mutate(var3 = trimws(str_extract(var2, " Male| Female| Total"), which = "both"),
           unit = str_extract(var2,"N.*"),
           unit = ifelse(is.na(unit), "P", unit),
           var2 = trimws(gsub("Male P|Female P|Total P|Male N|Female N|Total N", "", var2), which = "both" ),
           var2 = plyr::mapvalues(var2, c("Local Population", "Migrants from GER", "Migrants from third countries"),  
                                        c("Local \nPopulation", "Migrants \nfrom GER", "Migrants from \nthird countries")),
           value = as.numeric(gsub("%|,", "", value)),
           value = ifelse(unit == "P", value/100, value),
           Service =  str_sub(Service, 4, nchar(Service)))%>% 
    relocate(unit, .before = value) %>% 
    relocate(var3, .after = var2) %>% 
    filter(unit == "N", var2 != "Total", var3 != "Total") %>% select(-unit) 
  
  names(df) <- c("individual", "group", "observation", "value")
  data <- df
  
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 2
  nObsType <- nlevels(as.factor(data$observation))
  to_add <- data.frame(matrix(NA, empty_bar*nlevels(as.factor(data$group))*nObsType, ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$group <- rep(levels(as.factor(data$group)), each=empty_bar*nObsType )
  data <- rbind(data, to_add)
  data <- data %>% arrange(group, individual)
  data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)
  
  # Get the name and the y position of each label
  label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value, na.rm = T))
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(group) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # Make the plot
  p <- ggplot(data) +      
    
    # Add the stacked bar
    geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", color = "#a2a2a2", alpha =0.6) +
    scale_fill_manual(values = ccPalette[c(2,4)]) +
    
    # Add text showing the value of each 100/75/50/25 lines
    ylim(-400, max(label_data$tot, na.rm=T)+20) +
    theme_bw()+
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks = element_blank(), 
          axis.title = element_blank(),  
          panel.border = element_blank()) +
    coord_polar() +
    
    # Add labels on top of each bar
    geom_text(data=label_data, aes(x=id, y=tot+10, label=individual, hjust=hjust), color="black",alpha=0.6, size=3.2, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, aes(x = start -.5, y = -9, xend = end +.5, yend = -9), colour = "black", alpha=0.8, size=0.5, inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -50, label=group), hjust=c(1.2,0.7,0),  vjust=c(.3,-0.2,1), colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE)
  
  return(p)
}


library(SnowballC)
library(wordcloud)
library(tm)

se <- c("baš","bez","biæe","bio","biti","blizu","broj","dana","danas","doæi","dobar","dobiti","dok",
  "dole","došao","drugi","duž","dva","èesto","èiji","gde","gore","hvala","iæi","iako","ide",
  "ima","imam","imao","ispod","izmeðu","iznad","izvan","izvoli","jedan","jedini","jednom",
  "jeste","još","juèe","kad","kako","kao","koga","koja","koje","koji","kroz","mali","manji",
  "misli","mnogo","moæi","mogu","mora","morao","naæi","naš","negde","nego","nekad","neki",
  "nemam","nešto","nije","nijedan","nikada","nismo","ništa","njega","njegov","njen","njih",
  "njihov","oko","okolo","ona","onaj","oni","ono","osim","ostali","otišao","ovako","ovamo",
  "ovde","ove","ovo","pitati","poèetak","pojedini","posle","povodom","praviti","pre","preko",
  "prema","prvi","put","radije","sada","smeti","šta","stvar","stvarno","sutra","svaki","sve",
  "svim","svugde","taèno","tada","taj","takoðe","tamo","tim","uèinio","uèiniti","umalo","unutra",
  "upotrebiti","uzeti","vaš","veæina","veoma","video","više","zahvaliti","zašto","zbog","želeo",
  "želi","znati")

al <- c("apo","asnjë","asnje","ata","ato","ca","deri","dhe","do","e","i","jam","janë","jane","jemi","jeni",
        "ju","juaj","kam","kaq","ke","kemi","kete","këtë","më me","mu","në","ne","nëse","nese","një","nje",
        "nuk","pa","pas","pasi","për","per","prej","që","qe","sa","së","se","seç","sec","si","saj","të","te",
        "ti","tek","tij","tonë","tone","tuaj","ty","tyre","unë","une","veç","vec")


get_worldcloud <- function(aux, stoplang){
  aux <- iconv(aux, to = "UTF-8")
  #aux <- iconv(aux, to='ASCII//TRANSLIT') 
  
  aux <- Corpus(VectorSource(aux))
  aux <- tm_map(aux, tolower)
  aux <- tm_map(aux, stripWhitespace) 
  aux <- tm_map(aux, removeNumbers) 
  aux <- tm_map(aux, removePunctuation) 
  aux <- tm_map(aux, removeWords, c(stopwords(stoplang), se,al,"nuk","sam","kam","dont","nje","per")) 
  aux <- TermDocumentMatrix(aux) 
  aux <- as.matrix(aux) 
  aux <- sort(rowSums(aux),decreasing=TRUE)
  df <- data.frame(word = names(aux), freq = aux) 
  
  wordcloud(words = df$word, freq = df$freq, min.freq = 0.5,
            max.words=100, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Reds"))
}

#' # Load data
list.data <- list.files(data.folder)

data <- NULL

for (i in list.data) {
  rawData <- read_sav(paste0(data.folder,i))
  myvars <- colnames(rawData)[grepl("F1_CSO__",colnames(rawData))]
  if (!is.null(rawData$No)) {
    rawData <- rawData %>% rename("ID" = "No")
  }else{
    rawData <- rawData %>% rename("ID" = "VAR00001")
  }
  rawData <- rawData %>% select_at(colnames(rawData)[!(colnames(rawData) %in% myvars)]) %>% select(!starts_with("v"))
  data <- rbind(data, rawData)
}

rawData <- data

data <- data %>% rename("VAR00001" = "ID") 

data$Country <- substr(data$VAR00001,1,2)

data <- data %>% filter(Country != "")

data$Gender <- ifelse(data$UI4 == 1, "Male", ifelse(data$UI4 == 2, "Female", "Other"))

data$Age <- cut(data$UI3, c(0,25,35,45,300))
levels(data$Age) <- c('18-25','26-35','36-45','older than 45')
table(data$UI3, data$Age, exclude = F)

data$Migration <- ifelse(data$UI5 == 0, "Local Population", 
                    ifelse(data$UI5_country == 74, "Migrants from GER", "Migrants from third countries"))

data$F1_1 <- ifelse(data$F1__1 == 1, "Advisory Center", NA)
data$F1_2 <- ifelse(data$F1__2 == 1, "CSO", NA)
data$F1_3 <- ifelse(data$F1__3 == 1, "Other", NA)
data$F1_4 <- ifelse(data$F1__4 == 1, "Not support", NA)

data$Organization <- paste(data$F1_1, data$F1_2, data$F1_3, data$F1_4, sep = " & ")
data$Organization <- gsub("NA & | & NA$|NA$","", data$Organization)
data$Organization <- gsub(" & $","", data$Organization)

data$YearReturn <- as.numeric(substr(data$UI5_datereturn,1,4))
data$CountryReturn <- as.character(as_factor(data$UI5_country))

data$F2_other <- gsub("none| لا شيء | لا ھیج ","", data$F2_other, ignore.case = T)
data$F2_other_spec <- data$F2_other
data$F2_other <- ifelse(data$F2_other == "", 0, 1)
myvars <- colnames(data)[grepl("F2_",colnames(data)) & !grepl("other",colnames(data))]
data[,paste0(myvars,"_rate")] <- data[,myvars]
data <- data %>% mutate_at(.vars = myvars, .funs = ~ifelse(. > 0, 1, 0))
data <- data %>% mutate_at(.vars = paste0(myvars,"_rate"), .funs = ~ifelse(. == 0, NA, .))

data$MHPSS_improve <- ifelse(data$Q3_2 > data$Q3_1, "Better", ifelse(data$Q3_2 == data$Q3_1, "Equal", "Worse"))
data$MHPSS_improve_points <- data$Q3_2 - data$Q3_1

data$Q3_4_11_other <- gsub("none| لا شيء | لا ھیج ","", data$Q3_4_11_other, ignore.case = T)
data$Q3_4_11_other_spec <- data$Q3_4_11_other
data$Q3_4_11_other <- ifelse(data$Q3_4_11_other == "", 0, 1)
myvars <- colnames(data)[grepl("Q3_4_",colnames(data)) & !grepl("other",colnames(data))]
data[,paste0(myvars,"_rate")] <- data[,myvars]
data <- data %>% mutate_at(.vars = myvars, .funs = ~ifelse(. > 0, 1, 0))
data <- data %>% mutate_at(.vars = paste0(myvars,"_rate"), .funs = ~ifelse(. == 0, NA, .))

data$Q1_2 <- as.character(as_factor(data$Q1_2))
data$Q1_3[is.na(data$Q1_3) & !is.na(is.na(data$Q1_2))] <- 8
data$Q1_3 <- as.character(as_factor(data$Q1_3))
data$Q1_5 <- as.character(as_factor(data$Q1_5))
data$Q1_6 <- as.character(as_factor(data$Q1_6))
data$Q1_7 <- as.character(as_factor(data$Q1_7))

data$Q2_1_other <- gsub("none| لا شيء | لا ھیج ","", data$Q2_1_other, ignore.case = T)
data$Q2_1_other_spec <- data$Q2_1_other
data$Q2_1_other <- ifelse(data$Q2_1_other == "", 0, 1)

data$Q2_1_coaching_startup[data$Country %in% countries.without.jobfair & !is.na(data$Q2_1_coaching_startup) & data$Q2_1_coaching_startup == 1] <- 0

#data$Q2_4 <- as.character(as_factor(data$Q2_4))

myvars <- colnames(data)[grepl("Q2_1",colnames(data)) & !grepl("other",colnames(data))]
data[,paste0(myvars,"_rate")] <- data[,myvars]
data <- data %>% mutate_at(.vars = myvars, .funs = ~ifelse(. > 0, 1, 0))
data <- data %>% mutate_at(.vars = paste0(myvars,"_rate"), .funs = ~ifelse(. == 0, NA, .))


myvars <- colnames(data)[grepl("F3_",colnames(data)) & grepl("_lr",colnames(data))]
data <- data %>% mutate_at(.vars = myvars, .funs = ~ifelse(nchar(.) == 1, NA, .))

data$Q2_3[nchar(data$Q2_3) == 1] <- NA
data$Q2_3[grepl("##N/A##",data$Q2_3)] <- NA
data$Q2_3[grepl("asnje|None",data$Q2_3,ignore.case = T)] <- "None"
data$Q2_3[grepl("financial is|No financi|financial pro|fund|capital|Financial rea|Financial cons|finance|not financially|ana financiare",data$Q2_3,ignore.case = T) & !grepl("material",data$Q2_3,ignore.case = T)] <- "Financial constrains"
data$Q2_3[grepl("financial is|No financi|financial pro|fund|capital|Financial rea|Financial cons|finance|not financially|ana financiare",data$Q2_3,ignore.case = T) & grepl("material",data$Q2_3,ignore.case = T)] <- "Financial and materials constrains"
data$Q2_3[!grepl("financial is|No financi|financial pro|fund|capital|Financial rea|Financial cons|finance|not financially|ana financiare",data$Q2_3,ignore.case = T) & grepl("material",data$Q2_3,ignore.case = T)] <- "Materials constrains"

data$Q2_3[grepl("financiare",data$Q2_3,ignore.case = T)]

data <- data %>% mutate_if(is.character, ~replace(.,. == '', NA))
tracer <- as.data.frame(zap_labels(data))

for (i in unique(tracer$Country)) {
  
  country <- tracer %>% filter(Country == i)
  rmarkdown::render(paste0("GIZ2103_Country_",doc.ver,".Rmd"), output_file = paste0("../Outputs/GIZ2103_Country_",i,"_",doc.ver,".html"))
  
}

i <- "All"
country <- tracer %>% mutate(Nationality = Country, Country = i)
rmarkdown::render(paste0("GIZ2103_Country_",doc.ver,".Rmd"), output_file = paste0("../Outputs/GIZ2103_All_Countries_",doc.ver,".html"))

data <- tracer %>% filter(!is.na(UI3)) %>% filter(!is.na(Age)) %>% filter(Gender != "Other") %>% filter(!(F1__4 == 1)) %>% filter(!(VAR00001 %in% not.valid))
#write_sav(data, paste0("../Outputs/GIZ2103_Tracer-Studies_Data-Clean_",doc.ver,".sav"))

