dev.off(); # ?
rm(list=ls()); #remove this command
options(scipen=100, digits=3); #digit formatting

## install libraries
library("pacman");
p_load(ggplot2,tidyverse, Hmisc, gtable, gridExtra, plyr, dplyr, lme4, eply, stringr, do,glmm, car, scales, tidyr, grid, ggrepel, lmerTest, tidyselect, tidyverse, Kmisc,
       nlme, ggpubr, goft, openxlsx, broom.mixed, patchwork, psych,
       hrbrthemes, latticeExtra, plotrix, pbapply, data.table, word2vec, here);

## if you're using custom packages, include path

## set paths
# setwd("/Users/elenaluchkina/Documents/GitHub/code_review/9 months csv")
path <-here::here('data/9 months csv')  # consider getting rid of sapces in filenames
merge_file_name <- here::here('data/9 months csv/All_9mo')
  
## load data
filenames <- list.files(path= path, full.names=TRUE)
all_data <- map_df(filenames, ~read.csv(.x) %>% mutate(File = basename(.x)))
df <- strings2factors(all_data) # convert all strings to factors, rename as df

## preprocessing the data structure
df[df==""] <- NA
df <- df %>% mutate(ID_subj=ID.code01)
data <- df
remove <-grepl("ordinal|code01|offset|childspeech|momspeech|X",names(data))
variables <- names(data)
delete <-data.frame(remove,variables)
data <- data[,delete$variables[which(delete$remove==FALSE)]]

## making a composite variable

data$I_beh_obj <- paste_na(data$I_beh_obj.Manipulate,data$I_beh_obj.Inspect,data$I_beh_obj.Ignore,data$I_beh_obj.Other, sep='') # pasta_na
data$I_beh_obj_type <- NA 
data <- data%>% mutate(
  I_beh_obj_type=case_when(!is.na(I_beh_obj.Manipulate)~"Manipulate",
                           !is.na(I_beh_obj.Inspect)~"Inspect",
                           !is.na(I_beh_obj.Ignore)~"Ignore",
                           !is.na(I_beh_obj.Other)~"Other",
                           TRUE ~ NA_character_)
)

data$M_beh_obj <- paste_na(data$M_beh_obj.Retrieve,data$M_beh_obj.Show,data$M_beh_obj.Offer,data$M_beh_obj.Prompt_action,data$M_beh_obj.Hold,data$M_beh_obj.Other, sep='')
data$M_beh_obj_type <- NA
data <- data%>% mutate(
  M_beh_obj_type=case_when(!is.na(M_beh_obj.Retrieve)~"Retrieve",
                           !is.na(M_beh_obj.Show)~"Show",
                           !is.na(M_beh_obj.Offer)~"Offer",
                           !is.na(M_beh_obj.Prompt_action)~"Prompt_action",
                           !is.na(M_beh_obj.Hold)~"Hold",
                           !is.na(M_beh_obj.Other)~"Other",
                           TRUE ~ NA_character_)
)

data$I_beh_other <- paste_na(data$I_beh_other.Gesture,data$I_beh_other.Move,data$I_beh_other.Touch_body,data$I_beh_other.Other, sep='')
data$I_beh_other_type <- NA
data <- data%>% mutate(
I_beh_other_type=case_when(!is.na(I_beh_other.Gesture)~"Gesture",
                             !is.na(I_beh_other.Move)~"Move",
                             !is.na(I_beh_other.Touch_body)~"Touch_body",
                             !is.na(I_beh_other.Other)~"Other",
                             TRUE ~ NA_character_)
)


data$M_beh_other <- paste_na(data$M_beh_other.Gesture,data$M_beh_other.Move,data$M_beh_other.Touch_body,data$M_beh_other.Other, sep='')
data$M_beh_other_type <- NA
data <- data %>% mutate(
  M_beh_other_type=case_when(!is.na(M_beh_other.Gesture)~"Gesture",
                               !is.na(M_beh_other.Move)~"Move",
                               !is.na(M_beh_other.Touch_body)~"Touch_body",
                               !is.na(M_beh_other.Other)~"Other",
                               TRUE ~ NA_character_)
)


data$I_looking_dir <- paste_na(data$I_looking_dir.To_mother,data$I_looking_dir.To_object_specify_object,
                               data$I_looking_dir.Away,data$I_looking_dir.Other, sep='')
data$I_looking_dir_type <- NA
data <- data%>% mutate(
  I_looking_dir_type=case_when(!is.na(I_looking_dir.To_mother)~"Mother",
                               !is.na(I_looking_dir.To_object_specify_object)~"Object",
                               !is.na(I_looking_dir.Away)~"Away",
                               !is.na(I_looking_dir.Other)~"Other",
                               TRUE ~ NA_character_)
)


data$M_looking_dir <- paste_na(data$M_looking_dir.To_mother,data$M_looking_dir.To_object_specify_object,
                               data$M_looking_dir.Away,data$M_looking_dir.Other, sep='')
data$M_looking_dir_type <- NA
data <- data%>% mutate(
  M_looking_dir_type=case_when(!is.na(M_looking_dir.To_infant)~"Infant",
                       !is.na(M_looking_dir.Joint_with_baby)~"Joint",
                       !is.na(M_looking_dir.Away)~"Away",
                       !is.na(M_looking_dir.Other)~"Other",
                       TRUE ~ NA_character_)
)


data$I_voc <- paste_na(data$I_voc.NL,data$I_voc.CV,data$I_voc.V,data$I_voc.Utterance, sep='')
data$I_voc_type <- NA
data <- data%>% mutate(
  I_voc_type=case_when(!is.na(I_voc.NL)~"Non-lingustic",
                       !is.na(I_voc.CV)~"Consonant-vowel",
                       !is.na(I_voc.V)~"Vowel",
                       !is.na(I_voc.Utterance)~"Utterance",
                       TRUE ~ NA_character_)
)


data$M_voc <- paste_na(data$M_voc.Ack,data$M_voc.Att,data$M_voc.Dir,data$M_voc.Proh,data$M_voc.Nam,data$M_voc.Play,
                       data$M_voc.Que,data$M_voc.Imit,data$M_voc.Narr_ment,data$M_voc.Narr_act,data$M_voc.Other, sep='')
data$M_voc_type <- NA
data<- data %>% mutate(
  M_voc_type=case_when(!is.na(M_voc.Ack)~"Acknowledgement",
                           !is.na(M_voc.Att)~"Attribution",
                           !is.na(M_voc.Dir)~"Directive",
                           !is.na(M_voc.Proh)~"Prohibition",
                           !is.na(M_voc.Nam)~"Naming",
                           !is.na(M_voc.Play)~"Play",
                           !is.na(M_voc.Que)~"Question",
                           !is.na(M_voc.Imit)~"Imitation",
                           !is.na(M_voc.Narr_ment)~"Narrating mental state",
                           !is.na(M_voc.Narr_act)~"Narrating activity",
                           !is.na(M_voc.Other)~"Other",
                           TRUE ~ NA_character_)
  )

data$ID <- substr(data$File,1,4)
setwd("/Users/elenaluchkina/Documents/GitHub/code_review")
vocab_demo <- strings2factors(read.csv("vocabulary 12mo.csv",header = TRUE))
data <- merge(data,vocab_demo,by='ID')

data$cont_I_obj_M_voc <- NA
for (h in unique(data$File)){
  for (i in unique_no.NA(data$I_beh_obj.onset[which(data$File==h)])) {
   for (j in unique_no.NA(data$M_voc.onset[which(data$File==h)])) {
     if(data$M_voc.onset[which(data$M_voc.onset==j&data$File==h)]-data$I_beh_obj.onset[which(data$I_beh_obj.onset==i&data$File==h)]<=2000 &
        data$M_voc.onset[which(data$M_voc.onset==j&data$File==h)]-data$I_beh_obj.onset[which(data$I_beh_obj.onset==i&data$File==h)]>0)
       {
      data$cont_I_obj_M_voc[which(data$I_beh_obj.onset==i&data$File==h)] <- 
        paste(data$I_beh_obj[which(data$I_beh_obj.onset==i&data$File==h)], ", ",
              data$M_voc_type[which(data$M_voc.onset==j&data$File==h)],", ",
              data$M_voc[which(data$M_voc.onset==j&data$File==h)],", ",
              data$M_voc.onset[which(data$M_voc.onset==j&data$File==h)]-
                data$I_beh_obj.onset[which(data$I_beh_obj.onset==i&data$File==h)]
                                                          )
      next
     }
    }
  }
}

data$cont_I_voc_M_voc <- NA
for (h in unique(data$File)){
  for (i in unique_no.NA(data$I_voc.onset[which(data$File==h)])) {
    for (j in unique_no.NA(data$M_voc.onset[which(data$File==h)])) {
      if(data$M_voc.onset[which(data$M_voc.onset==j&data$File==h)]-data$I_voc.onset[which(data$I_voc.onset==i&data$File==h)]<=2000 &
         data$M_voc.onset[which(data$M_voc.onset==j&data$File==h)]-data$I_voc.onset[which(data$I_voc.onset==i&data$File==h)]>0)
      {
        data$cont_I_voc_M_voc[which(data$I_voc.onset==i&data$File==h)] <- 
          paste(data$I_voc[which(data$I_voc.onset==i&data$File==h)], ", ",
                data$M_voc_type[which(data$M_voc.onset==j&data$File==h)],", ",
                data$M_voc[which(data$M_voc.onset==j&data$File==h)],", ",
                data$M_voc.onset[which(data$M_voc.onset==j&data$File==h)]-
                  data$I_voc.onset[which(data$I_voc.onset==i&data$File==h)]
          )
        next
      }
    }
  }
}

data$cont_I_other_M_voc <- NA
for (h in unique(data$File)){
  for (i in unique_no.NA(data$I_beh_other.onset[which(data$File==h)])) {
    for (j in unique_no.NA(data$M_voc.onset[which(data$File==h)])) {
      if(data$M_voc.onset[which(data$M_voc.onset==j&data$File==h)]-data$I_beh_other.onset[which(data$I_beh_other.onset==i&data$File==h)]<=2000 &
         data$M_voc.onset[which(data$M_voc.onset==j&data$File==h)]-data$I_beh_other.onset[which(data$I_beh_other.onset==i&data$File==h)]>0)
      {
        data$cont_I_other_M_voc[which(data$I_beh_other.onset==i&data$File==h)] <- 
          paste(data$I_beh_other[which(data$I_beh_other.onset==i&data$File==h)], ", ",
                data$M_voc_type[which(data$M_voc.onset==j&data$File==h)],", ",
                data$M_voc[which(data$M_voc.onset==j&data$File==h)],", ",
                data$M_voc.onset[which(data$M_voc.onset==j&data$File==h)]-
                  data$I_beh_other.onset[which(data$I_beh_otheroc.onset==i&data$File==h)]
          )
        next
      }
    }
  }
}
save(data, file = "contingency_new.RData")

load("contingency_new.RData")

data_length_video <- ddply(data,.(File), summarize, cont_I_obj_M_voc_n = length(unique(cont_I_obj_M_voc)),
                           cont_I_obj_M_voc_rate=cont_I_obj_M_voc_n/max(time/60000),
                           cont_I_voc_M_voc_n = length(unique(cont_I_voc_M_voc)),
                           cont_I_voc_M_voc_rate=cont_I_voc_M_voc_n/max(time/60000),
                           cont_I_other_M_voc_n = length(unique(cont_I_other_M_voc)),
                           cont_I_other_M_voc_rate=cont_I_other_M_voc_n/max(time/60000))


data <- merge(data,data_length_video, by='File')

data1 <- data
require(eply)
data1$M_voc <- unquote(data1$M_voc)
require(stringr)
data1$M_voc <- gsub("\\[[^][]*]", NA,data1$M_voc )
data1$M_voc_mlu=str_count(data1$M_voc,'\\w+')


data1$all_m_voc <- NA
data1$all_m_voc_tnw <- NA
all_m_voc_type <- NA
data1 <- data1 %>% 
  group_by(File) %>% 
    mutate(
    all_m_voc=paste(unique(M_voc[!is.na(M_voc)]), collapse = ' '),
    all_m_voc_tnw=str_count(all_m_voc,'\\w+'),
    all_m_voc_type=map_int(all_m_voc, ~
                             .x %>% 
                             str_trim %>% 
                             str_split(" ") %>% 
                             unlist() %>% 
                             n_distinct)
    ) %>% 
  ungroup()
  

data2 <- ddply(data1,.(File),summarize, all_m_voc_mlu=mean(M_voc_mlu, na.rm = TRUE))
data1 <- merge(data1,data2,by='File')

  

b <- ddply(data1, .(ID),summarize,
           I_obj_manipulate=length(unique(I_beh_obj.Manipulate)),
           I_obj_inspect=length(unique(I_beh_obj.Inspect)),
           I_obj_ignore=length(unique(I_beh_obj.Ignore)),
           I_obj_other=length(unique(I_beh_obj.Other)),
           M_obj_retrieve=length(unique(M_beh_obj.Retrieve)),
           M_obj_show=length(unique(M_beh_obj.Show)),
           M_obj_offer=length(unique(M_beh_obj.Offer)),
           M_obj_prompt=length(unique(M_beh_obj.Prompt_action)),
           M_obj_other=length(unique(M_beh_obj.Other)),
           M_obj_hold=length(unique(M_beh_obj.Hold)),
           M_obj_other=length(unique(M_beh_obj.Other)),
           I_other_gesture=length(unique(I_beh_other.Gesture)),
           I_other_move=length(unique(I_beh_other.Move)),
           I_other_touch_body=length(unique(I_beh_other.Touch_body)),
           I_other_other=length(unique(I_beh_other.Other)),
           M_other_gesture=length(unique(M_beh_other.Gesture)),
           M_other_move=length(unique(M_beh_other.Move)),
           M_other_touch_body=length(unique(M_beh_other.Touch_body)),
           M_other_other=length(unique(M_beh_other.Other)),
           Ack=length(unique(M_voc.Ack)),
           Att=length(unique(M_voc.Att)),
           Dir=length(unique(M_voc.Dir)),
           Proh=length(unique(M_voc.Proh)),
           Nam=length(unique(M_voc.Nam)),
          Play=length(unique(M_voc.Play)),
          Que=length(unique(M_voc.Que)),
          Imit=length(unique(M_voc.Imit)),
          Narr_ment=length(unique(M_voc.Narr_ment)),
          Narr_act=length(unique(M_voc.Narr_act)),
          Other=length(unique(M_voc.Other)),
          cont_I_obj_M_voc_n=mean(cont_I_obj_M_voc_n),
          cont_I_voc_M_voc_n=mean(cont_I_voc_M_voc_n),
          cont_I_other_M_voc_n=mean(cont_I_other_M_voc_n),
          cont_I_M=cont_I_obj_M_voc_n+cont_I_voc_M_voc_n+cont_I_other_M_voc_n,
          all_m_voc_tnw=mean(all_m_voc_tnw),
          all_m_voc_type=mean(all_m_voc_type),
          all_m_voc_mlu=mean(all_m_voc_mlu))

  covariance <- cor(b[-c(1)], y = NULL, use = "all.obs")
   data_full <- merge(vocab_demo,b,by="ID")
   covariance_sig <- rcorr(as.matrix(data_full[-c(1)]))$P
   covariance_val<- rcorr(as.matrix(data_full[-c(1)]))$r

l1 <- lm(UWORDS ~ all_m_voc_tnw + all_m_voc_type + all_m_voc_mlu, data = data_full)
summary(l1)
predicted_UWORDS <- predict(l1, newdata = data_full)
residuals_UWORDS <- data_full$UWORDS-predicted_UWORDS
data_full <- cbind(residuals_UWORDS,data_full)

l2 <- lm(PWORDS ~ all_m_voc_tnw + all_m_voc_type + all_m_voc_mlu, data = data_full)
summary(l2)
predicted_PWORDS <- predict(l2, newdata = data_full)
residuals_PWORDS <-  data_full$PWORDS-predicted_PWORDS
data_full <- cbind(residuals_PWORDS,data_full)

covariance_sig_2 <- rcorr(as.matrix(data_full[-c(3)]))$P
covariance_val_2<- rcorr(as.matrix(data_full[-c(3)]))$r

l2 <- lm(residuals ~ cont_I_obj_M_voc_n + cont_I_voc_M_voc_n +cont_I_other_M_voc_n, data = predicted_vocab_data)
summary(l2)
