#packages-------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(descr)
library(data.table)
library(rlist)

#import-------
maclaury=read.csv("/Users/liusimin/Desktop/MacLaury/ColCatData/fulldata.csv")

#clean------
maclaury=maclaury %>% arrange(survey_id, informant, decreasing=FALSE)
maclaury=maclaury %>% group_by(survey_id, informant)

#unify language names
maclaury$Language[maclaury$Language=="Chinantec"]="Chinanteco"
maclaury$Language[maclaury$Language=="Zapoteco" | maclaury$Language== "Zapoteco de Mixtepec"]="Zapotec"
maclaury$language_participant=paste(maclaury$survey_id, "_", maclaury$informant)
maclaury=maclaury %>% filter(Language %in% c("Chinanteco","Mixtec","Zapotec"))

chinanteco=maclaury %>% filter(Language=="Chinanteco")
mixtec=maclaury %>% filter(Language=="Mixtec")
zapotec=maclaury %>% filter(Language=="Zapotec")

#testing shared chips: nothing-------
s11i1=maclaury[maclaury$survey_id==11 & maclaury$informant==1,  ]
length(unique(s11i1$idChip))
length(unique(s11i1$term))
length(unique(s11i1$boxRC))
length(unique(s11i1$WCS_chip))

commonchips=Reduce(intersect, split(maclaury$WCS_chip, maclaury$language_participant))
commonchips

#chinanteco----------
#creating color naming metrics for each participant
#columns: number of chips; rows: unique color naming terms for the whole language
#if for a chip the answer is missing, then do 1/union of all unique terms in this language
u_chi_chips=unique(chinanteco$idChip)
u_chi_terms=unique(chinanteco$term)
cols_chi=length(u_chi_chips) #280
rows_chi=length(u_chi_terms)#29
unique_terms=length(unique(chinanteco$term))
chi_df=list()
counting_chi=0
for (surveyid in unique(chinanteco$survey_id)){
  for (informantid in unique(chinanteco[chinanteco$survey_id==surveyid, "informant"])){
    current=chinanteco %>% filter(survey_id==surveyid, informant==informantid)
    counting_chi=counting_chi+1
    xmatrix=matrix(NA,nrow=rows_chi,ncol=cols_chi
                   ,dimnames = list(u_chi_terms, u_chi_chips))
    for (idchip in unique(current$idChip)){
      for (person in unique(current$language_participant)) {
        if (grepl(idchip, current %>% filter(language_participant==person) %>% select(idChip))) {
          xmatrix[current[current$language_participant==person & current$idChip==idchip, "term"],as.character(idchip)]=1
          xmatrix[is.na(xmatrix[,as.character(idchip)]),as.character(idchip)]=0
        }
      }
    }
    xmatrix=xmatrix %>% as.data.frame() %>% mutate_all(~replace(., is.na(.), 1/unique_terms))
    chi_df[[counting_chi]]=xmatrix
  }
}
chi_metrics=lapply(chi_df, as.matrix)
#each matrix = chi_metrics[[i]]

#mixtec--------
u_mix_chips=unique(mixtec$idChip)
u_mix_terms=unique(mixtec$term)
cols_mix=length(u_mix_chips) #280
rows_mix=length(u_mix_terms)#29
unique_mix_terms=length(unique(mixtec$term))
counting_mix=0
mix_df=list()
for (surveyid in unique(mixtec$survey_id)){
  for (informantid in unique(mixtec[mixtec$survey_id==surveyid, "informant"])){
    current=mixtec %>% filter(survey_id==surveyid, informant==informantid)
    counting_mix=counting_mix+1
    xmatrix=matrix(NA,nrow=rows_mix,ncol=cols_mix
                   ,dimnames = list(u_mix_terms, u_mix_chips))
    for (idchip in unique(current$idChip)){
      for (person in unique(current$language_participant)) {
        if (grepl(idchip, current %>% filter(language_participant==person) %>% select(idChip))) {
          xmatrix[current[current$language_participant==person & current$idChip==idchip, "term"],as.character(idchip)]=1
          xmatrix[is.na(xmatrix[,as.character(idchip)]),as.character(idchip)]=0
        }
      }
    }
    xmatrix=xmatrix %>% as.data.frame() %>% mutate_all(~replace(., is.na(.), 1/unique_mix_terms))
    mix_df[[counting_mix]]=xmatrix
  }
}
mix_metrics=lapply(mix_df, as.matrix)


#zapoteco---------



