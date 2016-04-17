#Singles
tab<- EventResultSingle.df %>% filter(program_type=="Short Program",
  category%in%c('Men','Ladies')) %>% 
  mutate(TSS_SP=TSS,TES_SP=TES,PCS_SP=PCS,SS_SP=SS,TR_SP=TR,
         PE_SP=PE,CH_SP=CH,IN_SP=IN,Ded_SP=Ded) %>%
  select(c(first_name,last_name,TSS_SP,TES_SP,PCS_SP,SS_SP,TR_SP,PE_SP,
           CH_SP,IN_SP,Ded_SP,season_start_year,season_end_year,category))

tab<-left_join(tab,EventResultSingle.df,by=c("first_name","last_name",
      "season_start_year","season_end_year","category")) %>% 
  filter(program_type!='Short Program')
Singles_w_rank<-tab%>%mutate(total_score=TSS+TSS_SP)%>%group_by(season_start_year,
  category,event_code)%>%arrange(desc(total_score))%>%mutate(rank=row_number())%>%
  select(-c(first_name_1,first_name_2,last_name_1,last_name_2,Q,
  program_type,PI))


#Doubles
tab2<- EventResultDouble.df %>% filter(program_type=="Short Program") %>% 
  mutate(TSS_SP=TSS,TES_SP=TES,PCS_SP=PCS,SS_SP=SS,TR_SP=TR,
         PE_SP=PE,CH_SP=CH,IN_SP=IN,Ded_SP=Ded) %>%
  select(c(first_name_1,first_name_2,last_name_1,last_name_2,TSS_SP,
           TES_SP,PCS_SP,SS_SP,TR_SP,PE_SP,CH_SP,IN_SP,Ded_SP,
           season_start_year,season_end_year,category))

double_w_rank<- left_join(tab2,EventResultDouble.df,
                         by=c("first_name_1","first_name_2",
                              "last_name_1","last_name_2",
                              "season_start_year","season_end_year","category"))%>% 
  filter(program_type=="Free Skating")%>% mutate(total=TSS_SP+TSS) %>%
  group_by(season_start_year,category,event_code) %>% arrange(desc(total)) %>% 
  mutate(rank=row_number())%>%select(-c(Q,program_type,PI))

#Combine Season Best

tab3<-SeasonBestScoreSingle.df%>%select(first_name,last_name,
             season_start_year, event,score)
singles_w_seasonbest<-left_join(Singles_w_rank,tab3,by=c("first_name",
"last_name","season_start_year"))

tab4<-SeasonBestScoreDouble.df%>%select(first_name_1,last_name_1,first_name_2,
last_name_2,season_start_year, event,score)
double_w_seasonbest<-left_join(double_w_rank,tab4,by=c("first_name_1",
        "last_name_1",'first_name_2','last_name_2',"season_start_year"))






