
# Visual Objects ------------------------------------------------------------------#
join_estimated_speeds <- function(pk_0_es, pk_1_es, ok_0_es, ok_1_es, ft_grouping_col){
  bind_rows(pk_0_es, pk_1_es, ok_0_es, ok_1_es) %>%
    filter(!is.na(EstAvgmphdwell) & !is.na(ModelSpeed)) %>%
    rename("Modeled" = ModelSpeed, "Observed" = EstAvgmphdwell )%>%
    mutate(Modeled = ifelse(is.na(Modeled),P_SPEED1,Modeled)) %>%
    mutate(
      MODE_Name = case_when(
        MODE == 4 ~ "4-Local Bus",
        MODE == 5 ~ "5-Core Bus",
        MODE == 6 ~ "6-Express Bus",
        MODE == 7 ~ "7-Light Rail",
        MODE == 8 ~ "8-Commuter Rail",
        MODE == 9 ~ "9-BRT",
        TRUE ~ "None"
      ),  
      FT_Name = case_when(
        FT_2019 == 1 ~ "1-Centroid Connector",
        FT_2019 == 2 ~ "2-Principal Arterial",
        FT_2019 == 3 ~ "3-Minor Arterial",
        FT_2019 == 4 ~ "4-Major Collector",
        FT_2019 == 5 ~ "5-Minor Collector",
        FT_2019 == 13 ~ "13-Expressway (55-65mph)",
        FT_2019 == 14 ~ "14-Expressway (50-60mph)",
        FT_2019 == 15 ~ "15-Expressway (45-55mph)",
        FT_2019 == 30 ~ "30-Fwy: fwy-to-fwy loop ramp",
        FT_2019 == 31 ~ "31-Fwy: C-D road, flyover ramp",
        FT_2019 == 33 ~ "33-Fwy: (65mph), no aux lane",
        FT_2019 == 34 ~ "34-Fwy: (65pmh), aux lane",
        FT_2019 == 35 ~ "35-Fwy: (75pmh), no aux lane",
        FT_2019 == 36 ~ "36-Fwy: (75mph), aux lane",
        FT_2019 == 38 ~ "38-Fwy: Managed lane",
        FT_2019 == 39 ~ "39-Fwy: Managed lane access",
        FT_2019 == 41 ~ "41-Fwy: On-ramp",
        FT_2019 == 42 ~ "42-Fwy: Off-ramp",
        TRUE ~ "None"
      )) %>%
    mutate(
      FTG_2019 = case_when(
        FT_2019 %in% c(20:26,30:40) ~ 30,
        FT_2019 %in% c(28:29,41:42) ~ 40,
        FT_2019 == 3 & AREATYPE %in% c(4,5) ~ 3,
        FT_2019 == 3  & AREATYPE %in% c(1,2,3) ~ 4,
        FT_2019 %in% c(1,4:8) ~ 1,
        FT_2019 %in% c(2,13:15) ~  2,
        TRUE ~ 0 
    )) %>%
    mutate(
      FTG_Name = case_when(
        FTG_2019 == 30 ~ "FT-Freeways",
        FTG_2019 == 40 ~ "FT-Ramps",
        FTG_2019 == 3 ~ "FT-Minor Arterials (Urban)",
        FTG_2019 == 4 ~ "FT-Minor Arterials (Suburban)",
        FTG_2019 == 1 ~ "FT-Collectors & Locals",
        FTG_2019 == 2 ~ "FT-Principal Arterials & Expressways",
        TRUE ~ "None"
    )) %>%
    filter(FTG_2019 != 0) %>%
    group_by({{ft_grouping_col}}, MODE) %>%
    arrange(-Modeled) %>%
    mutate(link_seq = row_number())
}

mapPlots <- function(jointspeeds,ft_group,func){
  fttypes <- list()
  if(ft_group == "FT"){
    fttypes <- list(1,2,3,4,5,14,15,30,31,33,34,35,36,38,39,41,42)
  } else {
    fttypes <- list(1,2,3,4,30,40)
  }
  
  ftplots <- list()
  for (f in 1:length(fttypes)){
    ftplots[[f]] = func(jointspeeds,ft_group,fttypes[[f]])
    #ftplots[[f]] = errorScatterPlotter(joint_estimated_speeds,"FT_2019",fttypes[[f]])
  }
  listPlots(ftplots,ft_group)
}

makePNGs <- function(plots, location){
  file_names <- stringr::str_c(names(plots), ".png")
  pwalk(list(file_names, plots),ggsave,width=8,height=6, path = location)
}

listPlots <- function(ftplots, ft_group){
  if(ft_group == "FT"){
    list("1_FT_2019" = ftplots[[1]], "2_FT_2019" = ftplots[[2]], "3_FT_2019" = ftplots[[3]],
         "4_FT_2019" = ftplots[[4]], "5_FT_2019" = ftplots[[5]], #"13_FT_2019" = ftplots[[1]],
         "14_FT_2019" = ftplots[[6]], "15_FT_2019" = ftplots[[7]], "30_FT_2019" = ftplots[[8]],
         "31_FT_2019" = ftplots[[9]], "33_FT_2019" = ftplots[[10]], "34_FT_2019" = ftplots[[11]],
         "35_FT_2019" = ftplots[[12]], "36_FT_2019" = ftplots[[13]], "38_FT_2019" = ftplots[[14]],
         "39_FT_2019" = ftplots[[15]], "41_FT_2019" = ftplots[[16]], "42_FT_2019" = ftplots[[17]])
  } else{
    list("1_FT_2019" = ftplots[[1]], "2_FT_2019" = ftplots[[2]], "3_FT_2019" = ftplots[[3]],
         "4_FT_2019" = ftplots[[4]], "30_FT_2019" = ftplots[[5]], "40_FT_2019" = ftplots[[6]])
  }
}


descLinePlotter <- function(jointspeeds, ft_group, FTNUM){
  jointspeeds2 <- if(ft_group == "FT"){
    jointspeeds %>% 
      filter(FT_2019 == FTNUM) %>% as.tibble()  %>%
      select(link_id,link_seq,FT_2019,FT_Name,MODE,MODE_Name,Observed,Modeled)  %>% 
      pivot_longer(!c(link_id,FT_2019,FT_Name,MODE,MODE_Name,link_seq),names_to = "Type",values_to = "Speed") %>%
      arrange(link_seq) %>%  mutate(Type = factor(Type, levels = c("Observed","Modeled")))
  } else{
    jointspeeds %>% 
      filter(FTG_2019 == FTNUM) %>% as.tibble()  %>%
      select(link_id,link_seq,FTG_2019,FTG_Name,MODE,MODE_Name,Observed,Modeled)  %>% 
      pivot_longer(!c(link_id,FTG_2019,FTG_Name,MODE,MODE_Name,link_seq),names_to = "Type",values_to = "Speed") %>%
      arrange(link_seq) %>%  mutate(Type = factor(Type, levels = c("Observed","Modeled")))
  }
  
  ftTitle <- if(ft_group == "FT"){
    jointspeeds2$FT_Name[1]
  } else{
    jointspeeds2$FTG_Name[1]
  }
  
  ggplot(jointspeeds2, aes(x = link_seq, y = Speed, fill = Type))+
    facet_wrap(~MODE_Name, scales = "free") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    geom_col(alpha = .25, position = "dodge2")+     
    geom_line(aes(x = link_seq,y = Speed, colour = Type))+
    scale_color_manual(values = c("red", "blue")) +
    scale_fill_manual(values = c("red", "blue")) +
    xlab("Links by Descending Modeled Speed") + ylab("Speed (mph)") +
    ggtitle(paste0("Bus Speeds Comparison for '",ftTitle, "' by Mode")) +
    theme()+
    theme_bw()
}


descScatterPlotter <- function(jointspeeds,ft_group,FTNUM){
  jointspeeds2 <- if(ft_group == "FT"){
    jointspeeds %>% 
      filter(FT_2019 == FTNUM) %>% as.tibble()  %>%
      select(link_id,link_seq,FT_2019,FT_Name,MODE,MODE_Name,Observed,Modeled)  %>% 
      pivot_longer(!c(link_id,FT_2019,FT_Name,MODE,MODE_Name,link_seq),names_to = "Type",values_to = "Speed") %>%
      arrange(link_seq) %>%  mutate(Type = factor(Type, levels = c("Observed","Modeled")))
  } else{
    jointspeeds %>% 
      filter(FTG_2019 == FTNUM) %>% as.tibble()  %>%
      select(link_id,link_seq,FTG_2019,FTG_Name,MODE,MODE_Name,Observed,Modeled)  %>% 
      pivot_longer(!c(link_id,FTG_2019,FTG_Name,MODE,MODE_Name,link_seq),names_to = "Type",values_to = "Speed") %>%
      arrange(link_seq) %>%  mutate(Type = factor(Type, levels = c("Observed","Modeled")))
  }
  
  ftTitle <- if(ft_group == "FT"){
    jointspeeds2$FT_Name[1]
  } else{
   jointspeeds2$FTG_Name[1]
  }  
  
  ggplot(jointspeeds2)+
    facet_wrap(~MODE_Name, scales = "free") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    geom_point(aes(x = link_seq, y = Speed, color = Type, fill = Type, alpha = .1)) +
    scale_color_manual(values = c("pink", "lightblue")) +
    ggnewscale::new_scale_color() +
    geom_smooth(aes(x = link_seq, y = Speed, color = Type, fill = Type, alpha = .1)) +
    scale_color_manual(values = c("red", "blue")) +
    scale_fill_manual(values = c("red", "blue")) +
    xlab("Links by Descending Modeled Speed") + ylab("Speed (mph)") +
    ggtitle(paste0("Bus Speeds Comparison for '",ftTitle, "' by Mode")) +
    theme()+
    theme_bw()
}

aveScatterPlotter <- function(jointspeeds,ft_group,FTNUM){
  jointspeeds2 <- if(ft_group == "FT"){
    jointspeeds %>% 
    filter(FT_2019 == FTNUM) %>% as.tibble()  %>%
    select(link_id,link_seq,FT_2019,FT_Name,MODE,MODE_Name,Observed,Modeled)
  } else {
    jointspeeds %>% 
      filter(FTG_2019 == FTNUM) %>% as.tibble()  %>%
      select(link_id,link_seq,FTG_2019,FTG_Name,MODE,MODE_Name,Observed,Modeled)
  }
  
  ftTitle <- if(ft_group == "FT"){
    jointspeeds2$FT_Name[1]
  } else{
    jointspeeds2$FTG_Name[1]
  }
  
  facetlims <- if(ft_group == "FT"){
    jointspeeds2 %>% as.tibble() %>%
    group_by(MODE_Name) %>% 
    summarise(min = 0, max = max(Observed, Modeled)) %>%
    gather(range, Observed, -c(MODE_Name)) %>%
    mutate(Modeled = Observed, range = NULL)
  } else{
    jointspeeds2 %>% as.tibble() %>%
      group_by(MODE_Name) %>% 
      summarise(min = 0, max = max(Observed, Modeled)) %>%
      gather(range, Observed, -c(MODE_Name)) %>%
      mutate(Modeled = Observed, range = NULL)
  }
  
  ggplot(jointspeeds2,aes(x = Modeled, y = Observed))+
    geom_point(aes(color = "Observed"), alpha = 1) +
    facet_wrap(~MODE_Name, scales = "free") +
    geom_blank(data = facetlims) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_abline(intercept = 0, slope = 1, alpha = 1, size = .5, color = "blue") +
    scale_color_manual(values = c("lightcoral")) +
    guides(color = guide_legend(title = "Type")) +
    ggnewscale::new_scale_color() +
    geom_smooth(method = "lm",formula=y~x+0,fullrange = T,se = F,aes(color = "Observed"), alpha = 1, size = 1, linetype = "longdash") +
    scale_color_manual(values = c("red")) +
    guides(fill = guide_legend(override.aes = list(color = NA)), color = FALSE, shape = FALSE) +
    xlab("TDM Modeled Speed (mph)") + ylab("UTA Observed Speed (mph)") +
    ggtitle(paste0("Bus Speeds Comparison for '",ftTitle, "' by Mode")) +
    theme()+
    theme_bw()
}

errorScatterPlotter <- function(jointspeeds,ft_group,FTNUM){
  jointspeeds2 <- if(ft_group == "FT"){
    jointspeeds %>% 
      filter(FT_2019 == FTNUM) %>% as.tibble()  %>%
      select(link_id,link_seq,FT_2019,FT_Name,FTG_Name,MODE,MODE_Name,Observed,Modeled,PercentError)
  } else {
    jointspeeds %>% 
      filter(FTG_2019 == FTNUM) %>% as.tibble()  %>%
      select(link_id,link_seq,FTG_2019,FT_Name,FTG_Name,MODE,MODE_Name,Observed,Modeled,PercentError)
  }
  
  ftTitle <- if(ft_group == "FT"){
    jointspeeds2$FT_Name[1]
  } else{
    jointspeeds2$FTG_Name[1]
  }
  
  ggplot(jointspeeds2) +
    facet_wrap(~MODE_Name, scales = "free_x") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_point(aes(x = Modeled, y = PercentError*100,color = "Observed"), alpha = 1) +
    scale_color_manual(values = c("lightcoral")) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue", size = 1) +
    xlab("TDM Modeled Speed (mph)") + ylab("Percent Error of UTA Observed Speed") +
    ggtitle(paste0("UTA Bus Speeds Percent Error for '",ftTitle, "' by Mode")) +
    scale_color_discrete(guide="none") +
    theme_bw()
}


#test <- errorScatterPlotter(joint_estimated_speeds,FTG_2019,"FTG",30)
#test2 <- mapPlots(joint_estimated_speeds,FTG_2019,"FTG",errorScatterPlotter)
#test


