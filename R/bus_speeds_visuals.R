
# Visual Objects ------------------------------------------------------------------#
join_estimated_speeds <- function(pk_0_es, pk_1_es, ok_0_es, ok_1_es){
  bind_rows(pk_0_es, pk_1_es, ok_0_es, ok_1_es) %>%
    filter(!is.na(EstAvgmphdwell) & !is.na(ModelSpeed)) %>%
    group_by(FT_2021, MODE) %>%
    arrange(-ModelSpeed) %>%
    mutate(link_seq = row_number()) %>%
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
        FT_2021 == 1 ~ "1-Centroid Connector",
        FT_2021 == 2 ~ "2-Principal Arterial",
        FT_2021 == 3 ~ "3-Minor Arterial",
        FT_2021 == 4 ~ "4-Major Collector",
        FT_2021 == 5 ~ "5-Minor Collector",
        FT_2021 == 13 ~ "13-Expressway (55-65mph)",
        FT_2021 == 14 ~ "14-Expressway (50-60mph)",
        FT_2021 == 15 ~ "15-Expressway (45-55mph)",
        FT_2021 == 30 ~ "30-Fwy: fwy-to-fwy loop ramp",
        FT_2021 == 31 ~ "31-Fwy: C-D road, flyover ramp",
        FT_2021 == 33 ~ "33-Fwy: (65mph), no aux lane",
        FT_2021 == 34 ~ "34-Fwy: (65pmh), aux lane",
        FT_2021 == 35 ~ "35-Fwy: (75pmh), no aux lane",
        FT_2021 == 36 ~ "36-Fwy: (75mph), aux lane",
        FT_2021 == 38 ~ "38-Fwy: Managed lane",
        FT_2021 == 39 ~ "39-Fwy: Managed lane access",
        FT_2021 == 41 ~ "41-Fwy: On-ramp",
        FT_2021 == 42 ~ "42-Fwy: Off-ramp",
        TRUE ~ "None"
      ))
}

mapDescLineGraphs <- function(jointspeeds){
  ft1 <- descLinePlotter(jointspeeds, 1)
  ft2 <- descLinePlotter(jointspeeds, 2)
  ft3 <- descLinePlotter(jointspeeds, 3)
  ft4 <- descLinePlotter(jointspeeds, 4)
  ft5 <- descLinePlotter(jointspeeds, 5)
  #ft13 <- descLinePlotter(jointspeeds, 13)
  ft14 <- descLinePlotter(jointspeeds, 14)
  ft15 <- descLinePlotter(jointspeeds, 15)
  ft30 <- descLinePlotter(jointspeeds, 30)
  ft31 <- descLinePlotter(jointspeeds, 31)
  ft33 <- descLinePlotter(jointspeeds, 33)
  ft34 <- descLinePlotter(jointspeeds, 34)
  ft35 <- descLinePlotter(jointspeeds, 35)
  ft36 <- descLinePlotter(jointspeeds, 36)
  ft38 <- descLinePlotter(jointspeeds, 38)
  ft39 <- descLinePlotter(jointspeeds, 39)
  ft41 <- descLinePlotter(jointspeeds, 41)
  ft42 <- descLinePlotter(jointspeeds, 42)
  
  plots <- list("1_FT_2021" = ft1, "2_FT_2021" = ft2, "3_FT_2021" = ft3,
                "4_FT_2021" = ft4, "5_FT_2021" = ft5, #"13_FT_2021" = ft13,
                "14_FT_2021" = ft14, "15_FT_2021" = ft15, "30_FT_2021" = ft30,
                "31_FT_2021" = ft31, "33_FT_2021" = ft33, "34_FT_2021" = ft34,
                "35_FT_2021" = ft35, "36_FT_2021" = ft36, "38_FT_2021" = ft38,
                "39_FT_2021" = ft39, "41_FT_2021" = ft41, "42_FT_2021" = ft42)
  plots
}

mapDescScatterPlots <- function(jointspeeds){
  ft1 <- descScatterPlotter(jointspeeds, 1)
  ft2 <- descScatterPlotter(jointspeeds, 2)
  ft3 <- descScatterPlotter(jointspeeds, 3)
  ft4 <- descScatterPlotter(jointspeeds, 4)
  ft5 <- descScatterPlotter(jointspeeds, 5)
  #ft13 <- descScatterPlotter(jointspeeds, 13)
  ft14 <- descScatterPlotter(jointspeeds, 14)
  ft15 <- descScatterPlotter(jointspeeds, 15)
  ft30 <- descScatterPlotter(jointspeeds, 30)
  ft31 <- descScatterPlotter(jointspeeds, 31)
  ft33 <- descScatterPlotter(jointspeeds, 33)
  ft34 <- descScatterPlotter(jointspeeds, 34)
  ft35 <- descScatterPlotter(jointspeeds, 35)
  ft36 <- descScatterPlotter(jointspeeds, 36)
  ft38 <- descScatterPlotter(jointspeeds, 38)
  ft39 <- descScatterPlotter(jointspeeds, 39)
  ft41 <- descScatterPlotter(jointspeeds, 41)
  ft42 <- descScatterPlotter(jointspeeds, 42)
  
  plots <- list("1_FT_2021" = ft1, "2_FT_2021" = ft2, "3_FT_2021" = ft3,
                "4_FT_2021" = ft4, "5_FT_2021" = ft5, #"13_FT_2021" = ft13,
                "14_FT_2021" = ft14, "15_FT_2021" = ft15, "30_FT_2021" = ft30,
                "31_FT_2021" = ft31, "33_FT_2021" = ft33, "34_FT_2021" = ft34,
                "35_FT_2021" = ft35, "36_FT_2021" = ft36, "38_FT_2021" = ft38,
                "39_FT_2021" = ft39, "41_FT_2021" = ft41, "42_FT_2021" = ft42)
  plots
}

mapAveScatterPlots <- function(jointspeeds){
  ft1 <- aveScatterPlotter(jointspeeds, 1)
  ft2 <- aveScatterPlotter(jointspeeds, 2)
  ft3 <- aveScatterPlotter(jointspeeds, 3)
  ft4 <- aveScatterPlotter(jointspeeds, 4)
  ft5 <- aveScatterPlotter(jointspeeds, 5)
  #ft13 <- aveScatterPlotter(jointspeeds, 13)
  ft14 <- aveScatterPlotter(jointspeeds, 14)
  ft15 <- aveScatterPlotter(jointspeeds, 15)
  ft30 <- aveScatterPlotter(jointspeeds, 30)
  ft31 <- aveScatterPlotter(jointspeeds, 31)
  ft33 <- aveScatterPlotter(jointspeeds, 33)
  ft34 <- aveScatterPlotter(jointspeeds, 34)
  ft35 <- aveScatterPlotter(jointspeeds, 35)
  ft36 <- aveScatterPlotter(jointspeeds, 36)
  ft38 <- aveScatterPlotter(jointspeeds, 38)
  ft39 <- aveScatterPlotter(jointspeeds, 39)
  ft41 <- aveScatterPlotter(jointspeeds, 41)
  ft42 <- aveScatterPlotter(jointspeeds, 42)
  
  plots <- list("1_FT_2021" = ft1, "2_FT_2021" = ft2, "3_FT_2021" = ft3,
                "4_FT_2021" = ft4, "5_FT_2021" = ft5, #"13_FT_2021" = ft13,
                "14_FT_2021" = ft14, "15_FT_2021" = ft15, "30_FT_2021" = ft30,
                "31_FT_2021" = ft31, "33_FT_2021" = ft33, "34_FT_2021" = ft34,
                "35_FT_2021" = ft35, "36_FT_2021" = ft36, "38_FT_2021" = ft38,
                "39_FT_2021" = ft39, "41_FT_2021" = ft41, "42_FT_2021" = ft42)
  plots
}

mapErrScatterPlots <- function(jointspeeds){
  ft1 <- errorScatterPlotter(jointspeeds, 1)
  ft2 <- errorScatterPlotter(jointspeeds, 2)
  ft3 <- errorScatterPlotter(jointspeeds, 3)
  ft4 <- errorScatterPlotter(jointspeeds, 4)
  ft5 <- errorScatterPlotter(jointspeeds, 5)
  #ft13 <- errorScatterPlotter(jointspeeds, 13)
  ft14 <- errorScatterPlotter(jointspeeds, 14)
  ft15 <- errorScatterPlotter(jointspeeds, 15)
  ft30 <- errorScatterPlotter(jointspeeds, 30)
  ft31 <- errorScatterPlotter(jointspeeds, 31)
  ft33 <- errorScatterPlotter(jointspeeds, 33)
  ft34 <- errorScatterPlotter(jointspeeds, 34)
  ft35 <- errorScatterPlotter(jointspeeds, 35)
  ft36 <- errorScatterPlotter(jointspeeds, 36)
  ft38 <- errorScatterPlotter(jointspeeds, 38)
  ft39 <- errorScatterPlotter(jointspeeds, 39)
  ft41 <- errorScatterPlotter(jointspeeds, 41)
  ft42 <- errorScatterPlotter(jointspeeds, 42)
  
  plots <- list("1_FT_2021" = ft1, "2_FT_2021" = ft2, "3_FT_2021" = ft3,
                "4_FT_2021" = ft4, "5_FT_2021" = ft5, #"13_FT_2021" = ft13,
                "14_FT_2021" = ft14, "15_FT_2021" = ft15, "30_FT_2021" = ft30,
                "31_FT_2021" = ft31, "33_FT_2021" = ft33, "34_FT_2021" = ft34,
                "35_FT_2021" = ft35, "36_FT_2021" = ft36, "38_FT_2021" = ft38,
                "39_FT_2021" = ft39, "41_FT_2021" = ft41, "42_FT_2021" = ft42)
  plots
}

makePNGs <- function(plots, location){
  file_names <- stringr::str_c(names(plots), ".png")
  pwalk(list(file_names, plots),ggsave,width=8,height=6, path = location)
}


descLinePlotter <- function(jointspeeds, FTNUM){
  jointspeeds2 <- jointspeeds %>% 
    filter(FT_2021 == as.numeric(FTNUM)) %>% as.tibble()  %>%
    select(link_id,link_seq,FT_2021,FT_Name,MODE,MODE_Name,Observed,Modeled)  %>% 
    pivot_longer(!c(link_id,FT_2021,FT_Name,MODE,MODE_Name,link_seq),names_to = "Type",values_to = "Speed") %>%
    arrange(link_seq) %>%  mutate(Type = factor(Type, levels = c("Observed","Modeled")))
  
  ggplot(jointspeeds2, aes(x = link_seq, y = Speed, fill = Type))+
    facet_wrap(~MODE_Name, scales = "free") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    geom_col(alpha = .25, position = "dodge2")+     
    geom_line(aes(x = link_seq,y = Speed, colour = Type))+
    scale_color_manual(values = c("red", "blue")) +
    scale_fill_manual(values = c("red", "blue")) +
    xlab("Links by Descending Modeled Speed") + ylab("Speed (mph)") +
    ggtitle(paste0("Bus Speeds Comparison for '",jointspeeds2$FT_Name[1], "' by Mode")) +
    theme()+
    theme_bw()
}


descScatterPlotter <- function(jointspeeds,FTNUM){
  jointspeeds2 <- jointspeeds %>% 
    filter(FT_2021 == FTNUM) %>% as.tibble()  %>%
    select(link_id,link_seq,FT_2021,FT_Name,MODE,MODE_Name,Observed,Modeled)  %>% 
    pivot_longer(!c(link_id,FT_2021,FT_Name,MODE,MODE_Name,link_seq),names_to = "Type",values_to = "Speed") %>%
    arrange(link_seq) %>%  mutate(Type = factor(Type, levels = c("Observed","Modeled")))
  
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
    ggtitle(paste0("Bus Speeds Comparison for '",jointspeeds2$FT_Name[1], "' by Mode")) +
    theme()+
    theme_bw()
}

aveScatterPlotter <- function(jointspeeds,FTNUM){
  jointspeeds2 <- jointspeeds %>% 
    filter(FT_2021 == FTNUM) %>% as.tibble()  %>%
    select(link_id,link_seq,FT_2021,FT_Name,MODE,MODE_Name,Observed,Modeled)
  
  ggplot(jointspeeds2)+
    facet_wrap(~MODE_Name, scales = "free") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_point(aes(x = Modeled, y = Observed,color = "Observed"), alpha = 1) +
    geom_line(aes(x = Modeled, y = Modeled,color = "Modeled"), alpha = 1, size = 1) +
    scale_color_manual(values = c("blue", "pink")) +
    guides(color = guide_legend(title = "Type")) +
    ggnewscale::new_scale_color() +
    geom_smooth(method = "lm",se = F,aes(x = Modeled, y = Observed, color = "Observed"), alpha = 1, size = 1, linetype = "longdash") +
    scale_color_manual(values = c("red")) +
    guides(fill = guide_legend(override.aes = list(color = NA)), color = FALSE, shape = FALSE) +
    xlab("TDM Modeled Speed (mph)") + ylab("UTA Observed Speed (mph)") +
    ggtitle(paste0("Bus Speeds Comparison for '",jointspeeds2$FT_Name[1], "' by Mode")) +
    theme()+
    theme_bw()
}

errorScatterPlotter <- function(jointspeeds,FTNUM){
  jointspeeds2 <- jointspeeds %>% 
    filter(FT_2021 == FTNUM) %>% as.tibble()  %>%
    select(link_id,link_seq,FT_2021,FT_Name,MODE,MODE_Name,Observed,Modeled,PercentError)
  
  ggplot(jointspeeds2)+
    facet_wrap(~MODE_Name, scales = "free_x") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    geom_point(aes(x = Modeled, y = PercentError*100,color = "Observed"), alpha = 1) +
    scale_color_manual(values = c("lightcoral")) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue", size = 1) +
    xlab("TDM Modeled Speed (mph)") + ylab("Percent Error of UTA Observed Speed") +
    ggtitle(paste0("UTA Bus Speeds Percent Error for '",jointspeeds2$FT_Name[1], "' by Mode")) +
    scale_color_discrete(guide="none") +
    theme_bw()
}






