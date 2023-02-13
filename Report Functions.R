barplot_county <- function(varlist, graph_title) {
  
  Data_frame_values <- data.frame(matrix(ncol=5,nrow=2*length(varlist)))
  colnames(Data_frame_values) <- c("County", "Indicator", "Value", "ci_l", "ci_u")
  
  Data_frame_values$County <- c(rep("NM", length(varlist)), rep({params$counties}, length(varlist)))
  Data_frame_values$County <- factor(Data_frame_values$County, levels = c({params$counties}, "NM"))
  

  
  for(i in 1:length(varlist)){
    varname <- varlist[i]
    Data_frame_values[i,2] <- var_label(yrbss_NM[varname])
    Data_frame_values[i+ length(varlist),2] <- var_label(yrbss_NM[varname])

    #Get values for state level data
    Data_frame_values[i,3] <- (svymean(yrbss_NM[varname], yrrs_nm, na.rm=T))[1]
    Data_frame_values[i,4:5] <-(confint(svymean(yrbss_NM[varname], yrrs_nm, na.rm=T)))[1:2]

    #Get values for county level data
    Data_frame_values[i + length(varlist),3:5] <- (svyby(as.formula( paste0("~", varname)), ~cntytxt=={params$counties}, yrrs_cnty, svyciprop, vartype ="ci", df = Design_df, na.rm=T))[2,2:4]

    ## Erik's comments 1/31/23
    #Use foreach loop to parallelize code
      # Data_frame_values <- data.frame(matrix(ncol=5,nrow=2))
      # colnames(Data_frame_values) <- c("County", "Indicator", "Value", "ci_l", "ci_u")
      # 
      # Data_frame_values$County <- c(rep("NM", length(varlist)), rep({params$counties}, length(varlist)))
      # Data_frame_values$County <- factor(Data_frame_values$County, levels = c({params$counties}, "NM"))
      # 
      # varname <- varlist[i]
      # Data_frame_values[,2] <- var_label(yrbss_NM[varname])
      # 
      # 
      # #Get values for state level data
      # Data_frame_values[1,3] <- (svymean(yrbss_NM[varname], yrrs_nm, na.rm=T))[1]
      # Data_frame_values[1,4:5] <-(confint(svymean(yrbss_NM[varname], yrrs_nm, na.rm=T)))[1:2]
      # 
      # out <- (svyby(as.formula( paste0("~", varname)), ~cntytxt=={params$counties}, yrrs_cnty, svyciprop, vartype ="ci", df = Design_df, na.rm=T))    
      # Data_frame_values[2,3:5] <- out[2,2:4]
      ###########################################
      
  }
  
  #Optimized for parallel computing version; get Data_frame_values dataframe for each variable, then combine all into 
  #one for list of variables
  
  Data_frame_values$Indicator <- factor(Data_frame_values$Indicator, levels = var_label(yrbss_NM[varlist]))
  #Data_frame_values <- data.frame(County, Indicator, Value, ci_l, ci_u)
  
  new_plot <- ggplot(Data_frame_values, aes(y = Indicator, x = Value, fill = County)) + 
    geom_col(position="dodge") + theme(legend.position="top", legend.title = element_blank(), legend.key.size = unit(0.25, 'cm'), plot.subtitle = element_text(face="italic"), panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title.position = "plot")
  
  new_plot <- new_plot + geom_text(aes(group=County, label= scales::percent(Data_frame_values$Value, accuracy=0.1),x=ci_u), hjust=-.1, color="black", position = position_dodge(.9), size=4) + 
    
    labs(x = "Percent (%)", y = (element_blank()), subtitle = paste0({{graph_title}}, "\nNew Mexico & ",{params$counties}," County\nGrades 9-12, ",{params$year})) +
    
    geom_errorbar(aes(y=Indicator, xmin=ci_l, xmax=ci_u), width=.2, position= position_dodge(.9), alpha=0.7)
  
  new_plot <- new_plot + scale_fill_manual(values = c("#F68C43", "#753F98"), guide = guide_legend(reverse = TRUE)) + 
    scale_y_discrete(labels = str_wrap(Data_frame_values$Indicator, width = 15)) + coord_cartesian(clip = 'off')
  
  #Limit the width of the graph to 50% if all values are below 50%
  greater_than_50 <- FALSE
  for (p in 1:length(Data_frame_values$Value)){
    
    if (Data_frame_values[p,3] > 0.5|| Data_frame_values[p,5]> 0.5) {
      new_plot <- new_plot + scale_x_continuous(labels = scales::percent_format(scale = 100),
                                                limits = c(0,1), breaks = seq(0, 1, .25))
      greater_than_50 <- TRUE
      break
    }
  }
  if (greater_than_50 ==FALSE){
    new_plot <- new_plot + scale_x_continuous(labels = scales::percent_format(scale = 100),
                                              limits = c(0,.5), breaks = seq(0, 1, .25))
  }
  
  return(new_plot)
}

barplot_district <- function(varlist, graph_title) {
  
  Data_frame_values <- data.frame(matrix(ncol=5,nrow=2*length(varlist)))
  colnames(Data_frame_values) <- c("District", "Indicator", "Value", "ci_l", "ci_u")
  
  Data_frame_values$District <- c(rep("NM", length(varlist)), rep({params$District_name}, length(varlist)))
  Data_frame_values$District <- factor(Data_frame_values$District, levels = c({params$District_name}, "NM"))
  
  
  
  for(i in 1:length(varlist)){
    varname <- varlist[i]
    Data_frame_values[i,2] <- var_label(yrbss_NM[varname])
    Data_frame_values[i+ length(varlist),2] <- var_label(yrbss_NM[varname])
    
    #Get values for state level data
    Data_frame_values[i,3] <- (svymean(yrbss_NM[varname], yrrs_nm, na.rm=T,multicore=TRUE))[1]
    Data_frame_values[i,4:5] <-(confint(svymean(yrbss_NM[varname], yrrs_nm, na.rm=T,multicore=TRUE)))[1:2]
    
    #Get values for district level data
    Data_frame_values[i + length(varlist),3:5] <- (svyby(as.formula( paste0("~", varname)), ~sdid=={params$District}, yrrs_nm, svyciprop, vartype ="ci", na.rm=T,multicore=TRUE))[2,2:4]
    }
  
  Data_frame_values$Indicator <- factor(Data_frame_values$Indicator, levels = var_label(yrbss_NM[varlist]))
  
  new_plot <- ggplot(Data_frame_values, aes(y = Indicator, x = Value, fill = District)) + 
    geom_col(position="dodge") + theme(legend.position="top", legend.title = element_blank(), legend.key.size = unit(0.25, 'cm'), plot.subtitle = element_text(face="italic"), panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title.position = "plot")
  
  new_plot <- new_plot + geom_text(aes(group=District, label= scales::percent(Data_frame_values$Value, accuracy=0.1),x=ci_u), hjust=-.1, color="black", position = position_dodge(.9), size=4) + 
    
    labs(x = "Percent (%)", y = (element_blank()), subtitle = paste0({{graph_title}}, "\nNew Mexico & ",{params$District_name},"\nGrades 9-12, ",{params$year})) +
    
    geom_errorbar(aes(y=Indicator, xmin=ci_l, xmax=ci_u), width=.2, position= position_dodge(.9), alpha=0.7)
  
  new_plot <- new_plot + scale_fill_manual(values = c("#F68C43", "#753F98"), guide = guide_legend(reverse = TRUE)) + 
    scale_y_discrete(labels = str_wrap(Data_frame_values$Indicator, width = 15)) + coord_cartesian(clip = 'off')
  
  #Limit the width of the graph to 50% if all values are below 50%
  greater_than_50 <- FALSE
  for (p in 1:length(Data_frame_values$Value)){
    
    if (Data_frame_values[p,3] > 0.5|| Data_frame_values[p,5]> 0.5) {
      new_plot <- new_plot + scale_x_continuous(labels = scales::percent_format(scale = 100),
                                                limits = c(0,1), breaks = seq(0, 1, .25))
      greater_than_50 <- TRUE
      break
    }
  }
  if (greater_than_50 ==FALSE){
    new_plot <- new_plot + scale_x_continuous(labels = scales::percent_format(scale = 100),
                                              limits = c(0,.5), breaks = seq(0, 1, .25))
  }
  
  return(new_plot)
}


trendplot <- function(varlist, graph_title=NULL, graph_subtitle=NULL, grouped=FALSE, district_level=FALSE){
  
  Indicator <- c(rep(0, length(years)*length(varlist)))
  Value <- c(rep(0, length(years)*length(varlist)))
  survey <- c(rep(year_survey, length(varlist)))
  
  #year (as opposed to years) is list to be made into part of the table, not to be confused with years
  year <- c(rep(years, length(varlist)))
  
  if(district_level){
    for(i in 1:length(varlist)){
      varname <- varlist[i]
      
      
      for(j in 1:length(years)){
        currentyear = datasets[j]
        Indicator[j + (i-1)*length(years)] <- var_label(yrbss_NM[varname])[[1]]
        
        if(varname %in% colnames(datasets[[j]])){
          Value[j + (i-1)*length(years)] <- (svyby(as.formula( paste0("~", varname)), ~sdid=={params$District}, as.formula(noquote(year_survey[j])), svyciprop, vartype ="ci", na.rm=T,multicore=TRUE))[2,2]
        }
        
        else{
          Value[j + (i-1)*length(years)] <- NA
        }
      }
    }
  }
  
  else{
    for(i in 1:length(varlist)){
      varname <- varlist[i]
      
      
      for(j in 1:length(years)){
        currentyear = datasets[j]
        Indicator[j + (i-1)*length(years)] <- var_label(yrbss_NM[varname])[[1]]
        
        if(varname %in% colnames(datasets[[j]])){
          Value[j + (i-1)*length(years)] <- (svyby(as.formula( paste0("~", varname)), ~cntytxt=={params$counties}, as.formula(noquote(year_survey[j])), svyciprop, vartype ="ci", na.rm=T,multicore=TRUE))[2,2]
        }
        
        else{
          Value[j + (i-1)*length(years)] <- NA
        }
      }
    }
    
  }
  
  
  
  
  
  Data_frame_values <- data.frame(year, Indicator, Value)
  
  newplot <- ggplot(Data_frame_values, aes(x=year, y=Value, color=Indicator, group=Indicator)) 
  
  newplot <- newplot + geom_line(position = 'dodge', stat = 'identity', size=1) + theme_bw()
  
  newplot <- newplot + theme(
    legend.position="top", legend.justification = "right",legend.text.align = 0
    , plot.subtitle = element_text(face="italic"),
    legend.title = element_blank(), legend.background = element_blank(), panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title.position = "plot")
  
  newplot <- newplot + geom_point(size=2.5, aes(shape=Indicator))
  newplot <- newplot + scale_y_continuous(labels = scales::percent_format(scale = 100),
                                          limits = c(0,1), breaks = seq(0, 1, .25))
  
  #Reverse legend and wrap
  if(length(varlist) <= 3){
    newplot <- newplot + guides(color=guide_legend(nrow=1, byrow=TRUE, reverse=TRUE), shape = guide_legend(nrow=1, byrow=TRUE, reverse=TRUE))
    
  }
  if(length(varlist) >= 4){
    newplot <- newplot + guides(color=guide_legend(nrow=2, byrow=TRUE, reverse=TRUE), shape = guide_legend(nrow=2, byrow=TRUE, reverse=TRUE))
  }
  #Plot title and subtitle
  newplot <- newplot + labs(x = (element_blank()), y = "Percent (%)", title = {{graph_title}}, subtitle = 
                              paste0({{graph_subtitle}}, "\nby Year, ",
                                    ifelse(district_level,{params$District_name},{params$counties}),
                                    if(!district_level){" County"},
                                    "\nGrades 9-12, ",years[1], "-",{params$year}))
  
  #Text table
  gg.table <- ggplot(Data_frame_values, aes(x = year, y = Indicator, label = ifelse(!is.na(Value), format(round(Value*100, 1), nsmall=1), NA))) +
    geom_text(hjust=1, nudge_x=.2) +
    theme_bw() +
    scale_x_discrete(breaks = year) +
    scale_y_discrete(position = "left") +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position="none")
  
  #Adjust wording size
  if(grouped==TRUE){
    new_plot <- new_plot + theme(text = element_text(size=16, inherit.blank=TRUE))
    #gg.table <- gg.table + theme(text = element_text(size=16, inherit.blank=TRUE))
  }
  
  
  plot_and_years <- ggarrange(newplot, gg.table, ncol = 1, heights = c(4, 1), align = "v")
  return(plot_and_years)
  
}

barplot_resiliency <- function(risk_factor, Resiliency_factors_list, level){
  
  
  Resiliency_values <- data.frame(matrix(ncol = length(Resiliency_factors_list), nrow=3))
  colnames(Resiliency_values) <- Resiliency_factors_list
  rownames(Resiliency_values) <- c("Not true at all", "A little bit or pretty much true", "Very much true")
  
  
  #Populate data frame with values from dataset
  
  if(level == "region"){
    j = 1
    for (i in Resiliency_factors_list){
      
      #for region level
      Res_Table <- svyby(as.formula( paste0("~", risk_factor)), as.formula(paste0("~{region=={params$regions}}+", {{i}})), yrrs_nm, svyciprop, vartype ="ci", na.rm.all=T,multicore=TRUE)
      Resiliency_values[1:3, {{i}}] <- Res_Table[(Res_Table[1]==TRUE),3]
      colnames(Resiliency_values)[j] <- var_label(yrbss_NM[{{i}}])
      j <- j+1
    }
  }
  
  else if(level == "county"){
    j = 1
    for (i in Resiliency_factors_list){
      
      #for county level
      Res_Table <- svyby(as.formula( paste0("~", risk_factor)), as.formula(paste0("~{cntytxt=={params$counties}}+", {{i}})), yrrs_cnty, svyciprop, vartype ="ci", na.rm.all=T,multicore=TRUE)
      Resiliency_values[1:3, {{i}}] <- Res_Table[(Res_Table[1]==TRUE),3]
      colnames(Resiliency_values)[j] <- var_label(yrbss_NM[{{i}}])
      j <- j+1
    }  
  }
  
  else if(level == "district"){
    j = 1
    for (i in Resiliency_factors_list){
      
      #for region level
      Res_Table <- svyby(as.formula( paste0("~", risk_factor)), as.formula(paste0("~{sdid=={params$District}}+", {{i}})), yrrs_nm, svyciprop, vartype ="ci", na.rm.all=T,multicore=TRUE)
      Resiliency_values[1:3, {{i}}] <- Res_Table[(Res_Table[1]==TRUE),3]
      colnames(Resiliency_values)[j] <- var_label(yrbss_NM[{{i}}])
      j <- j+1
    }
  }
  
  
  #Melt new dataframe into long form and add labels to prepare for visualization          
  Resiliency_values[,"Resiliency"] <- row.names(Resiliency_values)          
  Melted_Resiliency_values <- data.frame(melt(Resiliency_values))
  Melted_Resiliency_values$Resiliency <- factor(Melted_Resiliency_values$Resiliency, levels = c("Very much true", "A little bit or pretty much true", "Not true at all"))
  Melted_Resiliency_values$variable <- factor(Melted_Resiliency_values$variable, levels = colnames(Resiliency_values))
  
  
  #Create the visualization          
  newplot <- ggplot(Melted_Resiliency_values, aes(x=value, y=variable, fill=Resiliency)) + 
    geom_bar(position="dodge", stat="identity")
  
  newplot <- newplot + theme(legend.position="top", legend.title = element_blank(), plot.subtitle = element_text(face="italic"), panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title.position = "plot", text = element_text(size=20))
  
  #Percentages next to bars 
  newplot <- newplot + geom_text(aes(group=Resiliency, label= scales::percent(value, accuracy=0.1)), hjust=-.1, color="black", position = position_dodge(.9), size=5)
  
  
  #Add title and subtitle
  
  # newplot <- newplot + case_when(
  #   level == "county" ~  labs(x = "Percent (%)", y = (element_blank()), title = var_label(yrbss_NM[risk_factor]), subtitle = paste0("by Selected Resiliency Factors\n",{params$counties},"County\nGrades 9-12, ",{params$year})),
  #   level == "region" ~ labs(x = "Percent (%)", y = (element_blank()), title = var_label(yrbss_NM[risk_factor]), subtitle = paste0("by Selected Resiliency Factors\n",{params$region},"\nGrades 9-12, ",{params$year})),
  #   level == "district" ~ labs(x = "Percent (%)", y = (element_blank()), title = var_label(yrbss_NM[risk_factor]), subtitle = paste0("by Selected Resiliency Factors\n",{params$District_name},"\nGrades 9-12, ",{params$year}))
  #   
  # )
  
  if(level == "county"){
    newplot <- newplot + labs(x = "Percent (%)", y = (element_blank()), title = var_label(yrbss_NM[risk_factor]), subtitle = paste0("by Selected Resiliency Factors\n",{params$counties},"County\nGrades 9-12, ",{params$year}))
  }
  else if(level == "region"){
    newplot <- newplot + labs(x = "Percent (%)", y = (element_blank()), title = var_label(yrbss_NM[risk_factor]), subtitle = paste0("by Selected Resiliency Factors\n",{params$region},"\nGrades 9-12, ",{params$year}))
  }
  else if(level == "district"){
    newplot <- newplot + labs(x = "Percent (%)", y = (element_blank()), title = var_label(yrbss_NM[risk_factor]), subtitle = paste0("by Selected Resiliency Factors\n",{params$District_name},"\nGrades 9-12, ",{params$year}))
  }
  
  #Wrap text, reverse legend and change colors
  newplot <- newplot + scale_y_discrete(labels = str_wrap(colnames(Resiliency_values), width = 45)) + scale_fill_manual(values = c("#aabad7", "#4f81bd", "#10253f"), guide = guide_legend(reverse = TRUE))
  
  newplot <- newplot + scale_x_continuous(labels = scales::percent_format(scale = 100),
                                          limits = c(0,1), breaks = seq(0, 1, .25))
  
  # #Limit the width of the graph to 50% if all values are below 50%
  # greater_than_50 <- FALSE
  # for (p in Melted_Resiliency_values$value){
  #   
  #   if (p > 0.5) {
  #     newplot <- newplot + scale_x_continuous(labels = scales::percent_format(scale = 100),
  #                    limits = c(0,1), breaks = seq(0, 1, .25))
  #     greater_than_50 <- TRUE
  #     break
  #   }
  # }
  # if (greater_than_50 ==FALSE){
  #     newplot <- newplot + scale_x_continuous(labels = scales::percent_format(scale = 100),
  #                    limits = c(0,.5), breaks = seq(0, 1, .25))
  # }
  
  #Global variables for use in text
  risk_props <<- Resiliency_values[1:3,length(Resiliency_factors_list)]%>% signif(3) *100
  
  return(newplot)
}

barplot_county_2 <- function(varlist, graph_title, grouped = FALSE) {
  Data_frame_values <- data.frame(matrix(ncol=5,nrow=2*length(varlist)))
  colnames(Data_frame_values) <- c("County", "Indicator", "Value", "ci_l", "ci_u")
  
  Data_frame_values$County <- c(rep("NM", length(varlist)), rep({params$counties}, length(varlist)))
  Data_frame_values$County <- factor(Data_frame_values$County, levels = c({params$counties}, "NM"))
  
  
  
  for(i in 1:length(varlist)){
    varname <- varlist[i]
    Data_frame_values[i,2] <- var_label(yrbss_NM[varname])
    Data_frame_values[i+ length(varlist),2] <- var_label(yrbss_NM[varname])
    
    #Get values for state level data
    Data_frame_values[i,3] <- (svymean(yrbss_NM[varname], yrrs_nm, na.rm=T,multicore=TRUE))[1]
    Data_frame_values[i,4:5] <-(confint(svymean(yrbss_NM[varname], yrrs_nm, na.rm=T,multicore=TRUE)))[1:2]
    
    #Get values for county level data
    Data_frame_values[i + length(varlist),3:5] <- (svyby(as.formula( paste0("~", varname)), ~cntytxt=={params$counties}, yrrs_cnty, svyciprop, vartype ="ci", df = Design_df, na.rm=T))[2,2:4]
  }
  
  Data_frame_values$Indicator <- factor(Data_frame_values$Indicator, levels = var_label(yrbss_NM[varlist]))
  

  new_plot <- ggplot(Data_frame_values, aes(x = Indicator, y = Value, fill = County)) + 
    geom_col(position="dodge") + 
    theme(
      #legend.key.size = unit(0.25, 'cm'),
      legend.position="top", legend.title = element_blank(),legend.justification = "right"
      , plot.subtitle = element_text(face="italic")
      , panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title.position = "plot") 
  
  if(grouped==TRUE){
    new_plot <- new_plot + geom_text(aes(group=County, label= scales::percent(Value, accuracy=0.1),y=ci_u), vjust= -1, color="black", position = position_dodge(.9), size=5)
  }
  if(grouped==FALSE){
    new_plot <- new_plot + geom_text(aes(group=County, label= scales::percent(Value, accuracy=0.1),y=ci_u), vjust= -1, color="black", position = position_dodge(.9), size=3)
  }
  
  
  new_plot <- new_plot + labs(x = (element_blank()), y = "Percent (%)", subtitle = paste0({{graph_title}}, "\nNew Mexico & ",{params$counties}," County\nGrades 9-12, ",{params$year})) +
    
    geom_errorbar(aes(x=Indicator, ymin=ci_l, ymax=ci_u), width=.1, position= position_dodge(.9), alpha=0.7) + 
    scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0,1), breaks = seq(0, 1, .25))+
    scale_fill_manual(values = c("#753F98", "#F68C43")) + coord_cartesian(clip = 'off')
  
  #Setting max length for labels depending on how many variables being used
  if(length(varlist) > 4){
    new_plot <- new_plot + scale_x_discrete(labels = str_wrap(Data_frame_values$Indicator, width = 15))
  }
  else{
    new_plot <- new_plot + scale_x_discrete(labels = str_wrap(Data_frame_values$Indicator, width = 20))
  }
  
  
  return(new_plot)
}

barplot_district_2 <- function(varlist, graph_title, grouped = FALSE) {
  Data_frame_values <- data.frame(matrix(ncol=5,nrow=2*length(varlist)))
  colnames(Data_frame_values) <- c("District", "Indicator", "Value", "ci_l", "ci_u")
  
  Data_frame_values$District <- c(rep("NM", length(varlist)), rep({params$District_name}, length(varlist)))
  Data_frame_values$District <- factor(Data_frame_values$District, levels = c({params$District_name}, "NM"))
  
  
  
  for(i in 1:length(varlist)){
    varname <- varlist[i]
    Data_frame_values[i,2] <- var_label(yrbss_NM[varname])
    Data_frame_values[i+ length(varlist),2] <- var_label(yrbss_NM[varname])
    
    #Get values for state level data
    Data_frame_values[i,3] <- (svymean(yrbss_NM[varname], yrrs_nm, na.rm=T,multicore=TRUE))[1]
    Data_frame_values[i,4:5] <-(confint(svymean(yrbss_NM[varname], yrrs_nm, na.rm=T,multicore=TRUE)))[1:2]
    
    #Get values for county level data
    Data_frame_values[i + length(varlist),3:5] <- (svyby(as.formula( paste0("~", varname)), ~sdid=={params$District}, yrrs_nm, svyciprop, vartype ="ci", na.rm=T))[2,2:4]
  }
  
  Data_frame_values$Indicator <- factor(Data_frame_values$Indicator, levels = var_label(yrbss_NM[varlist]))
  
  
  new_plot <- ggplot(Data_frame_values, aes(x = Indicator, y = Value, fill = District)) + 
    geom_col(position="dodge") + 
    theme(
      #legend.key.size = unit(0.25, 'cm'),
      legend.position="top", legend.title = element_blank(),legend.justification = "right"
      , plot.subtitle = element_text(face="italic")
      , panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title.position = "plot") 
  
  if(grouped==TRUE){
    new_plot <- new_plot + geom_text(aes(group=District, label= scales::percent(Value, accuracy=0.1),y=ci_u), vjust= -1, color="black", position = position_dodge(.9), size=5)
  }
  if(grouped==FALSE){
    new_plot <- new_plot + geom_text(aes(group=District, label= scales::percent(Value, accuracy=0.1),y=ci_u), vjust= -1, color="black", position = position_dodge(.9), size=3)
  }
  
  
  new_plot <- new_plot + labs(x = (element_blank()), y = "Percent (%)", subtitle = paste0({{graph_title}}, "\nNew Mexico & ",{params$District_name},"\nGrades 9-12, ",{params$year})) +
    
    geom_errorbar(aes(x=Indicator, ymin=ci_l, ymax=ci_u), width=.1, position= position_dodge(.9), alpha=0.7) + 
    scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0,1), breaks = seq(0, 1, .25))+
    scale_fill_manual(values = c("#753F98", "#F68C43")) + coord_cartesian(clip = 'off')
  
  #Setting max length for labels depending on how many variables being used
  if(length(varlist) > 4){
    new_plot <- new_plot + scale_x_discrete(labels = str_wrap(Data_frame_values$Indicator, width = 15))
  }
  else{
    new_plot <- new_plot + scale_x_discrete(labels = str_wrap(Data_frame_values$Indicator, width = 20))
  }
  
  
  return(new_plot)
}

barplot_sex <- function(varlist, graph_title, grouped = FALSE, level="county") {
  
  #Making a different table of values for graph split by sex
  
  Data_frame_values_Sex <- data.frame(matrix(ncol=5,nrow=2*length(varlist)))
  colnames(Data_frame_values_Sex) <- c("Sex", "Indicator", "Value", "ci_l", "ci_u")
  
  Data_frame_values_Sex$Sex <- c(rep("Female", length(varlist)), rep("Male", length(varlist)))
  Data_frame_values_Sex$Sex <- factor(Data_frame_values_Sex$Sex, levels = c("Female", "Male"))
  
  if(level=="county"){
    for(i in 1:length(varlist)){
      varname <- varlist[i]
      Data_frame_values_Sex[i,2] <- var_label(yrbss_NM[varname])
      Data_frame_values_Sex[i+ length(varlist),2] <- var_label(yrbss_NM[varname])
      
      #Get values for female students
      Data_frame_values_Sex[i,3:5] <-(svyby(as.formula( paste0("~", varname)), ~(cntytxt=={params$counties} & sex==1), yrrs_cnty, svyciprop, vartype ="ci", df=Design_df, na.rm=T,multicore=TRUE))[2,2:4] 
      
      #Get values for male students
      Data_frame_values_Sex[i + length(varlist),3:5] <- (svyby(as.formula( paste0("~", varname)), ~(cntytxt=={params$counties} & sex==2), yrrs_cnty, svyciprop, vartype ="ci", df=Design_df, na.rm=T,multicore=TRUE))[2,2:4] 
    }
  }
  
  else if(level=="district"){
    for(i in 1:length(varlist)){
      varname <- varlist[i]
      Data_frame_values_Sex[i,2] <- var_label(yrbss_NM[varname])
      Data_frame_values_Sex[i+ length(varlist),2] <- var_label(yrbss_NM[varname])
      
      #Get values for male students
      Data_frame_values_Sex[i,3:5] <-(svyby(as.formula( paste0("~", varname)), ~(sdid=={params$District} & sex==1), yrrs_nm, svyciprop, vartype ="ci", na.rm=T,multicore=TRUE))[2,2:4] 
      
      #Get values for female students
      Data_frame_values_Sex[i + length(varlist),3:5] <- (svyby(as.formula( paste0("~", varname)), ~(sdid=={params$District} & sex==2), yrrs_nm, svyciprop, vartype ="ci", na.rm=T,multicore=TRUE))[2,2:4] 
    }
  }
  

  
  Data_frame_values_Sex$Indicator <- factor(Data_frame_values_Sex$Indicator, levels = var_label(yrbss_NM[varlist]))
  
  
  sex_plot <- ggplot(Data_frame_values_Sex, aes(x = Indicator, y = Value, fill = Sex)) + 
    geom_col(position="dodge") + 
    
    theme(legend.position="top", legend.title = element_blank(),legend.justification = "right"
          , plot.subtitle = element_text(face="italic")
          ,  panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), plot.title.position = "plot")
  
  if(grouped==TRUE){
    sex_plot <- sex_plot + geom_text(aes(group=Sex, label= scales::percent(Value, accuracy=0.1),y=ci_u), vjust= -1, color="black", position = position_dodge(.9), size=5)
  }
  if(grouped==FALSE){
    sex_plot <- sex_plot + geom_text(aes(group=Sex, label= scales::percent(Value, accuracy=0.1),y=ci_u), vjust= -1, color="black", position = position_dodge(.9), size=3)
  }
  
  
  sex_plot <- sex_plot + labs(x = (element_blank()), y = "Percent (%)", subtitle = paste0("By Sex, ", ifelse(level=="district",{params$District_name},{params$counties}),
                                                                                         if(level=="county"){" County"})) +
    
    geom_errorbar(aes(x=Indicator, ymin=ci_l, ymax=ci_u), width=.1, position= position_dodge(.9), alpha=0.7) + 
    scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0,1), breaks = seq(0, 1, .25)) +
    scale_fill_manual(values = c("#82C341", "#4995D1")) + coord_cartesian(clip = 'off')
  
  #Setting max length for labels depending on how many variables being used
  if(length(varlist) > 4){
    sex_plot <- sex_plot + scale_x_discrete(labels = str_wrap(Data_frame_values_Sex$Indicator, width = 15))
  }
  else{
    sex_plot <- sex_plot + scale_x_discrete(labels = str_wrap(Data_frame_values_Sex$Indicator, width = 20))
  }    
  
  
  return(sex_plot)
}

#Barplot grade function
barplot_grade <- function(varlist, graph_title, grouped = FALSE, ms = FALSE, level="county") {
  
  if(ms){
    Grade <- c(rep("6th", length(varlist)), rep("7th", length(varlist)), rep("8th", length(varlist)))
    Grade <- factor(Grade, levels = c("6th","7th", "8th"))
    Indicator <- c(rep(0, 3*length(varlist)))
    Value <- c(rep(0, 3*length(varlist)))
    ci_l <- c(rep(0, 3*length(varlist)))
    ci_u <- c(rep(0, 3*length(varlist)))
    
    
    if(level=="county"){
      for(i in 1:length(varlist)){
        varname <- varlist[i]
        Indicator[i] <- var_label(yrbss_NM[varname])
        Indicator[i + length(varlist)] <- var_label(yrbss_NM[varname])
        Indicator[i + 2*(length(varlist))] <- var_label(yrbss_NM[varname])
        
        CITable <- svyby(as.formula( paste0("~", varname)),~{cntytxt=={params$counties}}+grade, yrrs_cnty, svyciprop, vartype ="ci", df = Design_df, na.rm.all=T,multicore=TRUE)
        
        
        for(j in 1:3){
          Value[i + ((j-1)*length(varlist))] <- CITable[j*2,3]
          ci_l[i + ((j-1)*length(varlist))] <- CITable[j*2,4]
          ci_u[i + ((j-1)*length(varlist))] <- CITable[j*2,5]
        }
      }
      
    }
    else if(level=="district"){
      for(i in 1:length(varlist)){
        varname <- varlist[i]
        Indicator[i] <- var_label(yrbss_NM[varname])
        Indicator[i + length(varlist)] <- var_label(yrbss_NM[varname])
        Indicator[i + 2*(length(varlist))] <- var_label(yrbss_NM[varname])
        
        CITable <- svyby(as.formula( paste0("~", varname)),~{sdid=={params$District}}+grade, yrrs_nm, svyciprop, vartype ="ci", na.rm.all=T,multicore=TRUE)
        
        
        for(j in 1:3){
          Value[i + ((j-1)*length(varlist))] <- CITable[j*2,3]
          ci_l[i + ((j-1)*length(varlist))] <- CITable[j*2,4]
          ci_u[i + ((j-1)*length(varlist))] <- CITable[j*2,5]
        }
      }
      
    }

    
  }
  
  else if(!ms){
    Grade <- c(rep("9th", length(varlist)), rep("10th", length(varlist)), rep("11th", length(varlist)), rep("12th", length(varlist)))
    Grade <- factor(Grade, levels = c("9th","10th", "11th", "12th"))
    Indicator <- c(rep(0, 4*length(varlist)))
    Value <- c(rep(0, 4*length(varlist)))
    ci_l <- c(rep(0, 4*length(varlist)))
    ci_u <- c(rep(0, 4*length(varlist)))
    
    
    if(level=="county"){
      for(i in 1:length(varlist)){
        varname <- varlist[i]
        Indicator[i] <- var_label(yrbss_NM[varname])
        Indicator[i + length(varlist)] <- var_label(yrbss_NM[varname])
        Indicator[i + 2*(length(varlist))] <- var_label(yrbss_NM[varname])
        Indicator[i + 3*(length(varlist))] <- var_label(yrbss_NM[varname])
        
        CITable <- svyby(as.formula( paste0("~", varname)),~{cntytxt=={params$counties}}+grade, yrrs_cnty, svyciprop, vartype ="ci", df = Design_df, na.rm.all=T,multicore=TRUE)
        
        
        for(j in 1:4){
          Value[i + ((j-1)*length(varlist))] <- CITable[j*2,3]
          ci_l[i + ((j-1)*length(varlist))] <- CITable[j*2,4]
          ci_u[i + ((j-1)*length(varlist))] <- CITable[j*2,5]
        }
      } 
      
    }
    
    else if(level=="district"){
      for(i in 1:length(varlist)){
        varname <- varlist[i]
        Indicator[i] <- var_label(yrbss_NM[varname])
        Indicator[i + length(varlist)] <- var_label(yrbss_NM[varname])
        Indicator[i + 2*(length(varlist))] <- var_label(yrbss_NM[varname])
        Indicator[i + 3*(length(varlist))] <- var_label(yrbss_NM[varname])
        
        CITable <- svyby(as.formula( paste0("~", varname)),~{sdid=={params$District}}+grade, yrrs_nm, svyciprop, vartype ="ci", na.rm.all=T,multicore=TRUE)
        
        
        for(j in 1:4){
          Value[i + ((j-1)*length(varlist))] <- CITable[j*2,3]
          ci_l[i + ((j-1)*length(varlist))] <- CITable[j*2,4]
          ci_u[i + ((j-1)*length(varlist))] <- CITable[j*2,5]
        }
      } 
      
    }
    
  }
  
  Indicator <- factor(Indicator, levels = var_label(yrbss_NM[varlist]))
  
  Data_frame_values_Grade <- data.frame(Grade, Indicator, Value, ci_l, ci_u)
  
  grade_plot <- ggplot(Data_frame_values_Grade, aes(x = Indicator, y = Value, fill = Grade)) + 
    geom_col(position="dodge") + theme(legend.position="top", legend.title = element_blank()) + 
    scale_fill_manual(values = c("#F7D6D3", "#EFA1A2", "#E56461", "#D93328")) +
    theme(legend.position="top", legend.title = element_blank(),legend.justification = "right"
          , plot.subtitle = element_text(face="italic")
          , panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(colour = "black"), plot.title.position = "plot")
  
  
  # if(grouped==TRUE){
  #   grade_plot <- grade_plot + geom_text(aes(group=Grade, label= scales::percent(Value, accuracy=0.1),y=ci_u), vjust= -1, color="black", position = position_dodge(.9), size=ifelse(grouped, 5,3))
  # 
  #   }
  # if(grouped==FALSE){
  #   grade_plot <- grade_plot + geom_text(aes(group=Grade, label= scales::percent(Value, accuracy=0.1),y=ci_u), vjust= -1, color="black", position = position_dodge(.9), size=ifelse(grouped, 5,3))
  # }
  
  # Move labels if confidence interval is too high, so they don't get cut off
  greater_than_100 <- FALSE
  for (p in Data_frame_values_Grade$ci_u){

    if (p > .9) {
      grade_plot <- grade_plot + geom_text(aes(group=Grade, label= scales::percent(Value, accuracy=0.1),y=ci_l), vjust= +2, color="black", position = position_dodge(.9), size=ifelse(grouped, 5,3))
      
      greater_than_100 <- TRUE
      if(p>1){p <- .9999}
      break
    }
  }
  if (greater_than_100 ==FALSE){
    grade_plot <- grade_plot + geom_text(aes(group=Grade, label= scales::percent(Value, accuracy=0.1),y=ci_u), vjust= -1, color="black", position = position_dodge(.9), size=ifelse(grouped, 5,3))
    
  }
  
  grade_plot <- grade_plot + labs(x = (element_blank()), y = "Percent (%)", subtitle = paste0("By Grade, ", ifelse(level=="county",{params$counties},{params$District_name}), if(level=="county"){" County"})) +
    
    geom_errorbar(aes(x=Indicator, ymin=ci_l, ymax=ci_u), width=.1, position= position_dodge(.9), alpha=0.7) + 
    scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0,1), breaks = seq(0, 1, .25))+
    coord_cartesian(clip = 'off')
  
  #Setting max length for labels depending on how many variables being used    
  if(length(varlist) > 4){
    grade_plot <- grade_plot + scale_x_discrete(labels = str_wrap(Indicator, width = 15))
  }
  else{
    grade_plot <- grade_plot + scale_x_discrete(labels = str_wrap(Indicator, width = 20))
  }
  
  #grade_plot <- grade_plot + scale_x_discrete(labels = str_wrap(Indicator, width = 15))
  return(grade_plot)
  
}


barplot_group <- function(varlist, graph_title, ms=FALSE, level="county"){
  
  if(level=="district"){
    county_plot <- barplot_district_2(varlist,graph_title, grouped = TRUE)
  }
  else{
    county_plot <- barplot_county_2(varlist,graph_title, grouped = TRUE)
  }

  county_plot <- county_plot + theme(text = element_text(size=21, inherit.blank=TRUE))

  sex_plot <- barplot_sex(varlist, graph_title, grouped = TRUE, level=level)
  sex_plot <- sex_plot + theme(text = element_text(size=21, inherit.blank=TRUE))

  grade_plot <- barplot_grade(varlist, graph_title, grouped = TRUE,ms=ms, level=level)
  grade_plot <- grade_plot + theme(text = element_text(size=21, inherit.blank=TRUE))

  return(ggpubr::ggarrange(county_plot, sex_plot, grade_plot, ncol = 1, align = "v"))
  
  
}
