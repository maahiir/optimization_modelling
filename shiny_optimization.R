library(shiny)
library(dplyr)
library(readr)
library(tidyplots)
library(shiny.router)
Sys.setenv(LANG="eng")
#install.packages("shiny.router")
#devtools::install_github("jbengler/tidyplots")
#remotes::install_github("Appsilon/shiny.router")

# Define UI (User Interface)
ui <- fluidPage(
  titlePanel("Upload Your CSV File"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File", accept = ".csv"),
      actionButton("process", "Process Data")
    ),
    
    mainPanel(
      h4("Uploaded Data Preview"),
      tableOutput("data_table"),
      
      h4("Processed Data with Weekly Frequencies"),
      tableOutput("processed_data")
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  data <- reactiveVal(NULL)  # Store uploaded data
  
  # Load CSV file when uploaded
  observeEvent(input$file, {
    req(input$file)
    df <- read_csv2(input$file$datapath)
    data(df)  # Store data in reactive variable
  })
  
  # Display raw data preview
  output$data_table <- renderTable({
    req(data())
    head(data(), 10)
  })
  
  # Process Data when "Process" button is clicked
  processed_data <- eventReactive(input$process, {
    req(data())
    df <- data()
    
    # Convert column names to lowercase
    colnames(df) <- tolower(gsub(" ", "_", colnames(df)))
    
    if("bread_rolls_buns" %in% colnames(df)){
      
      df<-df%>% mutate(bread_rolls_buns_week=case_when(bread_rolls_buns==0~0,
                                                       bread_rolls_buns==1~6,
                                                       bread_rolls_buns==2~1.5*7,
                                                       bread_rolls_buns==3~3.5*7,
                                                       bread_rolls_buns==4~5.5*7,
                                                       bread_rolls_buns==5~7.5*7,
                                                       bread_rolls_buns==6~9*7))
      
      if ("rye_multigrain_bread" %in% colnames(df)) {
        df <- df %>%
          mutate(rye_multigrain_bread_1 = case_when(rye_multigrain_bread == 0 ~ 0,
                                                    rye_multigrain_bread == 1 ~ 0.1 * bread_rolls_buns_week,
                                                    rye_multigrain_bread == 2 ~ 0.4 * bread_rolls_buns_week,
                                                    rye_multigrain_bread == 3 ~ 0.77 * bread_rolls_buns_week,
                                                    rye_multigrain_bread == 4 ~ bread_rolls_buns_week
          ))
      }
      
      if ("whole_grain_bread_buns" %in% colnames(df)) {
        df <- df %>%
          mutate(whole_grain_bread_buns_1 = case_when(whole_grain_bread_buns == 0 ~ 0,
                                                      whole_grain_bread_buns == 1 ~ 0.1 * bread_rolls_buns_week,
                                                      whole_grain_bread_buns == 2 ~ 0.4 * bread_rolls_buns_week,
                                                      whole_grain_bread_buns == 3 ~ 0.77 * bread_rolls_buns_week,
                                                      whole_grain_bread_buns == 4 ~ bread_rolls_buns_week
          ))
      }
      
      if ("white_wheat_bread_buns" %in% colnames(df)) {
        df <- df %>%
          mutate(whole_grain_bread_buns_1 = case_when(whole_grain_bread_buns == 0 ~ 0,
                                                      whole_grain_bread_buns == 1 ~ 0.1 * bread_rolls_buns_week,
                                                      whole_grain_bread_buns == 2 ~ 0.4 * bread_rolls_buns_week,
                                                      whole_grain_bread_buns == 3 ~ 0.77 * bread_rolls_buns_week,
                                                      whole_grain_bread_buns == 4 ~ bread_rolls_buns_week
          ))
      }
      
      if ("crisp" %in% colnames(df)) {
        df <- df %>%
          mutate(crisp_1 = case_when(crisp == 0 ~ 0,
                                     crisp == 1 ~ 0.1,
                                     crisp == 2 ~ 0.4,
                                     crisp == 3 ~ 0.77,
                                     crisp == 4 ~ 1
          ))
      }
      
      if ("crispbread" %in% colnames(df)) {
        df <- df %>%
          mutate(crispbread_1 = case_when(crispbread == 0 ~ 0,
                                          crispbread == 1 ~ 0.1,
                                          crispbread == 2 ~ 0.4,
                                          crispbread == 3 ~ 0.77,
                                          crispbread == 4 ~ 1
          ))
      }
      
      if ("butter" %in% colnames(df)) {
        df <- df %>%
          mutate(butter_1 = case_when(butter == 0 ~ 0,
                                      butter == 1 ~ 0.1,
                                      butter == 2 ~ 0.4,
                                      butter == 3 ~ 0.77,
                                      butter == 4 ~ 1
          ))
      }
      
      if ("regular" %in% colnames(df)) {
        df <- df %>%
          mutate(regular_1 = case_when(regular == 0 ~ 0,
                                       regular == 1 ~ 0.1,
                                       regular == 2 ~ 0.4,
                                       regular == 3 ~ 0.77,
                                       regular == 4 ~ 1
          ))
      }
      
      if ("fat_reduced_margarine" %in% colnames(df)) {
        df <- df %>%
          mutate(fat_reduced_margarine_1 = case_when(fat_reduced_margarine == 0 ~ 0,
                                                     fat_reduced_margarine == 1 ~ 0.1,
                                                     fat_reduced_margarine == 2 ~ 0.4,
                                                     fat_reduced_margarine == 3 ~ 0.77,
                                                     fat_reduced_margarine == 4 ~ 1
          ))
      }
      
      if ("peanut_butter" %in% colnames(df)) {
        df <- df %>%
          mutate(peanut_butter_1 = case_when(peanut_butter == 0 ~ 0,
                                             peanut_butter == 1 ~ 0.1,
                                             peanut_butter == 2 ~ 0.4,
                                             peanut_butter == 3 ~ 0.77,
                                             peanut_butter == 4 ~ 1
          ))
      }
      
    }  else {
      
              df$bread_rolls_buns_week <- NA
              df$rye_multigrain_bread_1 <- NA
              df$whole_grain_bread_buns_1 <- NA
              df$white_wheat_bread_buns_1 <- NA
              df$crisp_1 <- NA
              df$crispbread_1 <- NA
              df$butter_1<-NA
              df$regular_1<-NA
              df$fat_reduced_margarine_1<-NA
              df$peanut_butter_1<-NA
      
    }
          
    bread_components <- c("rye_multigrain_bread_1", "whole_grain_bread_buns_1", "white_wheat_bread_buns_1", "crisp_1", "crispbread_1")
    existing_components <- intersect(bread_components, colnames(df))
    
    if (length(existing_components) > 0) {
      df <- df %>%
        mutate(bread = rowSums(select(df, all_of(existing_components)), na.rm = TRUE))
    } else {
      df$bread <- NA  # Initialize `bread` if no components exist
    }
    
    spread_components<-c("butter_1","regular_1","fat_reduced_margarine_1","peanut_butter_1")
    existing_spread_components<-intersect(spread_components,colnames(df))
    
    if (length(existing_spread_components) > 0) {
      df <- df %>%
        mutate(spreads = rowSums(select(df, all_of(existing_spread_components)), na.rm = TRUE))
    } else {
      df$spreads <- NA  # Initialize `spreads` if no components exist
    }
    
    df <- df %>%
      mutate(
        rye_multigrain_bread_week = ifelse(!is.na(bread) & bread > 0, (rye_multigrain_bread_1 / bread) * bread_rolls_buns_week, NA),
        whole_grain_bread_buns_week = ifelse(!is.na(bread) & bread > 0, (whole_grain_bread_buns_1 / bread) * bread_rolls_buns_week, NA),
        white_wheat_bread_buns_week = ifelse(!is.na(bread) & bread>0, (white_wheat_bread_buns_1/bread) * bread_rolls_buns_day),
        butter_week = ifelse(!is.na(spreads) & spreads > 0, (butter_1 / spreads) * bread_rolls_buns_week, NA),
        regular_week = ifelse(!is.na(spreads) & spreads > 0, (regular_1 / spreads) * bread_rolls_buns_week, NA),
        fat_reduced_margarine_week = ifelse(!is.na(spreads) & spreads > 0, (fat_reduced_margarine_1 / spreads) * bread_rolls_buns_week, NA),
        regular_week = ifelse(!is.na(spreads) & spreads > 0, (regular_1 / spreads) * bread_rolls_buns_week, NA),
        crisp_week = ifelse(!is.na(bread) & bread > 0, (crisp_1 / bread) * bread_rolls_buns_week, NA),
        crispbread_week = ifelse(!is.na(bread)& bread >0, (crispbread_1/bread)*bread_rolls_buns_week,NA)
      ) 
      
      
      if ("muesli_cereals" %in% colnames(df)) {
        
        df <- df %>%
          mutate(muesli_cereals_week = case_when(muesli_cereals == 0 ~ 0,
                                                    muesli_cereals == 1~ 1/30*7,
                                                    muesli_cereals == 2~ 2.5/30*7,
                                                    muesli_cereals == 3~ 1.5,
                                                    muesli_cereals == 4~ 3.5,
                                                    muesli_cereals == 5~5.5,
                                                    muesli_cereals == 6~7))
      }
    
    if ("hot_cereal_porridge" %in% colnames(df)) {
      
      df <- df %>%
        mutate(hot_cereal_porridge_week = case_when(hot_cereal_porridge == 0 ~ 0,
                                                    hot_cereal_porridge == 1~ 1/30*7,
                                                    hot_cereal_porridge == 2~ 2.5/30*7,
                                                    hot_cereal_porridge == 3~ 1.5,
                                                    hot_cereal_porridge == 4~ 3.5,
                                                    hot_cereal_porridge == 5~5.5,
                                                    hot_cereal_porridge == 6~7))
    }
    if ("cocoa_chocolate_milk_fruit" %in% colnames(df)) {
      
      df <- df %>%
        mutate(cocoa_chocolate_milk_fruit_week = case_when(cocoa_chocolate_milk_fruit == 0 ~ 0,
                                                           cocoa_chocolate_milk_fruit == 1~ 1/30*7,
                                                           cocoa_chocolate_milk_fruit == 2~ 2.5/30*7,
                                                           cocoa_chocolate_milk_fruit == 3~ 1.5,
                                                           cocoa_chocolate_milk_fruit == 4~ 3.5,
                                                           cocoa_chocolate_milk_fruit == 5~5.5,
                                                           cocoa_chocolate_milk_fruit == 6~7))
    }
    
    if ("plain_yoghurt_buttermilk" %in% colnames(df)) {
      
      df <- df %>%
        mutate(plain_yoghurt_buttermilk_week = case_when(plain_yoghurt_buttermilk == 0 ~ 0,
                                                         plain_yoghurt_buttermilk == 1~ 1/30*7,
                                                         plain_yoghurt_buttermilk == 2~ 2.5/30*7,
                                                         plain_yoghurt_buttermilk == 3~ 1.5,
                                                         plain_yoghurt_buttermilk == 4~ 3.5,
                                                         plain_yoghurt_buttermilk == 5~5.5,
                                                         plain_yoghurt_buttermilk == 6~7))
    }
    
    if ("sour_milk" %in% colnames(df)) {
      
      df <- df %>%
        mutate(sour_milk_week = case_when(sour_mlik == 0 ~ 0,
                                          sour_mlik == 1~ 1/30*7,
                                          sour_mlik == 2~ 2.5/30*7,
                                          sour_mlik == 3~ 1.5,
                                          sour_mlik == 4~ 3.5,
                                          sour_mlik == 5~5.5,
                                          sour_mlik == 6~7))
    }
    
    if ("flavoured_yoghurt" %in% colnames(df)) {
      
      df <- df %>%
        mutate(flavoured_yoghurt_week = case_when(flavoured_yoghurt_week == 0 ~ 0,
                                                  flavoured_yoghurt_week == 1~ 1/30*7,
                                                  flavoured_yoghurt_week == 2~ 2.5/30*7,
                                                  flavoured_yoghurt_week == 3~ 1.5,
                                                  flavoured_yoghurt_week == 4~ 3.5,
                                                  flavoured_yoghurt_week == 5~5.5,
                                                  flavoured_yoghurt_week == 6~7))
    }
    
    if ("greek_yoghurt" %in% colnames(df)) {
      
      df <- df %>%
        mutate(greek_yoghurt_week = case_when(greek_yoghurt == 0 ~ 0,
                                              greek_yoghurt == 1~ 1/30*7,
                                              greek_yoghurt == 2~ 2.5/30*7,
                                              greek_yoghurt == 3~ 1.5,
                                              greek_yoghurt == 4~ 3.5,
                                              greek_yoghurt == 5~5.5,
                                              greek_yoghurt == 6~7))
    }
    
    if ("cream_cheese" %in% colnames(df)) {
      
      df <- df %>%
        mutate(cream_cheese_week = case_when(cream_cheese == 0 ~ 0,
                                             cream_cheese == 1~ 1/30*7,
                                             cream_cheese == 2~ 2.5/30*7,
                                             cream_cheese == 3~ 1.5,
                                             cream_cheese == 4~ 3.5,
                                             cream_cheese == 5~5.5,
                                             cream_cheese == 6~7))
    }
    
    if ("soft_cheese" %in% colnames(df)) {
      
      df <- df %>%
        mutate(soft_cheese_week = case_when(soft_cheese == 0 ~ 0,
                                            soft_cheese == 1~ 1/30*7,
                                            soft_cheese == 2~ 2.5/30*7,
                                            soft_cheese == 3~ 1.5,
                                            soft_cheese == 4~ 3.5,
                                            soft_cheese == 5~5.5,
                                            soft_cheese == 6~7))
    }
    
    if ("semi_soft_firm_cheese" %in% colnames(df)) {
      
      df <- df %>%
        mutate(semi_soft_firm_cheese_week = case_when(semi_soft_firm_cheese == 0 ~ 0,
                                                      semi_soft_firm_cheese == 1~ 1/30*7,
                                                      semi_soft_firm_cheese == 2~ 2.5/30*7,
                                                      semi_soft_firm_cheese == 3~ 1.5,
                                                      semi_soft_firm_cheese == 4~ 3.5,
                                                      semi_soft_firm_cheese == 5~5.5,
                                                      semi_soft_firm_cheese == 6~7))
    }
    
    if ("mozarella_mascarpone_feta" %in% colnames(df)) {
      
      df <- df %>%
        mutate(mozarella_mascarpone_feta_week = case_when(mozarella_mascarpone_feta == 0 ~ 0,
                                                          mozarella_mascarpone_feta == 1~ 1/30*7,
                                                          mozarella_mascarpone_feta == 2~ 2.5/30*7,
                                                          mozarella_mascarpone_feta == 3~ 1.5,
                                                          mozarella_mascarpone_feta == 4~ 3.5,
                                                          mozarella_mascarpone_feta == 5~5.5,
                                                          mozarella_mascarpone_feta == 6~7))
    }
    
    if ("nuts_almonds" %in% colnames(df)) {
      
      df <- df %>%
        mutate(nuts_almonds_week = case_when(nuts_almonds == 0 ~ 0,
                                             nuts_almonds == 1~ 1/30*7,
                                             nuts_almonds == 2~ 2.5/30*7,
                                             nuts_almonds == 3~ 1.5,
                                             nuts_almonds == 4~ 3.5,
                                             nuts_almonds == 5~5.5,
                                             nuts_almonds == 6~7))
    }
    
    if ("seeds" %in% colnames(df)) {
      
      df <- df %>%
        mutate(seeds_week = case_when(seeds == 0 ~ 0,
                                      seeds == 1~ 1/30*7,
                                      seeds == 2~ 2.5/30*7,
                                      seeds == 3~ 1.5,
                                      seeds == 4~ 3.5,
                                      seeds == 5~5.5,
                                      seeds == 6~7))
    }
    
    if ("potatoes" %in% colnames(df)) {
      
      df <- df %>%
        mutate(potatoes_week = case_when(potatoes == 0 ~ 0,
                                         potatoes == 1~ 1/30*7,
                                         potatoes == 2~ 2.5/30*7,
                                         potatoes == 3~ 1.5,
                                         potatoes == 4~ 3.5,
                                         potatoes == 5~5.5,
                                         potatoes == 6~7))
    }
    
    if ("carrots" %in% colnames(df)) {
      
      df <- df %>%
        mutate(carrots_week = case_when(carrots == 0 ~ 0,
                                        carrots == 1~ 1/30*7,
                                        carrots == 2~ 2.5/30*7,
                                        carrots == 3~ 1.5,
                                        carrots == 4~ 3.5,
                                        carrots == 5~5.5,
                                        carrots == 6~7))
    }
    
    if ("tomatoes" %in% colnames(df)) {
      
      df <- df %>%
        mutate(tomatoes_week = case_when(tomatoes == 0 ~ 0,
                                         tomatoes == 1~ 1/30*7,
                                         tomatoes == 2~ 2.5/30*7,
                                         tomatoes == 3~ 1.5,
                                         tomatoes == 4~ 3.5,
                                         tomatoes == 5~5.5,
                                         tomatoes == 6~7))
    }
    
    if ("lettuce_endive" %in% colnames(df)) {
      
      df <- df %>%
        mutate(lettuce_endive_week = case_when(lettuce_endive == 0 ~ 0,
                                               lettuce_endive == 1~ 1/30*7,
                                               lettuce_endive == 2~ 2.5/30*7,
                                               lettuce_endive == 3~ 1.5,
                                               lettuce_endive == 4~ 3.5,
                                               lettuce_endive == 5~5.5,
                                               lettuce_endive == 6~7))
    }
    
    if ("cucumber" %in% colnames(df)) {
      
      df <- df %>%
        mutate(cucumber_week = case_when(cucumber == 0 ~ 0,
                                         cucumber == 1~ 1/30*7,
                                         cucumber == 2~ 2.5/30*7,
                                         cucumber == 3~ 1.5,
                                         cucumber == 4~ 3.5,
                                         cucumber == 5~5.5,
                                         cucumber == 6~7))
    }
    
    if ("peppers" %in% colnames(df)) {
      
      df <- df %>%
        mutate(peppers_week = case_when(peppers == 0 ~ 0,
                                        peppers == 1~ 1/30*7,
                                        peppers == 2~ 2.5/30*7,
                                        peppers == 3~ 1.5,
                                        peppers == 4~ 3.5,
                                        peppers == 5~5.5,
                                        peppers == 6~7))
    }
    
    if ("rice" %in% colnames(df)) {
      
      df <- df %>%
        mutate(rice_week = case_when(rice == 0 ~ 0,
                                     rice == 1~ 1/30*7,
                                     rice == 2~ 2.5/30*7,
                                     rice == 3~ 1.5,
                                     rice == 4~ 3.5,
                                     rice == 5~5.5,
                                     rice == 6~7))
    }
    
    if ("other_grains" %in% colnames(df)) {
      
      df <- df %>%
        mutate(other_grains_week = case_when(other_grains == 0 ~ 0,
                                             other_grains == 1~ 1/30*7,
                                             other_grains == 2~ 2.5/30*7,
                                             other_grains == 3~ 1.5,
                                             other_grains == 4~ 3.5,
                                             other_grains == 5~5.5,
                                             other_grains == 6~7))
    }
    
    if ("dry_cured_meat" %in% colnames(df)) {
      
      df <- df %>%
        mutate(dry_cured_meat_week = case_when(dry_cured_meat == 0 ~ 0,
                                               dry_cured_meat == 1~ 1/30*7,
                                               dry_cured_meat == 2~ 2.5/30*7,
                                               dry_cured_meat == 3~ 1.5,
                                               dry_cured_meat == 4~ 3.5,
                                               dry_cured_meat == 5~5.5,
                                               dry_cured_meat == 6~7))
    }
    
    if ("egg" %in% colnames(df)) {
      
      df <- df %>%
        mutate(egg_week = case_when(egg == 0 ~ 0,
                                    egg == 1~ 1/30*7,
                                    egg == 2~ 2.5/30*7,
                                    egg == 3~ 1.5,
                                    egg == 4~ 3.5,
                                    egg == 5~5.5,
                                    egg == 6~7))
    }
    
    if ("salami" %in% colnames(df)) {
      
      df <- df %>%
        mutate(salami_week = case_when(salami == 0 ~ 0,
                                       salami == 1~ 1/30*7,
                                       salami == 2~ 2.5/30*7,
                                       salami == 3~ 1.5,
                                       salami == 4~ 3.5,
                                       salami == 5~5.5,
                                       salami == 6~7))
    }
    
    if ("chocolate" %in% colnames(df)) {
      
      df <- df %>%
        mutate(chocolate_week = case_when(chocolate == 0 ~ 0,
                                          chocolate == 1~ 1/30*7,
                                          chocolate == 2~ 2.5/30*7,
                                          chocolate == 3~ 1.5,
                                          chocolate == 4~ 3.5,
                                          chocolate == 5~5.5,
                                          chocolate == 6~7))
    }
    
    if ("sweets_candy" %in% colnames(df)) {
      
      df <- df %>%
        mutate(sweets_candy_week = case_when(sweets_candy == 0 ~ 0,
                                             sweets_candy == 1~ 1/30*7,
                                             sweets_candy == 2~ 2.5/30*7,
                                             sweets_candy == 3~ 1.5,
                                             sweets_candy == 4~ 3.5,
                                             sweets_candy == 5~5.5,
                                             sweets_candy == 6~7))
    }
    
    if ("marmelade_jam_jelly_honey" %in% colnames(df)) {
      
      df <- df %>%
        mutate(marmelade_jam_jelly_honey_week = case_when(marmelade_jam_jelly_honey == 0 ~ 0,
                                                          marmelade_jam_jelly_honey == 1~ 1/30*7,
                                                          marmelade_jam_jelly_honey == 2~ 2.5/30*7,
                                                          marmelade_jam_jelly_honey == 3~ 1.5,
                                                          marmelade_jam_jelly_honey == 4~ 3.5,
                                                          marmelade_jam_jelly_honey == 5~5.5,
                                                          marmelade_jam_jelly_honey == 6~1.5*7,
                                                          marmelade_jam_jelly_honey == 7~3*7))
    }
    
    if ("milk" %in% colnames(df)) {
      
      df <- df %>%
        mutate(milk_week = case_when(milk == 0 ~ 0,
                                     milk == 1~ 1/30*7,
                                     milk == 2~ 2.5/30*7,
                                     milk == 3~ 1.5,
                                     milk == 4~ 3.5,
                                     milk == 5~5.5,
                                     milk == 6~1.5*7,
                                     milk == 7~3*7))
    }
    
    if ("apple_pear_fresh" %in% colnames(df)) {
      
      df <- df %>%
        mutate(apple_pear_fresh_week = case_when(apple_pear_fresh == 0 ~ 0,
                                                 apple_pear_fresh == 1~ 1/30*7,
                                                 apple_pear_fresh == 2~ 2.5/30*7,
                                                 apple_pear_fresh == 3~ 1.5,
                                                 apple_pear_fresh == 4~ 3.5,
                                                 apple_pear_fresh == 5~5.5,
                                                 apple_pear_fresh == 6~1.5*7,
                                                 apple_pear_fresh == 7~3*7))
    }
    
    if ("orange" %in% colnames(df)) {
      
      df <- df %>%
        mutate(orange_week = case_when(orange == 0 ~ 0,
                                       orange == 1~ 1/30*7,
                                       orange == 2~ 2.5/30*7,
                                       orange == 3~ 1.5,
                                       orange == 4~ 3.5,
                                       orange == 5~5.5,
                                       orange == 6~1.5*7,
                                       orange == 7~3*7))
    }
    
    if ("banana" %in% colnames(df)) {
      
      df <- df %>%
        mutate(banana_week = case_when(banana == 0 ~ 0,
                                       banana == 1~ 1/30*7,
                                       banana == 2~ 2.5/30*7,
                                       banana == 3~ 1.5,
                                       banana == 4~ 3.5,
                                       banana == 5~5.5,
                                       banana == 6~1.5*7,
                                       banana == 7~3*7))
    }
    
    if ("plum" %in% colnames(df)) {
      
      df <- df %>%
        mutate(plum_week = case_when(plum == 0 ~ 0,
                                     plum == 1~ 1/30*7,
                                     plum == 2~ 2.5/30*7,
                                     plum == 3~ 1.5,
                                     plum == 4~ 3.5,
                                     plum == 5~5.5,
                                     plum == 6~1.5*7,
                                     plum == 7~3*7))
    }
    
    if ("strawberries_cherries" %in% colnames(df)) {
      
      df <- df %>%
        mutate(strawberries_cherries_week = case_when(strawberries_cherries == 0 ~ 0,
                                                      strawberries_cherries == 1~ 1/30*7,
                                                      strawberries_cherries == 2~ 2.5/30*7,
                                                      strawberries_cherries == 3~ 1.5,
                                                      strawberries_cherries == 4~ 3.5,
                                                      strawberries_cherries == 5~5.5,
                                                      strawberries_cherries == 6~1.5*7,
                                                      strawberries_cherries == 7~3*7))
    }
    
    if ("water_melon" %in% colnames(df)) {
      
      df <- df %>%
        mutate(water_melon_week = case_when(water_melon == 0 ~ 0,
                                            water_melon == 1~ 1/30*7,
                                            water_melon == 2~ 2.5/30*7,
                                            water_melon == 3~ 1.5,
                                            water_melon == 4~ 3.5,
                                            water_melon == 5~5.5,
                                            water_melon == 6~1.5*7,
                                            water_melon == 7~3*7))
    }
    
    if ("mango_pawpaw" %in% colnames(df)) {
      
      df <- df %>%
        mutate(mango_pawpaw_week = case_when(mango_pawpaw == 0 ~ 0,
                                             mango_pawpaw == 1~ 1/30*7,
                                             mango_pawpaw == 2~ 2.5/30*7,
                                             mango_pawpaw == 3~ 1.5,
                                             mango_pawpaw == 4~ 3.5,
                                             mango_pawpaw == 5~5.5,
                                             mango_pawpaw == 6~1.5*7,
                                             mango_pawpaw == 7~3*7))
    }
    
    if ("berries" %in% colnames(df)) {
      
      df <- df %>%
        mutate(berries_week = case_when(berries == 0 ~ 0,
                                        berries == 1~ 1/30*7,
                                        berries == 2~ 2.5/30*7,
                                        berries == 3~ 1.5,
                                        berries == 4~ 3.5,
                                        berries == 5~5.5,
                                        berries == 6~1.5*7,
                                        berries == 7~3*7))
    }
    
    if ("grapes" %in% colnames(df)) {
      
      df <- df %>%
        mutate(grapes_week = case_when(grapes == 0 ~ 0,
                                       grapes == 1~ 1/30*7,
                                       grapes == 2~ 2.5/30*7,
                                       grapes == 3~ 1.5,
                                       grapes == 4~ 3.5,
                                       grapes == 5~5.5,
                                       grapes == 6~1.5*7,
                                       grapes == 7~3*7))
    }
    
    if ("plantain" %in% colnames(df)) {
      
      df <- df %>%
        mutate(plantain_week = case_when(plantain == 0 ~ 0,
                                         plantain == 1~ 1/30*7,
                                         plantain == 2~ 2.5/30*7,
                                         plantain == 3~ 1.5,
                                         plantain == 4~ 3.5,
                                         plantain == 5~5.5,
                                         plantain == 6~1.5*7,
                                         plantain == 7~3*7))
    }
    
    if ("cassava" %in% colnames(df)) {
      
      df <- df %>%
        mutate(cassava_week = case_when(cassava == 0 ~ 0,
                                        cassava == 1~ 1/30*7,
                                        cassava == 2~ 2.5/30*7,
                                        cassava == 3~ 1.5,
                                        cassava == 4~ 3.5,
                                        cassava == 5~5.5,
                                        cassava == 6~1.5*7,
                                        cassava == 7~3*7))
    }
    
    if ("yam" %in% colnames(df)) {
      
      df <- df %>%
        mutate(yam_week = case_when(yam == 0 ~ 0,
                                    yam == 1~ 1/30*7,
                                    yam == 2~ 2.5/30*7,
                                    yam == 3~ 1.5,
                                    yam == 4~ 3.5,
                                    yam == 5~5.5,
                                    yam == 6~1.5*7,
                                    yam == 7~3*7))
    }
    
    if ("sweet_potatoes" %in% colnames(df)) {
      
      df <- df %>%
        mutate(sweet_potatoes_week = case_when(sweet_potatoes == 0 ~ 0,
                                               sweet_potatoes == 1~ 1/30*7,
                                               sweet_potatoes == 2~ 2.5/30*7,
                                               sweet_potatoes == 3~ 1.5,
                                               sweet_potatoes == 4~ 3.5,
                                               sweet_potatoes == 5~5.5,
                                               sweet_potatoes == 6~1.5*7,
                                               sweet_potatoes == 7~3*7))
    }
    
    if ("fufu" %in% colnames(df)) {
      
      df <- df %>%
        mutate(fufu_week = case_when(fufu == 0 ~ 0,
                                     fufu == 1~ 1/30*7,
                                     fufu == 2~ 2.5/30*7,
                                     fufu == 3~ 1.5,
                                     fufu == 4~ 3.5,
                                     fufu == 5~5.5,
                                     fufu == 6~1.5*7,
                                     fufu == 7~3*7))
    }
    
    if ("banku" %in% colnames(df)) {
      
      df <- df %>%
        mutate(banku_week = case_when(banku == 0 ~ 0,
                                      banku == 1~ 1/30*7,
                                      banku == 2~ 2.5/30*7,
                                      banku == 3~ 1.5,
                                      banku == 4~ 3.5,
                                      banku == 5~5.5,
                                      banku == 6~1.5*7,
                                      banku == 7~3*7))
    }
    
    if ("kenkey" %in% colnames(df)) {
      
      df <- df %>%
        mutate(kenkey_week = case_when(kenkey == 0 ~ 0,
                                       kenkey == 1~ 1/30*7,
                                       kenkey == 2~ 2.5/30*7,
                                       kenkey == 3~ 1.5,
                                      kenkey == 4~ 3.5,
                                      kenkey == 5~5.5,
                                      kenkey == 6~1.5*7,
                                      kenkey == 7~3*7))
    } 
    
    if ("stewed_fruit" %in% colnames(df)) {
      
      df <- df %>%
        mutate(stewed_fruit_week = case_when(stewed_fruit == 0 ~ 0,
                                             stewed_fruit == 1~ 1/30*7,
                                             stewed_fruit == 2~ 2.5/30*7,
                                             stewed_fruit == 3~ 1.5,
                                             stewed_fruit == 4~ 3))
    }
    
    if ("dried_fruit" %in% colnames(df)) {
      
      df <- df %>%
        mutate(dried_fruit_week = case_when(dried_fruit == 0 ~ 0,
                                            dried_fruit == 1~ 1/30*7,
                                            dried_fruit == 2~ 2.5/30*7,
                                            dried_fruit == 3~ 1.5,
                                            dried_fruit == 4~ 3))
    }
    
    if ("pan_fried_potatoes" %in% colnames(df)) {
      
      df <- df %>%
        mutate(pan_fried_potatoes_week = case_when(pan_fried_potatoes == 0 ~ 0,
                                                   pan_fried_potatoes == 1~ 1/30*7,
                                                   pan_fried_potatoes == 2~ 2.5/30*7,
                                                   pan_fried_potatoes == 3~ 1.5,
                                                   pan_fried_potatoes == 4~ 3))
    }
    
    if ("leaves_spinach" %in% colnames(df)) {
      
      df <- df %>%
        mutate(leaves_spinach_week = case_when(leaves_spinach == 0 ~ 0,
                                               leaves_spinach == 1~ 1/30*7,
                                               leaves_spinach == 2~ 2.5/30*7,
                                               leaves_spinach == 3~ 1.5,
                                               leaves_spinach == 4~ 3))
    }
    
    if ("cooked_white_cabbage" %in% colnames(df)) {
      
      df <- df %>%
        mutate(cooked_white_cabbage_week = case_when(cooked_white_cabbage == 0 ~ 0,
                                                     cooked_white_cabbage == 1~ 1/30*7,
                                                     cooked_white_cabbage == 2~ 2.5/30*7,
                                                     cooked_white_cabbage == 3~ 1.5,
                                                     cooked_white_cabbage == 4~ 3))
    }
    
    if ("egg_plant" %in% colnames(df)) {
      
      df <- df %>%
        mutate(egg_plant_week = case_when(egg_plant == 0 ~ 0,
                                          egg_plant == 1~ 1/30*7,
                                          egg_plant == 2~ 2.5/30*7,
                                          egg_plant == 3~ 1.5,
                                          egg_plant == 4~ 3))
    }
    
    if ("legumes" %in% colnames(df)) {
      
      df <- df %>%
        mutate(legumes_week = case_when(legumes == 0 ~ 0,
                                        legumes == 1~ 1/30*7,
                                        legumes == 2~ 2.5/30*7,
                                        legumes == 3~ 1.5,
                                        legumes == 4~ 3))
    }
    
    if ("beans" %in% colnames(df)) {
      
      df <- df %>%
        mutate(beans_week = case_when(beans == 0 ~ 0,
                                      beans == 1~ 1/30*7,
                                      beans == 2~ 2.5/30*7,
                                      beans == 3~ 1.5,
                                      beans == 4~ 3))
    }
    
    if ("groundnut_soup" %in% colnames(df)) {
      
      df <- df %>%
        mutate(groundnut_soup_week = case_when(groundnut_soup == 0 ~ 0,
                                               groundnut_soup == 1~ 1/30*7,
                                               groundnut_soup == 2~ 2.5/30*7,
                                               groundnut_soup == 3~ 1.5,
                                               groundnut_soup == 4~ 3))
    }
    
    if ("palmnut_soup" %in% colnames(df)) {
      
      df <- df %>%
        mutate(palmnut_soup_week = case_when(palmnut_soup == 0 ~ 0,
                                             palmnut_soup == 1~ 1/30*7,
                                             palmnut_soup == 2~ 2.5/30*7,
                                             palmnut_soup == 3~ 1.5,
                                             palmnut_soup == 4~ 3))
    }
    
    if ("green_stew" %in% colnames(df)) {
      
      df <- df %>%
        mutate(green_stew_week = case_when(green_stew == 0 ~ 0,
                                           green_stew == 1~ 1/30*7,
                                           green_stew == 2~ 2.5/30*7,
                                           green_stew == 3~ 1.5,
                                           green_stew == 4~ 3))
    }
    
    if ("okro_stew" %in% colnames(df)) {
      
      df <- df %>%
        mutate(okro_stew_week = case_when(Okro_stew == 0 ~ 0,
                                          Okro_stew == 1~ 1/30*7,
                                          Okro_stew == 2~ 2.5/30*7,
                                          Okro_stew == 3~ 1.5,
                                          Okro_stew == 4~ 3))
    }
    
    if ("tomato_sauce_stew" %in% colnames(df)) {
      
      df <- df %>%
        mutate(tomato_sauce_stew_week = case_when(tomato_sauce_stew == 0 ~ 0,
                                                  tomato_sauce_stew == 1~ 1/30*7,
                                                  tomato_sauce_stew == 2~ 2.5/30*7,
                                                  tomato_sauce_stew == 3~ 1.5,
                                                  tomato_sauce_stew == 4~ 3))
    }
    
    if ("ketchup" %in% colnames(df)) {
      
      df <- df %>%
        mutate(ketchup_week = case_when(ketchup == 0 ~ 0,
                                        ketchup == 1~ 1/30*7,
                                        ketchup == 2~ 2.5/30*7,
                                        ketchup == 3~ 1.5,
                                        ketchup == 4~ 3))
    }
    
    if ("lentil_pea_bean_soup" %in% colnames(df)) {
      
      df <- df %>%
        mutate(lentil_pea_bean_soup_week = case_when(lentil_pea_bean_soup == 0 ~ 0,
                                                     lentil_pea_bean_soup == 1~ 1/30*7,
                                                     lentil_pea_bean_soup == 2~ 2.5/30*7,
                                                     lentil_pea_bean_soup == 3~ 1.5,
                                                     lentil_pea_bean_soup == 4~ 3))
    }
    
    if ("vegetable_soup" %in% colnames(df)) {
      
      df <- df %>%
        mutate(vegetable_soup_week = case_when(vegetable_soup == 0 ~ 0,
                                               vegetable_soup == 1~ 1/30*7,
                                               vegetable_soup == 2~ 2.5/30*7,
                                               vegetable_soup == 3~ 1.5,
                                               vegetable_soup == 4~ 3))
    }
    
    if ("beef" %in% colnames(df)) {
      
      df <- df %>%
        mutate(beef_week = case_when(beef == 0 ~ 0,
                                     beef == 1~ 1/30*7,
                                     beef == 2~ 2.5/30*7,
                                     beef == 3~ 1.5,
                                     beef == 4~ 3))
    }
    
    if ("poultry" %in% colnames(df)) {
      
      df <- df %>%
        mutate(poultry_week = case_when(poultry == 0 ~ 0,
                                        poultry == 1~ 1/30*7,
                                        poultry == 2~ 2.5/30*7,
                                        poultry == 3~ 1.5,
                                        poultry == 4~ 3))
    }
    
    if ("goat" %in% colnames(df)) {
      
      df <- df %>%
        mutate(goat_week = case_when(goat == 0 ~ 0,
                                     goat == 1~ 1/30*7,
                                     goat == 2~ 2.5/30*7,
                                     goat == 3~ 1.5,
                                     goat == 4~ 3))
    }
    
    if ("pork" %in% colnames(df)) {
      
      df <- df %>%
        mutate(pork_week = case_when(pork == 0 ~ 0,
                                     pork == 1~ 1/30*7,
                                     pork == 2~ 2.5/30*7,
                                     pork == 3~ 1.5,
                                     pork == 4~ 3))
    }
    
    if ("bush_meat" %in% colnames(df)) {
      
      df <- df %>%
        mutate(bush_meat_week = case_when(bush_meat == 0 ~ 0,
                                          bush_meat == 1~ 1/30*7,
                                          bush_meat == 2~ 2.5/30*7,
                                          bush_meat == 3~ 1.5,
                                          bush_meat == 4~ 3))
    }
    
    if ("intestine" %in% colnames(df)) {
      
      df <- df %>%
        mutate(intestine_week = case_when(intestine == 0 ~ 0,
                                          intestine == 1~ 1/30*7,
                                          intestine == 2~ 2.5/30*7,
                                          intestine == 3~ 1.5,
                                          intestine == 4~ 3))
    }
    
    if ("meatballs" %in% colnames(df)) {
      
      df <- df %>%
        mutate(meatballs = case_when(meatballs == 0 ~ 0,
                                     meatballs == 1~ 1/30*7,
                                     meatballs == 2~ 2.5/30*7,
                                     meatballs == 3~ 1.5,
                                     meatballs == 4~ 3))
    }
    
    if ("fried_sausage" %in% colnames(df)) {
      
      df <- df %>%
        mutate(fried_sausage_week = case_when(fried_sausage == 0 ~ 0,
                                              fried_sausage == 1~ 1/30*7,
                                              fried_sausage == 2~ 2.5/30*7,
                                              fried_sausage == 3~ 1.5,
                                              fried_sausage == 4~ 3))
    }
    
    if ("boiled_sausage" %in% colnames(df)) {
      
      df <- df %>%
        mutate(boiled_sausage_week = case_when(boiled_sausage == 0 ~ 0,
                                               boiled_sausage == 1~ 1/30*7,
                                               boiled_sausage == 2~ 2.5/30*7,
                                               boiled_sausage == 3~ 1.5,
                                               boiled_sausage == 4~ 3))
    }
    
    if ("fatty_fish" %in% colnames(df)) {
      
      df <- df %>%
        mutate(fatty_fish_week = case_when(fatty_fish == 0 ~ 0,
                                           fatty_fish == 1~ 1/30*7,
                                           fatty_fish == 2~ 2.5/30*7,
                                           fatty_fish == 3~ 1.5,
                                           fatty_fish == 4~ 3))
    }
    
    if ("lean_fish" %in% colnames(df)) {
      
      df <- df %>%
        mutate(lean_fish_week = case_when(lean_fish == 0 ~ 0,
                                          lean_fish == 1~ 1/30*7,
                                          lean_fish == 2~ 2.5/30*7,
                                          lean_fish == 3~ 1.5,
                                          lean_fish == 4~ 3))
    }
    
    if ("fish_preparations" %in% colnames(df)) {
      
      df <- df %>%
        mutate(fish_preparations_week = case_when(fish_preparations == 0 ~ 0,
                                                  fish_preparations == 1~ 1/30*7,
                                                  fish_preparations == 2~ 2.5/30*7,
                                                  fish_preparations == 3~ 1.5,
                                                  fish_preparations == 4~ 3))
    }
    
    if ("shell_fish" %in% colnames(df)) {
      
      df <- df %>%
        mutate(shell_fish_week = case_when(shell_fish == 0 ~ 0,
                                           shell_fish == 1~ 1/30*7,
                                           shell_fish == 2~ 2.5/30*7,
                                           shell_fish == 3~ 1.5,
                                           shell_fish == 4~ 3))
    }
    
    if ("tofu" %in% colnames(df)) {
      
      df <- df %>%
        mutate(tofu_week = case_when(tofu == 0 ~ 0,
                                     tofu == 1~ 1/30*7,
                                     tofu == 2~ 2.5/30*7,
                                     tofu == 3~ 1.5,
                                     tofu == 4~ 3))
    }
    
    if ("lasagna_pizza" %in% colnames(df)) {
      
      df <- df %>%
        mutate(lasagna_pizza_week = case_when(lasagna_pizza == 0 ~ 0,
                                              lasagna_pizza == 1~ 1/30*7,
                                              lasagna_pizza == 2~ 2.5/30*7,
                                              lasagna_pizza == 3~ 1.5,
                                              lasagna_pizza == 4~ 3))
    }
    
    if ("mixed_dishes_without_meat" %in% colnames(df)) {
      
      df <- df %>%
        mutate(mixed_dishes_without_meat_week = case_when(mixed_dishes_without_meat == 0 ~ 0,
                                                          mixed_dishes_without_meat == 1~ 1/30*7,
                                                          mixed_dishes_without_meat == 2~ 2.5/30*7,
                                                          mixed_dishes_without_meat == 3~ 1.5,
                                                          mixed_dishes_without_meat == 4~ 3))
    }
    
    if ("mixed_dishes_with_meat" %in% colnames(df)) {
      
      df <- df %>%
        mutate(mixed_dishes_with_meat_week = case_when(mixed_dishes_with_meat == 0 ~ 0,
                                                       mixed_dishes_with_meat == 1~ 1/30*7,
                                                       mixed_dishes_with_meat == 2~ 2.5/30*7,
                                                       mixed_dishes_with_meat == 3~ 1.5,
                                                       mixed_dishes_with_meat == 4~ 3))
    }
    
    if ("pasta_noodles_macaroni" %in% colnames(df)) {
      
      df <- df %>%
        mutate(pasta_noodles_macaroni_week = case_when(pasta_noodles_macaroni == 0 ~ 0,
                                                       pasta_noodles_macaroni == 1~ 1/30*7,
                                                       pasta_noodles_macaroni == 2~ 2.5/30*7,
                                                       pasta_noodles_macaroni == 3~ 1.5,
                                                       pasta_noodles_macaroni == 4~ 3.5,
                                                       pasta_noodles_macaroni == 5~5))
    }
    
    if ("liverwurst" %in% colnames(df)) {
      
      df <- df %>%
        mutate(liverwurst_week = case_when(liverwurst == 0 ~ 0,
                                           liverwurst == 1~ 1/30*7,
                                           liverwurst == 2~ 2.5/30*7,
                                           liverwurst == 3~ 1.5,
                                           liverwurst == 4~ 3.5,
                                           liverwurst == 5~5))
    }
    
    if ("tart_pie" %in% colnames(df)) {
      
      df <- df %>%
        mutate(tart_pie_week = case_when(tart_pie == 0 ~ 0,
                                         tart_pie == 1~ 1/30*7,
                                         tart_pie == 2~ 2.5/30*7,
                                         tart_pie == 3~ 1.5,
                                         tart_pie == 4~ 3.5,
                                         tart_pie == 5~5))
    }
    
    if ("yeast_cake_pastry" %in% colnames(df)) {
      
      df <- df %>%
        mutate(yeast_cake_pastry_week = case_when(yeast_cake_pastry == 0 ~ 0,
                                                  yeast_cake_pastry == 1~ 1/30*7,
                                                  yeast_cake_pastry == 2~ 2.5/30*7,
                                                  yeast_cake_pastry == 3~ 1.5,
                                                  yeast_cake_pastry == 4~ 3.5,
                                                  yeast_cake_pastry == 5~5))
    }
    
    if ("cake_with_cream" %in% colnames(df)) {
      
      df <- df %>%
        mutate(cake_with_cream_week = case_when(cake_with_cream == 0 ~ 0,
                                                cake_with_cream == 1~ 1/30*7,
                                                cake_with_cream == 2~ 2.5/30*7,
                                                cake_with_cream == 3~ 1.5,
                                                cake_with_cream == 4~ 3.5,
                                                cake_with_cream == 5~5))
    }
    
    if ("whipped_cream" %in% colnames(df)) {
      
      df <- df %>%
        mutate(whipped_cream_week = case_when(whipped_cream == 0 ~ 0,
                                              whipped_cream == 1~ 1/30*7,
                                              whipped_cream == 2~ 2.5/30*7,
                                              whipped_cream == 3~ 1.5,
                                              whipped_cream == 4~ 3.5,
                                              whipped_cream == 5~5))
    }
    
    if ("cookies_biscuits" %in% colnames(df)) {
      
      df <- df %>%
        mutate(cookies_biscuits_week = case_when(cookies_biscuits == 0 ~ 0,
                                                 cookies_biscuits == 1~ 1/30*7,
                                                 cookies_biscuits == 2~ 2.5/30*7,
                                                 cookies_biscuits == 3~ 1.5,
                                                 cookies_biscuits == 4~ 3.5,
                                                 cookies_biscuits == 5~5))
    }
    
    if ("garlic_raw" %in% colnames(df)) {
      
      df <- df %>%
        mutate(garlic_raw_week = case_when(garlic_raw == 0 ~ 0,
                                           garlic_raw == 1~1,
                                           garlic_raw == 2~1))
    }
    
    if ("garlic_cooked" %in% colnames(df)) {
      
      df <- df %>%
        mutate(garlic_cooked_week = case_when(garlic_cooked == 0 ~ 0,
                                              garlic_cooked == 1~1,
                                              garlic_cooked == 2~1))
    }
    
    if ("onion_raw" %in% colnames(df)) {
      
      df <- df %>%
        mutate(onion_raw_week = case_when(onion_raw == 0 ~ 0,
                                          onion_raw == 1~1,
                                          onion_raw == 2~1))
    }
    
    if ("onion_cooked" %in% colnames(df)) {
      
      df <- df %>%
        mutate(onion_cooked_week = case_when(onion_cooked == 0 ~ 0,
                                             onion_cooked == 1~1,
                                             onion_cooked == 2~1))
    }
    
    
    if ("sodas_soft_drinks" %in% colnames(df)) {
      
      df <- df %>%
        mutate(sodas_soft_drinks_week = case_when(sodas_soft_drinks == 0 ~ 0,
                                                  sodas_soft_drinks == 1~1/30*7,
                                                  sodas_soft_drinks == 2~2.5/30*7,
                                                  sodas_soft_drinks == 3~2,
                                                  sodas_soft_drinks == 4~5,
                                                  sodas_soft_drinks == 5~1.5,
                                                  sodas_soft_drinks == 6~3.5*7,
                                                  sodas_soft_drinks == 7~5.5*7,
                                                  sodas_soft_drinks == 8~7.5*7,
                                                  sodas_soft_drinks == 9~9.5*7,
                                                  sodas_soft_drinks ==10~11*7))
    }
    
    if ("light_soft_drinks" %in% colnames(df)) {
      
      df <- df %>%
        mutate(light_soft_drinks_week = case_when(light_soft_drinks == 0 ~ 0,
                                                  light_soft_drinks == 1~1/30*7,
                                                  light_soft_drinks == 2~2.5/30*7,
                                                  light_soft_drinks == 3~2,
                                                  light_soft_drinks == 4~5,
                                                  light_soft_drinks == 5~1.5,
                                                  light_soft_drinks == 6~3.5*7,
                                                  light_soft_drinks == 7~5.5*7,
                                                  light_soft_drinks == 8~7.5*7,
                                                  light_soft_drinks == 9~9.5*7,
                                                  light_soft_drinks ==10~11*7))
    }
    
    if ("fruit_juice" %in% colnames(df)) {
      
      df <- df %>%
        mutate(fruit_juice_week = case_when(fruit_juice == 0 ~ 0,
                                            fruit_juice == 1~1/30*7,
                                            fruit_juice == 2~2.5/30*7,
                                            fruit_juice == 3~2,
                                            fruit_juice == 4~5,
                                            fruit_juice == 5~1.5,
                                            fruit_juice == 6~3.5*7,
                                            fruit_juice == 7~5.5*7,
                                            fruit_juice == 8~7.5*7,
                                            fruit_juice == 9~9.5*7,
                                            fruit_juice ==10~11*7))
    }
    
    if ("fruit_nectar" %in% colnames(df)) {
      
      df <- df %>%
        mutate(fruit_nectar_week = case_when(fruit_nectar == 0 ~ 0,
                                             fruit_nectar == 1~1/30*7,
                                             fruit_nectar == 2~2.5/30*7,
                                             fruit_nectar == 3~2,
                                             fruit_nectar == 4~5,
                                             fruit_nectar == 5~1.5,
                                             fruit_nectar == 6~3.5*7,
                                             fruit_nectar == 7~5.5*7,
                                             fruit_nectar == 8~7.5*7,
                                             fruit_nectar == 9~9.5*7,
                                             fruit_nectar ==10~11*7))
    }
    
    if ("vegetable_juice" %in% colnames(df)) {
      
      df <- df %>%
        mutate(vegetable_juice_week = case_when(vegetable_juice == 0 ~ 0,
                                                vegetable_juice == 1~1/30*7,
                                                vegetable_juice == 2~2.5/30*7,
                                                vegetable_juice == 3~2,
                                                vegetable_juice == 4~5,
                                                vegetable_juice == 5~1.5,
                                                vegetable_juice == 6~3.5*7,
                                                vegetable_juice == 7~5.5*7,
                                                vegetable_juice == 8~7.5*7,
                                                vegetable_juice == 9~9.5*7,
                                                vegetable_juice ==10~11*7))
    }
    
    if ("regular_coffee" %in% colnames(df)) {
      
      df <- df %>%
        mutate(regular_coffee_week = case_when(regular_coffee == 0 ~ 0,
                                               regular_coffee == 1~1/30*7,
                                               regular_coffee == 2~2.5/30*7,
                                               regular_coffee == 3~2,
                                               regular_coffee == 4~5,
                                               regular_coffee == 5~1.5,
                                               regular_coffee == 6~3.5*7,
                                               regular_coffee == 7~5.5*7,
                                               regular_coffee == 8~7.5*7,
                                               regular_coffee == 9~9.5*7,
                                               regular_coffee ==10~11*7))
    }
    
    if ("decaffeinated_coffee" %in% colnames(df)) {
      
      df <- df %>%
        mutate(decaffeinated_coffee_week = case_when(decaffeinated_coffee == 0 ~ 0,
                                                     decaffeinated_coffee == 1~1/30*7,
                                                     decaffeinated_coffee == 2~2.5/30*7,
                                                     decaffeinated_coffee == 3~2,
                                                     decaffeinated_coffee == 4~5,
                                                     decaffeinated_coffee == 5~1.5,
                                                     decaffeinated_coffee == 6~3.5*7,
                                                     decaffeinated_coffee == 7~5.5*7,
                                                     decaffeinated_coffee == 8~7.5*7,
                                                     decaffeinated_coffee == 9~9.5*7,
                                                     decaffeinated_coffee ==10~11*7))
    }
    
    if ("tea_black_green" %in% colnames(df)) {
      
      df <- df %>%
        mutate(tea_black_green_week = case_when(tea_black_green == 0 ~ 0,
                                                tea_black_green == 1~1/30*7,
                                                tea_black_green == 2~2.5/30*7,
                                                tea_black_green == 3~2,
                                                tea_black_green == 4~5,
                                                tea_black_green == 5~1.5,
                                                tea_black_green == 6~3.5*7,
                                                tea_black_green == 7~5.5*7,
                                                tea_black_green == 8~7.5*7,
                                                tea_black_green == 9~9.5*7,
                                                tea_black_green ==10~11*7))
    }
    
    if ("fruit_herbal_tea" %in% colnames(df)) {
      
      df <- df %>%
        mutate(fruit_herbal_tea_week = case_when(fruit_herbal_tea == 0 ~ 0,
                                                 fruit_herbal_tea == 1~1/30*7,
                                                 fruit_herbal_tea == 2~2.5/30*7,
                                                 fruit_herbal_tea == 3~2,
                                                 fruit_herbal_tea == 4~5,
                                                 fruit_herbal_tea == 5~1.5,
                                                 fruit_herbal_tea == 6~3.5*7,
                                                 fruit_herbal_tea == 7~5.5*7,
                                                 fruit_herbal_tea == 8~7.5*7,
                                                 fruit_herbal_tea == 9~9.5*7,
                                                 fruit_herbal_tea ==10~11*7))
    }
    
    if ("regular_beer" %in% colnames(df)) {
      
      df <- df %>%
        mutate(regular_beer_week = case_when(regular_beer == 0 ~ 0,
                                             regular_beer == 1~1/30*7,
                                             regular_beer == 2~2.5/30*7,
                                             regular_beer == 3~1,
                                             regular_beer == 4~2.5,
                                             regular_beer == 5~5,
                                             regular_beer == 6~7,
                                             regular_beer == 7~14))
    }
    
    if ("non_alcoholic_beer" %in% colnames(df)) {
      
      df <- df %>%
        mutate(non_alcoholic_beer_week = case_when(non_alcoholic_beer == 0 ~ 0,
                                                   non_alcoholic_beer == 1~1/30*7,
                                                   non_alcoholic_beer == 2~2.5/30*7,
                                                   non_alcoholic_beer == 3~1,
                                                   non_alcoholic_beer == 4~2.5,
                                                   non_alcoholic_beer == 5~5,
                                                   non_alcoholic_beer == 6~7,
                                                   non_alcoholic_beer == 7~14))
    }
    
    if ("wine" %in% colnames(df)) {
      
      df <- df %>%
        mutate(wine_week = case_when(wine == 0 ~ 0,
                                     wine == 1~1/30*7,
                                     wine == 2~2.5/30*7,
                                     wine == 3~1,
                                     wine == 4~2.5,
                                     wine == 5~5,
                                     wine == 6~7,
                                     wine == 7~14))
    }
    
    if ("liquor_spirits" %in% colnames(df)) {
      
      df <- df %>%
        mutate(liquor_spirits_week = case_when(liquor_spirits == 0 ~ 0,
                                               liquor_spirits == 1~1/30*7,
                                               liquor_spirits == 2~2.5/30*7,
                                               liquor_spirits == 3~1,
                                               liquor_spirits == 4~2.5,
                                               liquor_spirits == 5~5,
                                               liquor_spirits == 6~7,
                                               liquor_spirits == 7~14))
    }
    
    if ("meat_fat" %in% colnames(df)) {
      
      df <- df %>%
        mutate(meat_fat_1 = case_when(meat_fat == . ~ 3))
    }
    
    if ("meat_products_fat" %in% colnames(df)) {
      
      df <- df %>%
        mutate(meat_products_fat_1 = case_when(meat_fat == . ~ 3))
    }
    
    if ("meat_products_fat" %in% colnames(df)) {
      
      df <- df %>%
        mutate(meat_products_fat_1 = case_when(meat_fat == . ~ 0))
    }
    
    if ("palmnut_oil" %in% colnames(df)) {
      
      df <- df %>%
        mutate(palmnut_oil1 = case_when(palmnut_oil == . ~ 0))
    }
    
    if ("groundnut_paste" %in% colnames(df)) {
      
      df <- df %>%
        mutate(groundnut_paste1 = case_when(groundnut_paste == . ~ 0))
    }
    
    if ("fats_butter" %in% colnames(df)) {
      
      df <- df %>%
        mutate(fats_butter1 = case_when(fats_butter == . ~ 0))
    }
    
    if ("margarine" %in% colnames(df)) {
      
      df <- df %>%
        mutate(margarine1 = case_when(margarine == . ~ 0))
    }
    
    if ("cooking_fat" %in% colnames(df)) {
      
      df <- df %>%
        mutate(cooking_fat1 = case_when(cooking_fat == . ~ 0))
    }
    
    if ("olive_oil" %in% colnames(df)) {
      
      df <- df %>%
        mutate(olive_oil1 = case_when(olive_oil == . ~ 0))
    }
    
    if ("oils" %in% colnames(df)) {
      
      df <- df %>%
        mutate(oils1 = case_when(oils == . ~ 0))
    }
    
    if ("palmnut_oil_vegetable" %in% colnames(df)) {
      
      df <- df %>%
        mutate(palmnut_oil_vegetable1 = case_when(palmnut_oil_vegetable == . ~ 0,
                                                  palmnut_oil_vegetable == 1~0.333,
                                                  palmnut_oil_vegetable == 2~0.666,
                                                  palmnut_oil_vegetable == 3~1,
                                                  palmnut_oil_vegetable == 4~1))
    }
    
    if ("groundnut_paste_vegetable" %in% colnames(df)) {
      
      df <- df %>%
        mutate(groundnut_paste_vegetable1 = case_when(groundnut_paste_vegetable == . ~ 0,
                                                      groundnut_paste_vegetable == 1~0.333,
                                                      groundnut_paste_vegetable == 2~0.666,
                                                      groundnut_paste_vegetable == 3~1,
                                                      groundnut_paste_vegetable == 4~1))
    }
    
    if ("fats_butter_vegetable" %in% colnames(df)) {
      
      df <- df %>%
        mutate(fats_butter_vegetable1 = case_when(fats_butter_vegetable == . ~ 0,
                                                  fats_butter_vegetable == 1~0.333,
                                                  fats_butter_vegetable == 2~0.666,
                                                  fats_butter_vegetable == 3~1,
                                                  fats_butter_vegetable == 4~1))
    }
    
    if ("margarine_vegetable" %in% colnames(df)) {
      
      df <- df %>%
        mutate(margarine_vegetable1 = case_when(margarine_vegetable == . ~ 0,
                                                margarine_vegetable == 1~0.333,
                                                margarine_vegetable == 2~0.666,
                                                margarine_vegetable == 3~1,
                                                margarine_vegetable == 4~1))
    }
    
    if ("cooking_fat_vegetable" %in% colnames(df)) {
      
      df <- df %>%
        mutate(cooking_fat_vegetable1 = case_when(cooking_fat_vegetable == . ~ 0,
                                                  cooking_fat_vegetable == 1~0.333,
                                                  cooking_fat_vegetable == 2~0.666,
                                                  cooking_fat_vegetable == 3~1,
                                                  cooking_fat_vegetable == 4~1))
    }
    
    if ("olive_oil_vegetable" %in% colnames(df)) {
      
      df <- df %>%
        mutate(olive_oil_vegetable1 = case_when(olive_oil_vegetable == . ~ 0,
                                                olive_oil_vegetable == 1~0.333,
                                                olive_oil_vegetable == 2~0.666,
                                                olive_oil_vegetable == 3~1,
                                                olive_oil_vegetable == 4~1))
    }
    
    if ("oils_vegetable" %in% colnames(df)) {
      
      df <- df %>%
        mutate(oils_vegetable1 = case_when(oils_vegetable == . ~ 0,
                                           oils_vegetable == 1~0.333,
                                           oils_vegetable == 2~0.666,
                                           oils_vegetable == 3~1,
                                           oils_vegetable == 4~1))
    }
    
    if ("sauce_meat_fish" %in% colnames(df)) {
      
      df <- df %>%
        mutate(sauce_meat_fish1 = case_when(sauce_meat_fish == . ~ 0))
    }
    
    if ("sauce_vegetables" %in% colnames(df)) {
      
      df <- df %>%
        mutate(sauce_vegetables1 = case_when(sauce_vegetables == . ~ 0))
    }
    
    if ("sauce_pasta_rice" %in% colnames(df)) {
      
      df <- df %>%
        mutate(sauce_pasta_rice1 = case_when(sauce_pasta_rice == . ~ 0))
    }
    
    if ("olive_oil_dressing" %in% colnames(df)) {
      
      df <- df %>%
        mutate(olive_oil_dressing1 = case_when(olive_oil_dressing == . ~ 0))
    }
    
    if ("oils_dressing" %in% colnames(df)) {
      
      df <- df %>%
        mutate(oils_dressing1 = case_when(oils_dressing == . ~ 0))
    }
    
    if ("mayonnaise_dressing" %in% colnames(df)) {
      
      df <- df %>%
        mutate(mayonnaise_dressing1 = case_when(mayonnaise_dressing == . ~ 0))
    }
    
    if ("cream_dressing" %in% colnames(df)) {
      
      df <- df %>%
        mutate(cream_dressing1 = case_when(cream_dressing == . ~ 0))
    }
    
    if ("yoghurt_dressing" %in% colnames(df)) {
      
      df <- df %>%
        mutate(yoghurt_dressing1 = case_when(yoghurt_dressing == . ~ 0))
    }
    
    if ("fresh_herbs_dressing" %in% colnames(df)) {
      
      df <- df %>%
        mutate(fresh_herbs_dressing1 = case_when(fresh_herbs_dressing == . ~ 0))
    }
    
    if ("vinegar_dressing" %in% colnames(df)) {
      
      df <- df %>%
        mutate(vinegar_dressing1 = case_when(vinegar_dressing == . ~ 0))
    }
    
    if ("palmnut_oil" %in% colnames(df)) {
      
      df <- df %>%
        mutate(palmnut_oil_meat_fish1 = case_when(palmnut_oil == 1~0.333,
                                                  palmnut_oil == 2~0.666,
                                                  palmnut_oil == 3~1,
                                                  palmnut_oil == 4~1))
    }
    
    if ("fats_butter" %in% colnames(df)) {
      
      df <- df %>%
        mutate(fats_butter_meat_fish1 = case_when(fats_butter == 1~0.333,
                                                  fats_butter == 2~0.666,
                                                  fats_butter == 3~1,
                                                  fats_butter == 4~1))
    }
    
    if ("margarine" %in% colnames(df)) {
      
      df <- df %>%
        mutate(margarine_meat_fish1 = case_when(margarine == 1~0.333,
                                                margarine == 2~0.666,
                                                margarine == 3~1,
                                                margarine == 4~1))
    }
    
    if ("cooking_fat" %in% colnames(df)) {
      
      df <- df %>%
        mutate(cooking_fat_meat_fish1 = case_when(cooking_fat == 1~0.333,
                                                  cooking_fat == 2~0.666,
                                                  cooking_fat == 3~1,
                                                  cooking_fat == 4~1))
    }
    
    if ("olive_oil" %in% colnames(df)) {
      
      df <- df %>%
        mutate(olive_oil_meat_fish1 = case_when(olive_oil == 1~0.333,
                                                olive_oil == 2~0.666,
                                                olive_oil == 3~1,
                                                olive_oil == 4~1))
    }
    
    if ("oils" %in% colnames(df)) {
      
      df <- df %>%
        mutate(oils_meat_fish1 = case_when(oils == 1~0.333,
                                           oils == 2~0.666,
                                           oils == 3~1,
                                           oils == 4~1))
    }
    
      
      vegatable_fat_components <- c("leaves_spinach_week","cooked_white_cabbage_week","egg_plant_week","beans_week")
      existing_vegetable_fat_components <- intersect(vegatable_fat_components, colnames(df))
    
    if (length(existing_vegetable_fat_components) > 0) {
      df <- df %>%
        mutate(fat_freq_veg = (leaves_spinach_week+cooked_white_cabbage_week+egg_plant_week+beans_week)*(1-sauce_vegetables1))
    } else {
      df$fat_freq_veg <- NA  # Initialize veg_fat if no components exist
    }
      
    total_fat_freq_vegetable<-c("palmnut_oil_vegetable","groundnut_paste_vegetable","fats_butter_vegetable","margarine_vegetable",
                                "cooking_fat_vegetable","olive_oil_vegetable","oils_vegetable")
    existing_total_fat_freq_vegetable<-intersect(total_fat_freq_vegetable, colnames(df))
    
    if (length(existing_total_fat_freq_vegetable) > 0) {
      df <- df %>%
        mutate(total_fat_freq_veg = palmnut_oil_vegetable1+fats_butter_vegetable1+margarine_vegetable1+cooking_fat_vegetable1+
                 olive_oil_vegetable1+oils_vegetable1+groundnut_paste_vegetable1)
    } else {
      df$total_fat_freq_veg <- NA  # Initialize veg_fat if no components exist
    }
    
  if(length(existing_total_fat_freq_vegetable)>0){
    
      df<-df%>% mutate(Proportion_palmnut_oil_veg=ifelse(!is.na(total_fat_freq_vegetable),
                                                             (palmnut_oil_vegetable1/total_fat_freq_vegetable),0),
                     Proportion_butter_veg=ifelse(!is.na(total_fat_freq_vegetable),
                                                  (fats_butter_vegetable1/total_fat_freq_vegetable),0),
                     Proportion_margarine_veg=ifelse(!is.na(total_fat_freq_vegetable),
                                                  (margarine_vegetable1/total_fat_freq_vegetable),0),
                     Proportion_cooking_fat_veg=ifelse(!is.na(total_fat_freq_vegetable),
                                                  (cooking_fat_vegetable1/total_fat_freq_vegetable),0),
                     Proportion_olive_oil_veg=ifelse(!is.na(total_fat_freq_vegetable),
                                                  (olive_oil_vegetable1/total_fat_freq_vegetable),0),
                     Proportion_oils_veg=ifelse(!is.na(total_fat_freq_vegetable),
                                                  (oils_vegetable1/total_fat_freq_vegetable),0),
                     Proportion_groundnut_paste_veg=ifelse(!is.na(total_fat_freq_vegetable),
                                                  (groundnut_paste_vegetable1/total_fat_freq_vegetable),0))%>%
                  mutate(total_palmnutoil_veg=Proportion_palmnut_oil_veg*fat_freq_veg,
                         total_butter_veg=Proportion_butter_veg*fat_freq_veg,
                         total_margarine_veg=Proportion_margarine_veg*fat_freq_veg,
                         total_cooking_fat_veg=Proportion_cooking_fat_veg*fat_freq_veg,
                         total_olive_oil_veg=Proportion_olive_oil_veg*fat_freq_veg,
                         total_oils_veg=Proportion_oils_veg*fat_freq_veg,
                         total_ground_paste_veg=Proportion_groundnut_paste_veg*fat_freq_veg)
       }
    
    meat_fish_fat_components <- c("beef_week","poultry_week","goat_week","pork_week","bush_meat_week","intestine_week","meatballs_week",
                                  "fried_sausage_week","boiled_sausage_week","fatty_fish_week","lean_fish_week")
    existing_meat_fish_fat_components <- intersect(meat_fish_fat_components, colnames(df))
    
    if (length(existing_meat_fish_fat_components) > 0) {
      df <- df %>%
        mutate(fat_freq_meat_fish = (beef_week+poultry_week+goat_week+pork_week+bush_meat_week+intestine_week+
                                 meatballs_week+fried_sausage_week+boiled_sausage_week+fatty_fish_week+lean_fish_week)*(1-sauce_meat_fish1))
    } else {
      df$fat_freq_meat_fish <- NA  # Initialize meat_fish_fat if no components exist
    }
    
    total_freq_fat_components <- c("palmnut_oil_meat_fish1","fats_butter_meat_fish1","margarine_meat_fish1","cooking_fat_meat_fish1",
                                   "olive_oil_meat_fish1","oils_meat_fish1","groundnut_paste1")
    existing_meat_fish_fat_components <- intersect(total_freq_fat_components, colnames(df))
    
    if (length(existing_meat_fish_fat_components) > 0) {
      df <- df %>%
        mutate(total_fats_freq_meat = palmnut_oil_meat_fish1+fats_butter_meat_fish1+margarine_meat_fish1+cooking_fat_meat_fish1+
                                         Olive_oil_meat_fish1+oils_meat_fish1+groundnut_paste1)
    } else {
      df$total_fats_freq_meat <- NA  # Initialize meat_fish_fat if no components exist
    }
    
    if(length(existing_meat_fish_fat_components)>0){
    
    df<-df%>% mutate(Proportion_palmnut_oil_meat=ifelse(!is.na(total_fats_freq_meat),
                                                       (palmnut_oil_meat_fish1/total_fats_freq_meat),0),
                     Proportion_butter_meat=ifelse(!is.na(total_fats_freq_meat),
                                                  (fats_butter_meat_fish1/total_fats_freq_meat),0),
                     Proportion_margarine_meat=ifelse(!is.na(total_fats_freq_meat),
                                                     (margarine_meat_fish1/total_fats_freq_meat),0),
                     Proportion_cooking_fat_meat=ifelse(!is.na(total_fats_freq_meat),
                                                       (cooking_fat_meat_fish1/total_fats_freq_meat),0),
                     Proportion_olive_oil_meat=ifelse(!is.na(total_fats_freq_meat),
                                                     (Olive_oil_meat_fish1/total_fats_freq_meat),0),
                     Proportion_oils_meat=ifelse(!is.na(total_fats_freq_meat),
                                                (oils_meat_fish1/total_fats_freq_meat),0),
                     Proportion_groundnut_paste_meat=ifelse(!is.na(total_fats_freq_meat),
                                                           (groundnut_paste1/total_fats_freq_meat),0))%>%
      mutate(total_palmnutoil_meat=Proportion_palmnut_oil_meat*fat_freq_meat_fish,
             total_butter_meat=Proportion_butter_meat*fat_freq_meat_fish,
             total_margarine_meat=Proportion_margarine_meat*fat_freq_meat_fish,
             total_cooking_fat_meat=Proportion_cooking_fat_meat*fat_freq_meat_fish,
             total_olive_oil_meat=Proportion_olive_oil_meat*fat_freq_meat_fish,
             total_oils_meat=Proportion_oils_meat*fat_freq_meat_fish,
             total_ground_paste_meat=Proportion_groundnut_paste_meat*fat_freq_meat_fish) 
    }  
  
    sauce2_components <- c("leaves_spinach_week","cooked_white_cabbage_week","egg_plant_week","beans_week")
    existing_sauce2_components <- intersect(sauce2_components, colnames(df))
    
    if (length(existing_sauce2_components) > 0) {
      df <- df %>%
        mutate(sauce_vegetables2 = (leaves_spinach_week+cooked_white_cabbage_week+egg_plant_week+beans_week)*sauce_vegetables1)
    } else {
      df$sauce_vegetables2 <- NA  # Initialize veg_fat if no components exist
    }
    
    saucemeat2_components <- c("beef_week","poultry_week","goat_week","pork_week","bush_meat_week","intestine_week","meatballs_week",
                               "fried_sausage_week","boiled_sausage_week","fatty_fish_week","lean_fish_week")
    existing_saucemeat2_components <- intersect(saucemeat2_components, colnames(df))
    
    if (length(existing_saucemeat2_components) > 0) {
      df <- df %>%
        mutate(sauce_meat_fish2 = (beef_week+poultry_week+goat_week+pork_week+bush_meat_week+intestine_week+
                                  meatballs_week+fried_sausage_week+boiled_sausage_week+fatty_fish_week+lean_fish_week)*sauce_meat_fish1)
    } else {
      df$sauce_meat_fish2 <- NA  # Initialize veg_fat if no components exist
    }
    
    saucerice2_components <- c("rice_week","pasta_noodles_macaroni_week")
    existing_saucerice2_components <- intersect(saucerice2_components, colnames(df))
    
    if (length(existing_saucerice2_components) > 0) {
      df <- df %>%
        mutate(sauce_rice_pasta2 = (rice_week + pasta_noodles_macaroni_week)*sauce_rice_pasta1)
    } else {
      df$sauce_rice_pasta2 <- NA  # Initialize veg_fat if no components exist
    }
    
    saucesalad2_components <- c("tomatoes_week","cucumber_week","peppers_week","lettuce_endive_week")
    existing_saucesalad2_components <- intersect(saucesalad2_components, colnames(df))
    
    if (length(existing_saucerice2_components) > 0) {
      df <- df %>%
        mutate(sauce_salad1 = (tomatoes_week + cucumber_week + peppers_week + lettuce_endive_week)/4)
    } else {
      df$sauce_salad1 <- NA  # Initialize veg_fat if no components exist
    }
    
    dressing_components <- c("olive_oil_dressing","oils_dressing","mayonnaise_dressing","cream_dressing","yoghurt_dressing",
                             "vinegar_dressing")
    existing_dressing_components <- intersect(dressing_components, colnames(df))
    
    if (length(existing_dressing_components) > 0) {
      df <- df %>%
        mutate(total_dressing = (olive_oil_dressing1 + oils_dressing1 + mayonnaise_dressing1 + cream_dressing1+yoghurt_dressing1+
                                 vinegar_dressing1))
    } else {
      df$total_dressing <- NA  # Initialize veg_fat if no components exist
    }
    
    if(length(existing_dressing_components)>0){
      
         df<-df%>%
           mutate(freq_olive_oil= ifelse(!is.na(olive_oil_dressing1),(olive_oil_dressing1/total_dressing),0),
                         freq_oils= ifelse(!is.na(oils_dressing1),(oils_dressing1/total_dressing),0),
                         freq_mayonnaise= ifelse(!is.na(mayonnaise_dressing1),(mayonnaise_dressing1/total_dressing),0),
                         freq_cream= ifelse(!is.na(cream_dressing1),(cream_dressing1/total_dressing),0),
                         freq_yoghurt= ifelse(!is.na(yoghurt_dressing1),(yoghurt_dressing1/total_dressing),0),
                         freq_vinegar= ifelse(!is.na(vinegar_dressing1),(vinegar_dressing1/total_dressing),0))%>%
           mutate(olive_oil_dressing2=ifelse(!is.na(freq_olive_oil)&freq_olive_oil!=0,(sauce_salad1*freq_olive_oil),0),
                  oils_dressing2=ifelse(!is.na(freq_oils)&freq_oils!=0,(sauce_salad1*freq_oils),0),
                  mayonnaise_dressing2=ifelse(!is.na(freq_mayonnaise)&freq_mayonnaise!=0,(sauce_salad1*freq_mayonnaise),0),
                  cream_dressing2=ifelse(!is.na(freq_cream)&freq_cream!=0,(sauce_salad1*freq_cream),0),
                  yoghurt_dressing2=ifelse(!is.na(freq_yoghurt)&freq_yoghurt!=0,(sauce_salad1*freq_yoghurt),0),
                  vinegar_dressing2=ifelse(!is.na(freq_vinegar)&freq_vinegar!=0,(sauce_salad1*freq_vinegar),0))
      
    }
    
    ###food groups
    
    food_groups<-list(whole_grain_bread_cereals=c("rye_multigrain_bread_week","whole_grain_bread_buns_week","crispbread_week",
                                                  "muesli_cereals_week","other_grains_week"),
                      white_bread_cereals=c("white_wheat_bread_buns_week","crisp_week","hot_cereal_porridge_week"),
                      sweet_spreads=c("marmelade_jam_jelly_honey_week"),
                      dairy_prodcuts=c("cocoa_chocolate_milk_fruit_week","milk_week","plain_yoghurt_buttermilk_week","sour_milk_week",
                                       "flavoured_yoghurt_week","greek_yoghurt_week","cream_cheese_week","soft_cheese_week",
                                       "semi_soft_firm_cheese_week","mozarella_mascarpone_feta_week","butter_week","total_butter_veg",
                                       "whipped_cream_week"),
                      fruit=c("apple_pear_fresh_week","orange_week","banana_week","plum_week","strawberries_cherries_week",
                              "water_melon_week","mango_pawpaw_week","berries_week","grapes_week","stewed_fruit_week"),
                      nuts_seeds=c("dried_fruit_week","nuts_almonds_week","seeds_week"),
                      roots_tubers_plantain=c("plantain_week","cassava_week","yam_week","plantain_dough_week"),
                      fermented=c("maize_dough_week","fermented_maize_week"),
                      vegetables=c("carrots_week","tomatoes_week","lettuce_endive_week","cucumber_week","peppers_week",
                                   "leaves_spinach_week","cooked_white_cabbage_week","egg_plant_week","beans_week","garlic_raw_week",
                                   "garlic_cooked_week","onions_raw_week","onions_cooked_week"),
                      legumes=c("groundnut_soup_week","legumes_week","lentil_pea_bean_soup_week"),
                      vegetable_soups_stew_sauces=c("palmnut_soup_week","green_stew_week","okro_stew_week","tomato_sauce_stew_week",
                                                    "vegetable_soup_week"),
                      rice_pasta=c("rice_week","pasta_noodles_macaroni_week"),
                      egg=c("egg_week"),
                      red_meat=c("beef_week","goat_week","pork_week","bush_meat_week","intestine_week"),
                      poultry=c("poultry_week"),
                      processed_meat=c("meatballs_week","fried_sausage_week","boiled_sausage_week","dry_cured_meat_week","salami_week",
                                       "jagdwurst_week","liverwurst_week"),
                      fish=c("fatty_fish_week","lean_fish_week","fish_preparations_week","shell_fish_week"),
                      meaty_mixed_dishes=c("lasagna_pizza_week","mixed_dishes_with_meat_week"),
                      vegetarian_mixed_dishes=c("mixed_dishes_without_meat_week","tofu_week"),
                      cakes_sweets=c("tart_pie_week","yeast_cake_pastry_week","cake_with_cream_week","cookies_biscuits_week",
                                     "chocolate_week","sweets_candy_week"),
                      coffee_tea=c("regular_coffee_week","decaffeinated_coffee_week","tea_black_green_week","fruits_herbal_tea_week"),
                      alcoholic_beverages=c("regular_beer_week","wine_week","liquor_spirits_week"),
                      soda_juices=c("non_alcoholic_beer_week","sodas_soft_drinks_week","light_soft_drinks_week","fruit_juice_week",
                                    "fruit_nectar_week","vegetable_juice_week"),
                      palm_oil=c("total_palmnutoil_veg","total_palmnutoil_meat"),
                      olive_oil=c("total_olive_oil_veg","olive_oil_dressing2","total_olive_oil_meat"),
                      other_oils=c("total_oils_veg","oils_dressing2","total_groundnut_paste_veg","peanut_butter_week","total_oils_meat",
                                   "total_groundnut_paste_veg"),
                      margarine=c("total_margarine_veg","regular_week","fat_reduced_margarine_week","total_margarine_meat"),
                      cookings_fat=c("total_cooking_fat_veg","total_cooking_fat_meat"),
                      condiments=c("ketchup_week","mayonnaise_dressing2","cream_dressing2","yoghurt_dressing2","vinegar_dressing2","sauce_vegetables2",
                                   "sauce_meat_fish2","sauce_rice_pasta2"))
    
    for (group_name in names(food_groups)) {
      group_items <- food_groups[[group_name]]  # Get list of food items in this group
      available_items <- intersect(group_items, colnames(df))  # Find which items exist in df
      
      if (length(available_items) > 0) {  # If at least one required food item is present
        df <- df %>%
          mutate(!!group_name := rowSums(select(df, all_of(available_items)), na.rm = TRUE))
      }
    }
    
    
    
    
    if("bread_rolls_buns" %in% colnames(df)){
      
      df<-df%>% mutate(bread_rolls_buns_day=case_when(bread_rolls_buns==0~0,
                                                      bread_rolls_buns==1~6/7,
                                                      bread_rolls_buns==2~1.5,
                                                      bread_rolls_buns==3~3.5,
                                                      bread_rolls_buns==4~5.5,
                                                      bread_rolls_buns==5~7.5,
                                                      bread_rolls_buns==6~9))
      
      if ("rye_multigrain_bread" %in% colnames(df)) {
        df <- df %>%
          mutate(rye_multigrain_bread1 = case_when(rye_multigrain_bread == 0 ~ 0,
                                                   rye_multigrain_bread == 1 ~ 0.1,
                                                   rye_multigrain_bread == 2 ~ 0.4,
                                                   rye_multigrain_bread == 3 ~ 0.77,
                                                   rye_multigrain_bread == 4 ~ 1
          ))
      }
      
      if ("whole_grain_bread_buns" %in% colnames(df)) {
        df <- df %>%
          mutate(whole_grain_bread_buns1 = case_when(whole_grain_bread_buns == 0 ~ 0,
                                                     whole_grain_bread_buns == 1 ~ 0.1,
                                                     whole_grain_bread_buns == 2 ~ 0.4,
                                                     whole_grain_bread_buns == 3 ~ 0.77,
                                                     whole_grain_bread_buns == 4 ~ 1
          ))
      }
      
      if ("white_wheat_bread_buns" %in% colnames(df)) {
        df <- df %>%
          mutate(whole_grain_bread_buns1 = case_when(whole_grain_bread_buns == 0 ~ 0,
                                                     whole_grain_bread_buns == 1 ~ 0.1,
                                                     whole_grain_bread_buns == 2 ~ 0.4,
                                                     whole_grain_bread_buns == 3 ~ 0.77,
                                                     whole_grain_bread_buns == 4 ~ 1
          ))
      }
      
      if ("crisp" %in% colnames(df)) {
        df <- df %>%
          mutate(crisp1 = case_when(crisp == 0 ~ 0,
                                    crisp == 1 ~ 0.1,
                                    crisp == 2 ~ 0.4,
                                    crisp == 3 ~ 0.77,
                                    crisp == 4 ~ 1
          ))
      }
      
      if ("crispbread" %in% colnames(df)) {
        df <- df %>%
          mutate(crispbread1 = case_when(crispbread == 0 ~ 0,
                                         crispbread == 1 ~ 0.1,
                                         crispbread == 2 ~ 0.4,
                                         crispbread == 3 ~ 0.77,
                                         crispbread == 4 ~ 1
          ))
      }
      
      if ("butter" %in% colnames(df)) {
        df <- df %>%
          mutate(butter1 = case_when(butter == 0 ~ 0,
                                     butter == 1 ~ 0.1,
                                     butter == 2 ~ 0.4,
                                     butter == 3 ~ 0.77,
                                     butter == 4 ~ 1
          ))
      }
      
      if ("regular" %in% colnames(df)) {
        df <- df %>%
          mutate(regular1 = case_when(regular == 0 ~ 0,
                                      regular == 1 ~ 0.1,
                                      regular == 2 ~ 0.4,
                                      regular == 3 ~ 0.77,
                                      regular == 4 ~ 1
          ))
      }
      
      if ("fat_reduced_margarine" %in% colnames(df)) {
        df <- df %>%
          mutate(fat_reduced_margarine1 = case_when(fat_reduced_margarine == 0 ~ 0,
                                                    fat_reduced_margarine == 1 ~ 0.1,
                                                    fat_reduced_margarine == 2 ~ 0.4,
                                                    fat_reduced_margarine == 3 ~ 0.77,
                                                    fat_reduced_margarine == 4 ~ 1
          ))
      }
      
      if ("peanut_butter" %in% colnames(df)) {
        df <- df %>%
          mutate(peanut_butter1 = case_when(peanut_butter == 0 ~ 0,
                                            peanut_butter == 1 ~ 0.1,
                                            peanut_butter == 2 ~ 0.4,
                                            peanut_butter == 3 ~ 0.77,
                                            peanut_butter == 4 ~ 1
          ))
      }
      
    }  else {
      
      df$bread_rolls_buns_day <- NA
      df$rye_multigrain_bread1 <- NA
      df$whole_grain_bread_buns1 <- NA
      df$white_wheat_bread_buns1 <- NA
      df$crisp1 <- NA
      df$crispbread1 <- NA
      df$butter1<-NA
      df$regular1<-NA
      df$fat_reduced_margarine1<-NA
      df$peanut_butter1<-NA
      
    }
    
    bread_components_day <- c("rye_multigrain_bread1", "whole_grain_bread_buns1", "white_wheat_bread_buns1", "crisp1", "crispbread1")
    existing_components_day <- intersect(bread_components_day, colnames(df))
    
    if (length(existing_components_day) > 0) {
      df <- df %>%
        mutate(bread_day = rowSums(select(df, all_of(existing_components_day)), na.rm = TRUE))
    } else {
      df$bread_day <- NA  # Initialize `bread` if no components exist
    }
    
    spread_components_day<-c("butter1","regular1","fat_reduced_margarine1","peanut_butter1")
    existing_spread_components_day<-intersect(spread_components_day,colnames(df))
    
    if (length(existing_spread_components_day) > 0) {
      df <- df %>%
        mutate(spreads_day = rowSums(select(df, all_of(existing_spread_components_day)), na.rm = TRUE))
    } else {
      df$spreads_day <- NA  # Initialize `spreads` if no components exist
    }
    
    df <- df %>%
      mutate(
        rye_multigrain_bread_day = ifelse(!is.na(bread_day) & bread_day > 0, (rye_multigrain_bread1 / bread_day) * bread_rolls_buns_day*50, NA),
        whole_grain_bread_buns_day = ifelse(!is.na(bread_day) & bread_day > 0, (whole_grain_bread_buns1 / bread_day) * bread_rolls_buns_day*50, NA),
        white_wheat_bread_buns_day = ifelse(!is.na(bread_day) & bread_day>0, (white_wheat_bread_buns1/bread_day) * bread_rolls_buns_day * 50),
        butter_day = ifelse(!is.na(spreads_day) & spreads_day > 0, (butter1 / spreads_day) * bread_rolls_buns_day*8, NA),
        regular_day = ifelse(!is.na(spreads_day) & spreads_day > 0, (regular1 / spreads_day) * bread_rolls_buns_day*8, NA),
        fat_reduced_margarine_day = ifelse(!is.na(spreads_day) & spreads_day > 0, (fat_reduced_margarine1 / spreads_day*6) * bread_rolls_buns_day, NA),
        regular_day = ifelse(!is.na(spreads_day) & spreads_day > 0, (regular1 / spreads_day) * bread_rolls_buns_day*34, NA),
        crisp_day = ifelse(!is.na(bread_day) & bread_day > 0, (crisp1 / bread_day) * bread_rolls_buns_day*30, NA),
        crispbread_day = ifelse(!is.na(bread_day)& bread_day >0, (crispbread1/bread_day)*bread_rolls_buns_day*30,NA)
      )
    
    if ("muesli_cereals" %in% colnames(df)) {
      
      df <- df %>%
        mutate(muesli_cereals_day = case_when(muesli_cereals == 0 ~ 0,
                                              muesli_cereals == 1~ 1/30*80,
                                              muesli_cereals == 2~ 2.5/30*80,
                                              muesli_cereals == 3~ 1.5/7*80,
                                              muesli_cereals == 4~ 3.5/7*80,
                                              muesli_cereals == 5~5.5/7*80,
                                              muesli_cereals == 6~1*80))
    }
    
    if ("hot_cereal_porridge" %in% colnames(df)) {
      
      df <- df %>%
        mutate(hot_cereal_porridge_day = case_when(hot_cereal_porridge == 0 ~ 0,
                                                   hot_cereal_porridge == 1~ 1/30*300,
                                                   hot_cereal_porridge == 2~ 2.5/30*300,
                                                   hot_cereal_porridge == 3~ 1.5/7*300,
                                                   hot_cereal_porridge == 4~ 3.5/7*300,
                                                   hot_cereal_porridge == 5~5.5/7*300,
                                                   hot_cereal_porridge == 6~1*300))
    }
    
    if ("marmelade_jam_jelly_honey" %in% colnames(df)) {
      
      df <- df %>%
        mutate(marmelade_jam_jelly_honey_day = case_when(marmelade_jam_jelly_honey == 0 ~ 0,
                                                         marmelade_jam_jelly_honey == 1~ 1/30*15,
                                                         marmelade_jam_jelly_honey == 2~ 2.5/30*15,
                                                         marmelade_jam_jelly_honey == 3~ 1.5/7*15,
                                                         marmelade_jam_jelly_honey == 4~ 3.5/7*15,
                                                         marmelade_jam_jelly_honey == 5~5.5/7*15,
                                                         marmelade_jam_jelly_honey =F= 6~1.5*15,
                                                         marmelade_jam_jelly_honey == 7~3*15))
    }
    
    if ("cream_cheese" %in% colnames(df)) {
      
      df <- df %>%
        mutate(cream_cheese_day = case_when(cream_cheese == 0 ~ 0,
                                             cream_cheese == 1~ 1/30*20,
                                             cream_cheese == 2~ 2.5/30*20,
                                             cream_cheese == 3~ 1.5/7*20,
                                             cream_cheese == 4~ 3.5/7*20,
                                             cream_cheese == 5~5.5/7*20,
                                             cream_cheese == 6~1*20))
    }
    
    if ("apple_pear_fresh" %in% colnames(df)) {
      
      df <- df %>%
        mutate(apple_pear_fresh_week = case_when(apple_pear_fresh == 0 ~ 0,
                                                 apple_pear_fresh == 1~ 1/30*125,
                                                 apple_pear_fresh == 2~ 2.5/30*125,
                                                 apple_pear_fresh == 3~ 1.5/7*125,
                                                 apple_pear_fresh == 4~ 3.5/7*125,
                                                 apple_pear_fresh == 5~5.5/7*125,
                                                 apple_pear_fresh == 6~1.5*125,
                                                 apple_pear_fresh == 7~3*125))
    }

   
    df  # Return processed data
  })
  
  # Display processed data
  output$processed_data <- renderTable({
    req(processed_data())
    head(processed_data(), 10)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


   