library(rio)
library(here)
library(readr)
library(tidyverse)
library(stringr)
library(shiny)
library(recipes)
library(bslib)
library(DT)
library(viridis)
library(glmnet)
library(caret)
library(vip)
library(gridExtra)
library(tidymodels)
library(tibble)

#data 
data <- import(here("data/wieiad_cleaned.csv"))

data_info <- data %>%
  select(current_mood, group, ON, SAC, TII, BD, selfesteem, exce_exercise, mother, vlogexperience, help_received, recent_help, race_ethnicity, Age)

data_info <- data_info %>%
  mutate(help_received = if_else(is.na(help_received), 0, help_received)) %>%
  mutate(help_received = recode(help_received,
                                "1" = "yes",
                                "0" = "no"))

data_info <- data_info %>%
  mutate(recent_help = if_else(is.na(recent_help), 0, recent_help))

data_info <- data_info %>%
  mutate(group = recode(group,
                        "Group1_C" = "non-food vlog",
                        "Group2_N" = "diet WIEIAD vlog",
                        "Group3_I" = "non-diet WIEIAD vlog",
                        "Group4_ID" = "non-diet WIEIAD vlog with prompts"))

data_info <- data_info %>%
  mutate(across(c(ON, SAC, BD, exce_exercise, mother), ~ round(., 2)))

race_choices <- c(
  "Asian",
  "American Indian or Alaska Native",
  "Black or African American",
  "Caucasian or White",
  "Hispanic or Latino",
  "Native Hawaiian or Pacific Islander",
  "Other"
)

# Define UI for application 
ui <- navbarPage(
  title = "",
  theme = bs_theme(
    bootswatch = "united"),
  
  # ------------------ TAB 1 INTRO ------------------
  tabPanel("Introduction",
             tags$style(HTML("
.embed-grid {display:grid;grid-template-columns:repeat(2,1fr);gap:20px;}
.embed-box{max-width:420px;margin:auto;}
")),
             h2("Are #WhatIEatInADay Making You Obsessed with \"Healthy\"?"),
             
             p("Have you ever watched #WhatIEatInADay (WIEIAD) videos and thought, \"I should eat like that too\"?"),
             
             HTML(paste0(
               '<div class="embed-grid">',
               '<div class="embed-box">',
               '<iframe src="https://www.tiktok.com/embed/7597822502827543828" width="325" height="575"></iframe>',
               '</div>',
               '<div class="embed-box">',
               '<blockquote class="instagram-media" data-instgrm-permalink="https://www.instagram.com/reel/DQ1ThmHiLdu/"></blockquote>',
               '</div>',
               '</div>',
               '<script async src="//www.instagram.com/embed.js"></script>'
               )),
             
             p("These short clips often show perfectly portioned meals, colorful ingredients, and a disciplined approach to eating that 
               looks inspiring and healthy. For many viewers, these videos feel motivating: 
               a glimpse into a lifestyle centered on self-care and \"clean\" eating. 
               But what if these videos are quietly shaping how we think about food in ways we don't realize?"),
           
             p("Studies have long shown that ", 
               tags$a(
               href = "https://doi.org/10.1016/j.bodyim.2024.101841",
               "appearance-focused media can influence how we feel about our bodies.",
               target = "_blank"
               ), "It can lead to comparison, dissatisfaction, and pressure to live up to unrealistic ideals. 
               A similar process may be happening with food."),
           
             p("While eating \"healthy\" is generally encouraged (though what counts as \"healthy\" and who gets to define it is not always clear), 
               an extreme preoccupation with healthy eating can quietly take a different turn. 
               People may begin comparing what they eat to what others eat, feeling dissatisfied with their own meals, 
               and developing a distorted idea of what an \"ideal\" diet should look like."),

             tags$div(
               style = "text-align: center;",
               tags$img(
                 src = "images/toast.jpg",
                 width = "450px"
               )
             ),
  
            p(HTML(
            "This pattern of thinking is known as <b>orthorexia</b>: unhealthy obsession with \"healthy\" eating. 
            It often hides behind good intentions but <a href='https://pmc.ncbi.nlm.nih.gov/articles/PMC10490497/' target='_blank'>
              can lead to anxiety, guilt around food, and rigid eating patterns that negatively affect both mental and physical well-being.</a>"
              )),
  
            p("This project aims to make these hidden connections easier to understand through interactive visualization that show 
              how different types of message around food embedded in daily #WhatIEatInaDay social media content may shape perceptions of body image, 
              health, and food."),
            
            h3("By Exploring This Project..."),
           
            tags$div(
              style = "text-align: center;",
              tags$img(
                src = "images/3943536.jpg",
                width = "450px"
              )
            ),

            p(HTML("
            1.  You'll see how different types of WIEIAD/vlog messages can make viewers focus too much on eating \"healthy\"<br>
            2.  You'll learn how comparing yourself to others and feeling dissatisfied with your body 
            can connect to an obsession with \"healthy\" eating<br>
            3.  You'll discover which psychological, behavioral, and social predictors make some people 
            more prone to othorexic tendencies than others<br>
            4.  You'll get glimpse into how viewers respond to non-restrictive weight-inclusive content, 
                   and how positive approaches to eating influence thoughts and feelings about food."))),

# ------------------ TAB 2 DATA ------------------
    tabPanel("Data",
           h2("About the Data"),
           p("This project used data from an online experiment with 471 young women (ages 19-25). They watched one of four types of vlogs:"),
           
           p(HTML("
           1)  non-food related vlogs<br>
           2)  weight-normative diet WIEIAD vlogs<br>
           3)  weight-inclusive non-diet WIEIAD vlogs<br>
           4)  weight-inclusive non-diet WIEIAD vlogs with prompts encouraging viewers to reflect on the positive effects of \"unhealthy\" food")),
           
           p("After watching, participants responded their thoughts on food, body image, and tendencies toward extreme healthy eating."),
           
           tags$div(
             style = "text-align: center;",
             tags$img(
               src = "images/brainfood.jpeg",
               width = "450px"
             )
           ),
           
           p("Table below gives a quick look at the data used for this project. Feel free to take a look around the data!"),
           
           fluidRow(
             column(4,
                    checkboxGroupInput(inputId = 'vlog_type',
                                       label = 'Vlog Type:',
                                       choices = unique(data_info$group),
                                       selected = unique(data_info$group))
             ),
             column(4,
                    checkboxGroupInput(inputId = 'help_received',
                                       label = 'ED Treatment Experience:',
                                       choices = unique(data_info$help_received),
                                       selected = unique(data_info$help_received))
             ),
             column(4,
                    checkboxGroupInput(inputId = 'race_ethnicity',
                                       label = 'Race/Ethnicity:',
                                       choices = race_choices,
                                       selected = race_choices
                    )
             ),
             hr(),
             
             fluidRow(
               column(3,
                      sliderInput(inputId = 'ON',
                                  label = 'Orthorexic Tendency',
                                  min = 1,
                                  max = 5,
                                  step = .1,
                                  value = c(1,5))),
               column(3,
                      sliderInput(inputId = 'SAC',
                                  label = 'Social Appearance Comparison',
                                  min = 1,
                                  max = 5,
                                  step = .1,
                                  value = c(1,5))),
               column(3,
                      sliderInput(inputId = 'TII',
                                  label = 'Thin-ideal Internalization',
                                  min = 1,
                                  max = 5,
                                  step = .1,
                                  value = c(1,5))),
               column(3,
                      sliderInput(inputId = 'BD',
                                  label = 'Body Dissatisfaction',
                                  min = 1,
                                  max = 5,
                                  step = .1,
                                  value = c(1,5)))
             ),
             
             fluidRow(
               column(3,
                      sliderInput(inputId = 'current_mood',
                                  label = 'Current Mood',
                                  min = 1,
                                  max = 5,
                                  step = .1,
                                  value = c(1,5))),
               column(3,
                      sliderInput(inputId = 'selfesteem',
                                  label = 'Self-Esteem',
                                  min = 1,
                                  max = 5,
                                  step = .1,
                                  value = c(1,5))),
               column(3,
                      sliderInput(inputId = 'exce_exercise',
                                  label = 'Excessive Exercise',
                                  min = 1,
                                  max = 5,
                                  step = .1,
                                  value = c(1,5))),
               column(3,
                      sliderInput(inputId = 'mother',
                                  label = 'Maternal Influence',
                                  min = 1,
                                  max = 5,
                                  step = .1,
                                  value = c(1,5)))
             ),
             
             fluidRow(
               column(4,
                      sliderInput(inputId = 'vlogexperience',
                                  label = 'Prior WIEIAD Experience',
                                  min = 0,
                                  max = 15,
                                  step = 1,
                                  value = c(0,15))),
               column(4,
                      sliderInput(inputId = 'recent_help',
                                  label = 'ED Treatment Recency',
                                  min = 0,
                                  max = 6,
                                  step = 1,
                                  value = c(0,6))),
               column(4,
                      sliderInput(inputId = 'Age',
                                  label = 'Age',
                                  min = 19,
                                  max = 25,
                                  step = 1,
                                  value = c(19,25)))
             ),
             
             hr(),
             
             fluidRow(
               column(12, DTOutput("table"))
             )
           ),

           p(HTML(
           '<p style="font-size:0.8em";">
           [NOTE]<br>
           *<i>Higher scores</i> on each factor indicate <i>greater intensity</i> of that characteristic.<br>
           *<i>Current Mood</i>: higher scores indicate stronger positive emotions.<br>
           *<i>Maternal Influence</i>: the extent to which the mother was involved verbally and behaviorally in dieting or weight management 
           during the respondent’s upbringing.<br>
           *<i>Prior WIEIAD Experiences</i>: the extent of prior experience with WIEIAD content such as viewing or creating them.<br>
           *<i>ED Treatment Recency</i>: how recently the respondant has received eating disorder treatment; higher scores indicate more recent treatment.
           </p>'
           ))),
# ------------------ TAB 3 ORTHOREXIA & BODY IMAGE ------------------
    tabPanel("Orthorexia & Body Image",
             
           p("Orthorexic tendencies are one type of problematic eating habit. They are often tie into a range of body image concerns. 
             Let's take a closer look at how each factor relates to orthorexic tendencies among young women."),   
             
           h3("Which Vlog Types are Linked to Higher Orthorexic Tendency?"),
           plotOutput("plot_group"),
           p(HTML("
           Looking at the average orthorexic tendency across different vlog types, we can see some interesting patterns. 
           Compared to people who watched non-food vlogs, those who watched diet-focused WIEIAD vlogs, 
           where meals are carefully controlled and only “healthy” foods are eaten, tended to show <b>higher</b> orthorexic tendencies.")),
           p(HTML("
           On the other hand, viewers of non-diet WIEIAD vlogs that showed people enjoy food freely without worrying about dieting or 
                  controlling their body shape showed <b>lower</b> orthorexic tendencies. 
                  Interestingly, the group encouraged to reflect on how so-called “unhealthy” foods like 
                  cookies, ice cream, or pizza could contribute positively to their overall well-being showed 
                  the <b>lowest</b> orthorexic tendencies on average! 
                  This suggests that a more flexible, holistic approach to food might help reduce extreme preoccupation with healthy eating.")),
           
           h3("How Thin-Ideal Internalization Relates to Orthorexic Tendency Across Vlog Types?"),
           plotOutput("plot_tii"),
           p("This graph shows how orthorexic tendency changes as people more strongly internalize the thin body ideal — 
           that is, when they not only recognize society’s preference for thinness but begin to adopt it as a personal value."),
           p("Overall, regardless of which type of vlog was watched, orthorexic tendency tended to increase as thin-ideal internalization became stronger.
             At the same time, the extent of orthorexic tendency still differed across vlog groups,
             suggesting that media exposure may shape how strongly this internalized ideal translates into eating-related attitudes and behaviors."),
           
           h3("How Social Appearance Comparison Relates to Orthorexic Tendency Across Vlog Types?"),
           plotOutput("plot_sac"),
           p("This graph shows how orthorexic tendency varies depending on how often and strongly individuals compare their appearance to others. 
             Similar to thin-ideal internalization, orthorexic tendency tended to increase as social appearance comparison became stronger 
             across all vlog types. Still, the actual orthorexic scores differed across vlog groups."),
           
           h3("How Body Dissatisfaction Relates to Orthorexic Tendency Across Vlog Types?"),
           plotOutput("plot_bd"),
           p("This time, the graph shows orthorexic tendency based on how dissatisfied individuals are with their own body. 
             Again, although general orthorexic scores were different according to vlogs types, 
             orthorexic tendency tended to increase as individuals were not happy with their body shapes regardless which vlog they watched."),
           
           h3("How Self-Esteem Relates to Orthorexic Tendency Across Vlog Types?"),
           plotOutput("plot_se"),
           p("This graph shows how orthorexic tendency varies according to overall self-esteem, not just appearance. 
             Individuals who treats themselves more kindly tended to be less preoccupied with “healthy” eating. 
             Of course, the absolute orthorexia scores still differed depending on which type of vlog they watched."),
           
           h3("How Excessive Exercise Relates to Orthorexic Tendency Across Vlog Types?"),
           plotOutput("plot_ex"),
           p("Lastly, this graph shows that individuals who engage in excessive exercise tend to have higher orthorexic tendencies. 
             In other words, those who exercise obsessively beyond what’s considered a proper amount may also be more preoccupied with eating “healthily.”
             Since diet and exercise are often seen as key components of health, this connection makes sense. 
             As before, the type of vlog watched still influences the absolute orthorexia scores.")
         ),

# ------------------ TAB 4 PREDICTION ------------------
    tabPanel("Orthorexia Prediction",
           h2("Predicting Orthorexic Tendency"),
           p("Now it's time for you to explore! You can pick the variables you're interested in and see how the orthorexia score might change."),
           
           p(HTML("Just to give you a quick idea of how the prediction works: I compared seveal regression-based models, including <b>Ridge, Lasso,
                  and ElasticNet</b>. Among them, <b>ElasticNet performed the best</b>, so I chose it as the final model (RSquared = 0.56, MAE = 0.28, RMSE = 0.38). 
                  ElasticNet combines the strengths of Ridge and Lasso methods. In simple terms, it examines all factors together 
                  while giving more weight to the most important ones and minimizing the influence of less relevant variables.
                  This helps produce more stable and acurate predictions.")),
           
           tags$div(
             style = "text-align: center;",
             tags$img(
               src = "images/runningfood.jpeg",
               width = "450px"
             )
           ),
           
           p(HTML("By examining with the variables, you can get a sense of <b>which factors tend to have a negative effect on orthorexcia score</b> and
             <b>how you might protect yourself from them</b>.")),
           
           p("Play around with the elements below and see how the orthorexia score changes in real time!"),
           
           p("*Race/Ethnicity is not included in this prediction tool because there weren't enough individuals in each race/ethnicity group to produce reliable results.
             To avoid misleading predictions, the model focuses on psychological, behavioral, and media-related factors."),
           
           h3("Prediction"),
           fluidRow(
             column(3,
                    selectInput("pred_group", "Vlog Type",
                                choices = unique(data_info$group),
                                selected = unique(data_info$group)[1])
             ),
             column(3,
                    selectInput("pred_help_received", "Help Received",
                                choices = c("Yes" = "yes", "No" = "no"),
                                selected = "no",
                                multiple = FALSE)
                    )
             ),
           
           fluidRow(
             column(2,
                    sliderInput("pred_SAC", "Social Appearance Comparison", 1, 5, 3, step = 0.1)
             ),
             column(2,
                    sliderInput("pred_TII", "Thin-ideal Internalization", 1, 5, 3, step = 0.1)
             ),
             column(2,
                    sliderInput("pred_BD", "Body Dissatisfaction", 1, 5, 3, step = 0.1)
             ),
             column(2,
                    sliderInput("pred_selfesteem", "Self-Esteem", 1, 5, 3, step = 0.1)
             ),
             column(2,
                    sliderInput("pred_exce_exercise", "Excessive Exercise", 1, 5, 3, step = 0.1)
             ),
             column(2,
                  sliderInput("pred_current_mood", "Current Mood", 1, 5, 3, step = 0.1)
             ),
             column(2,
                    sliderInput("pred_mother", "Maternal Influence", 1, 5, 3, step = 0.1)
             ),
             column(2,
                    sliderInput("pred_vlogexperience", "Prior WIEIAD Experience", 0, 15, 5, step = 1)
             ),
             column(2,
                    sliderInput("pred_recent_help", "ED Treatment Recency", 1, 6, 3.5, step = 0.1)
             ),
             column(2,
                    sliderInput("pred_Age", "Age", 19, 25, 22, step = 1)
             )
           ),
           fluidRow(
             column(2,
                    actionButton("predict", "Predict Orthorexia Score", width = "200%")
             )
           ),
           
           fluidRow(
             column(12,
                    h4("Predicted Orthorexic Tendency for the above condition is..."),
                    textOutput("on_prediction"),
                    style = "font-size: 24px; font-weight: bold"
             )
           )
    ),

# ------------------ TAB 5 WEIGHT INCLUSIVE ------------------
    tabPanel("Weight-Inclusive Responses",
           h2("Common Themes in Responses"),
           p("The data includes open-ended response from people who watched weight-inclusve videos and was instructed to write their personal opinion 
             about nonrestictive and weight-inclsuve eating habits for your overall health and well-being."),
           
           h3("Word Cloud"),
           p("Word cloud shows this and that"),
           
           h3("sentiment analysis"),
           p("sentiment shows positive or negative opinions")
         ),

# ------------------ TAB 6 CONCLUSION ------------------
    tabPanel("Conclusion",
           h2("Conclusion"),
           p("In conclusion, this project suggests that")
          ),

# ------------------ TAB 7 REFERENCES ------------------
    tabPanel("References",
             p(
               "Bonfanti, R. C., Melchiori, F., Teti, A., Albano, G., Raffard, S., Rodgers, R., & Lo Coco, G. (2025). 
    The association between social comparison in social media, body image concerns and eating disorder symptoms: A systematic review and meta-analysis. 
    Body Image, 52, 101841. ",
               tags$a(
                 href = "https://doi.org/10.1016/j.bodyim.2024.101841",
                 "https://doi.org/10.1016/j.bodyim.2024.101841",
                 target = "_blank"
               )
             ),
             
             p(
               "Horovitz, O., & Argyrides, M. (2023). 
    Orthorexia and Orthorexia Nervosa: A Comprehensive Examination of Prevalence, Risk Factors, Diagnosis, and Treatment.
    Nutrients, 15(17), 3851. ",
               tags$a(
                 href = "https://doi.org/10.3390/nu15173851",
                 "https://doi.org/10.3390/nu15173851",
                 target = "_blank"
               )
             ),
             
             tags$a(id = "returnUrl", href = "#")
             
    )
)


# load the prediction model
elastic_model_prediction <- readRDS("models/elastic_model.rds")
recipe_prep_prediction <- readRDS("models/recipe_prep.rds")

recipe_prep_prediction <- recipe_prep_prediction %>%
  update_role_requirements(role = "ID", bake = FALSE)
          
# Define server logic 
server <- function(input, output) {
  
  filtered_data_info <- reactive({
    data_info %>%
      rowwise %>%
      filter(group %in% input$vlog_type,
             help_received %in% input$help_received,
             any(str_detect(race_ethnicity, paste(input$race_ethnicity, collapse = "|"))),
             ON >= input$ON[1] & ON <= input$ON[2],
             current_mood >= input$current_mood[1] & current_mood <= input$current_mood[2],
             SAC >= input$SAC[1] & SAC <= input$SAC[2],
             TII >= input$TII[1] & TII <= input$TII[2],
             BD >= input$BD[1] & BD <= input$BD[2],
             selfesteem >= input$selfesteem[1] & selfesteem <= input$selfesteem[2],
             exce_exercise >= input$exce_exercise[1] & exce_exercise <= input$exce_exercise[2],
             mother >= input$mother[1] & mother <= input$mother[2],
             vlogexperience >= input$vlogexperience[1] & vlogexperience <= input$vlogexperience[2],
             recent_help >= input$recent_help[1] & recent_help <= input$recent_help[2],
             Age >= input$Age[1] & Age <= input$Age[2]
      )
   })
  
  output$table <- renderDT({
    datatable(filtered_data_info(),
              options = list(pageLength = 10))
  })


  # ---- Plots ----
  output$plot_group <- renderPlot({
    ggplot(data_info, aes(x = group, y = ON, fill = group)) +
      stat_summary(fun = mean, geom = "bar", alpha = 0.8) +
      stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)),
                   vjust = -0.3, size = 4) +
      scale_fill_viridis_d(option = "viridis", alpha = 0.8) +
      coord_cartesian(ylim = c(2, 3.5)) +
      theme_minimal() +
      labs(
        title = "Average Orthorexic Score by Vlog Type",
        x = "Vlog Type Watched",
        y = "Orthorexic Score",
        fill = "Vlog Type Watched"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text.x = element_text(hjust = 1, size = 10, angle = 20),
        axis.title = element_text(size = 14)
      )
  })
  
  output$plot_tii <- renderPlot({
    ggplot(data_info, aes(x = TII, y = ON, color = group)) +
      geom_jitter(width = 0.1, height = 0.1, size = 3, alpha = 0.5) +
      geom_smooth(method = "loess", se = FALSE, size = 1.3) +
      scale_color_viridis_d(option = "viridis") +
      theme_minimal() +
      labs(
        title = "Orthorexic Tendency by\nThin-Ideal Internalization and Vlog Type",
        x = "Thin-Ideal Internalization",
        y = "Orthorexic Score",
        color = "Vlog Type Watched"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14)
      )
  })
  
  output$plot_sac <- renderPlot({
    ggplot(data_info, aes(x = SAC, y = ON, color = group)) +
      geom_jitter(width = 0.1, height = 0.1, size = 3, alpha = 0.5) +
      geom_smooth(method = "loess", se = FALSE, size = 1.3) +
      scale_color_viridis_d(option = "viridis") +
      theme_minimal() +
      labs(
        title = "Orthorexic Tendency by\nSocial Comparison and Vlog Type",
        x = "Social Apperance Comparison",
        y = "Orthorexic Score",
        color = "Vlog Type Watched"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14)
      )
  })
  
  output$plot_bd <- renderPlot({
    ggplot(data_info, aes(x = BD, y = ON, color = group)) +
      geom_jitter(width = 0.1, height = 0.1, size = 3, alpha = 0.5) +
      geom_smooth(method = "loess", se = FALSE, size = 1.3) +
      scale_color_viridis_d(option = "viridis") +
      theme_minimal() +
      labs(
        title = "Orthorexic Tendency by\nBody Dissatisfaction and Vlog Type",
        x = "Body Dissatisfaction",
        y = "Orthorexic Score",
        color = "Vlog Type Watched"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14)
      )
  })
  
  output$plot_se <- renderPlot({
    ggplot(data_info, aes(x = selfesteem, y = ON, color = group)) +
      geom_jitter(width = 0.1, height = 0.1, size = 3, alpha = 0.5) +
      geom_smooth(method = "loess", se = FALSE, size = 1.3) +
      scale_color_viridis_d(option = "viridis") +
      theme_minimal() +
      labs(
        title = "Orthorexic Tendency by\nSelf-Esteem and Vlog Type",
        x = "Self-Esteem",
        y = "Orthorexic Tendency",
        color = "Vlog Type Watched"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14)
      )
  })
  
  output$plot_ex <- renderPlot({
    ggplot(data_info, aes(x = exce_exercise, y = ON, color = group)) +
      geom_jitter(width = 0.1, height = 0.1, size = 3, alpha = 0.5) +
      geom_smooth(method = "loess", se = FALSE, size = 1.3) +
      scale_color_viridis_d(option = "viridis") +
      theme_minimal() +
      labs(
        title = "Orthorexic Tendency by\nExcessive Exercise and Vlog Type",
        x = "Excessive Exercise",
        y = "Orthorexic Tendency",
        color = "Vlog Type Watched"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14)
      )
  })
  
# ---- Prediction ----
  
  observeEvent(input$predict, {
    
    new_data <- tibble(
      id = 0,
      group = factor(input$pred_group, levels = c("non-food vlog", "diet WIEIAD vlog", "non-diet WIEIAID vlog", "non-diet WIEIAD vlog with prompts")),
      SAC = as.numeric(input$pred_SAC),
      TII = as.numeric(input$pred_TII),
      BD = as.numeric(input$pred_BD),
      exce_exercise = as.numeric(input$pred_exce_exercise),
      mother = as.numeric(input$pred_mother),
      current_mood = as.numeric(input$pred_current_mood),
      vlogexperience = as.numeric(input$pred_vlogexperience),
      Age = as.numeric(input$pred_Age),
      selfesteem = as.numeric(input$pred_selfesteem),
      help_received = as.numeric(ifelse(length(input$pred_help_received) == 1 && input$pred_help_received == "yes", 1, 0)),
      recent_help = as.numeric(input$pred_recent_help)
    )
    
    model_features <- rownames(coef(elastic_model_prediction$finalModel, elastic_model_prediction$bestTune$lambda))
    model_features <- setdiff(model_features, c("(Intercept)")) 
    
    new_data_processed <- bake(recipe_prep_prediction, new_data)
    
    new_data_matrix <- do.call(cbind, lapply(new_data_processed, unname))
    colnames(new_data_matrix) <- colnames(new_data_processed)
    for(f in model_features){
      if(!f %in% colnames(new_data_matrix)){
        new_data_matrix <- cbind(new_data_matrix, setNames(matrix(0, nrow = nrow(new_data_matrix), ncol = 1), f))
      }
    }
    
    new_data_matrix <- new_data_matrix[, model_features, drop = FALSE]
    
    pred <- predict(elastic_model_prediction$finalModel, newx = new_data_matrix, s = elastic_model_prediction$bestTune$lambda)
    pred_val <- as.numeric(pred)
    
    output$on_prediction <- renderText({
      paste0("", round(pred_val, 2))
    })
    
  })
  
}

# Run the application 
shinyApp(ui, server)
