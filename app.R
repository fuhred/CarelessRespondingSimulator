# Setup ----
library(colourpicker)
library(colorspace)
library(ragg)
library(shiny)
library(MASS)
library(dplyr)
library(ggplot2)
options(shiny.useragg = TRUE) #turns on ragg for nice outputs


# Custom functions ----

#outputs numbers as characters with 2 digit rounding and no leading zero
numformat <- function(x, digits = 2) { 
  ncode <- paste0("%.", digits, "f")
  sub("^(-?)0.", "\\1.", sprintf(ncode, x))
}

p_stars=
  function(x){
    case_when(
      x <= .001 ~ "***",
      x <= .01  ~ "**",
      x < .05  ~ "*",
      TRUE     ~ ""
    )
  }


# UI ----

ui <- fluidPage(
  
  br(),
    
# Main panel: outputs ----
  mainPanel(
    width=9,
    
    
    

## Row 1 --

  fluidRow(
    ## Plot Title Text ----   
    column(5,
           p(HTML("<b>Given the simulation parameters,</b>"),
              style="line-height:0; color: #333333; font-size:16px; font-weight:700; margin-top: 10px;"
           ),
           
           h1(HTML(
             "<b>Inattentive responders change the correlation by </b>"), 
             textOutput("myText3_withplus", inline = TRUE) %>% strong()) %>%  
             span(
               style="letter-spacing:-.05em; margin-bottom: 40px; !font-size:14px;"
             )           
           
           ),
    column(4),
    column(3),
  ),
    
    
## Layout Setup ----    
  fluidRow(
    
    column(5,
           plotOutput("myPlot", click = "plot_click", width="100%")
           ),
    column(4,
           plotOutput("myPlot2", width="250px", height="225px"),
           tableOutput("df_results")
           ),
    column(3,
    ),
  ),
    

    ## CSS Tags ----
    tags$head(tags$style("#text1{color: gray}
                          #text2{color: gray}
                          #text3{color: gray}
                         .well{
                            background-color:hsla(0, 0%, 97%, 1);
                            border-width: 0px;
                         }
                         .irs--shiny .irs-bar {
                            border-top: 1px solid #c3c3c3;
                            border-bottom: 1px solid #c3c3c3;
                            background: #c3c3c3;
                         }
                        .irs--shiny .irs-from,
                        .irs--shiny .irs-to,
                        .irs--shiny .irs-single {
                          background-color: #c3c3c3;
                        }
                        hr{
                          margin-top:10px;
                        }
                        .h1, h1{
                          font-size:28px;
                        }
                        label{
                          color: gray;
                          font-weight: 500;
                        }
                        #CreatedEffect  {font-size:80%}
                        #DilutedEffect  {font-size:80%}
                        #InflatedEffect {font-size:80%}
                        .btn-default {
                            background-color: #FBFBFB;
                            color: gray;
                        }
                        li{
                          margin-bottom:10px;
                          color: #78909C;
                        }
                         ")),    

## Project Description ----
  
  br(),
  br(),
  hr(),

  #testing - delme
  # p("df_ir_nrow = ", textOutput("df_ir_nrow", inline=T)),
  # p("ncol = ", textOutput("temp.cor.ncol", inline=T)),
  # p("size = ", textOutput("size", inline=T)),
  # tableOutput("df_r"),

  p(HTML("<b>Project Description and Notes</b>"), style="color:#566573"),

tags$ul(
  tags$li(HTML("This is a demonstration of how inattentive responders can change the correlations found in true responders, and a companion piece to the article <i>Inattentive Responding: Why many findings are spurious or spuriously inflated</i>. Full code can be found at: X.")), 
  tags$li(HTML("<b>True Responders note:</b> you will find that your inputted correlation (rho) will not always match the found correlation (<i>r</i>) in the Table of Correlations. As you increase N, the <i>r</i> will more closely resemble rho.")), 
  tags$li(HTML("<b>Inattentive Responders note</b>")),
    tags$ul(
      tags$li("To our knowledge, there is no empirical data on the underlying response distributions for Inattentive Responders (see DiSimone et al., 2018). Here, we simulate them using uniform random distributions."),
      # tags$li(HTML(" 100% randomness may only apply to bots and people who are paying no attention at all. Many participants may only be partly randomly responding. Please adjust the <strong><i>% Extent Random</i></strong> to simulate partial random responding. The available values for % Extent Random correspond to the total number of items in scales X and Y. Random responding is randomly distributed among X and Y items.")),
      tags$li(HTML("This ShinyApp does not simulate straightlining.")),      
    ),
  tags$li(HTML("At extremely low sample sizes, the Pearson's correlation coefficients are unreliable."))
)


  ), #main panel closing bracket
   
# Sidebar: inputs ----
  sidebarPanel(
    
    h5(HTML("<b>Inattentive Responding Simulator</b>")) %>% span(style="color:#333333; font-weight: 700;"),
    hr(),
    
    
    fluidRow(
      column(6,
             numericInput(
               "n",
               "Sample N",
               value=400, #initial value
               min=50,
               max=1000,
               step=1,
             ))
    ),
    
    hr(),
    
    ## Effect Presets ----
    
    
    span(p("Effect Presets"),
         style="color:#333333; font-weight: 700; font-size: 85%"),
    
    fluidRow(
      column(4,
             actionButton(inputId="DilutedEffect", 
                          label=HTML("Diluted</br>Effect"),width="100%"),
      ),

      column(4,
             actionButton(inputId="InflatedEffect", 
                          label=HTML("Inflated</br>Effect"),width="100%"),
      ),
      
      column(4,
             actionButton(inputId="CreatedEffect", 
                          label=HTML("Created</br>Effect"),width="100%"),
      ),
      
    ),
    

    
    ## True Responders Parameters ----
    
    hr(),

    span(p("True Responders"),
         style="color:#333333; font-weight: 700; font-size: 85%"),    
    span(p("Population parameters for simulation."),
         style="color:#666666; font-size: 85%; "),    
    sliderInput(
      "true_corr",
      "Correlation (rho)",
      min=-1,
      max=1,
      step = .01,
      ticks= F,
      value=.0, #initial value
      
    ),     
    
    fluidRow(
      column(6,
             numericInput(
               "true_xmean",
               "Center of X (mu)",
               value=2.0, #initial value
               min=1,
               max=7,
               step = .1
             )
      ),  
      
      column(6,    
             numericInput(
               "true_ymean",
               "Center of Y (mu)",
               value= 2.5, #initial value
               min=1,
               max=7,
               step = .1
             )
      )  
    ),
    
    span(p("You can also click on the plot to set the population means."),
         style="color:gray; font-size: 70%"),
    
    
    hr(),
    
    ## Inattentive Responders Parameters ----
    
    span(p("Inattentive Responders (IR)"),
         style="color:#333333; font-weight: 700; font-size: 85%"),    
    
    ### Percentanges ----    
    fluidRow(
      
      column(6,
             numericInput(
               "IR_pct",
               "% of Sample N",
               value = 15, #initial value
               min = 0,
               max = 100,
               step = 1
             )),
      
      column(6,

               # numericInput(
               #   "IR_random_pct",
               #   "% Extent Random",
               #   value = 100, #initial value
               #   min = 0,
               #   max = 100,
               #   step = 5,
               # )
             
             ),             
      
    ),
    
    ### No. of Items ----
    fluidRow(
      
      column(6,
             numericInput(
               "x_items",
               "X scale: # of items",
               value=10, #initial value
               min=1,
               max=25,
               step = 1)
             ),
      
      column(6,
             numericInput(
               "y_items",
               "Y scale: # of items",
               value=10, #initial value
               min=1,
               max=25,
               step = 1
               )
      )),
    
    
    
        
    ## Aesthetics ----
    
    hr(),
    
    span(p("Aesthetics"),
         style="color:#333333; font-weight: 700; font-size: 85%"),    

    ### Colors ----
    
    fluidRow(
      column(6,
             colourInput("col_true", "True Responders",
                         value = "#B3BDBC",
                         showColour = c("text")
                         )),
      column(6,
             colourInput("col_ir", "IR", 
                         value = "#AB63DB",
                         showColour = c("text")
                         ))
    ),
    
    ### Shaded Region ----
    radioButtons(
      "shaded_region",
      label="Shaded Region (SE)",
      choices=c("Off"=FALSE, "On"=TRUE)
    ),
    
    ### Fullrange ----
    radioButtons(
      "fullrange",
      label="Full Range of Regression Line",
      choices=c("Off"=FALSE, "On"=TRUE),
      selected=TRUE
    ),    

    ### Jitter ----
    
    numericInput(
      "jitter",
      "Points Jitter",
      value=0, #initial value
      min=0,
      max=.25,
      step = .05
    ),    
    
    width=3
  )
  
  
  
  
)

# Server ----

server <- function(input, output, session) {
  
# Vars to Reactive ----
true_corr      = reactive({input$true_corr})
true_xmean     = reactive({input$true_xmean})
true_ymean     = reactive({input$true_ymean})
IR_prop        = reactive({input$IR_pct/100})
# IR_random_prop = reactive({input$IR_random_pct/100}) #uncomment
IR_random_prop = 1

sd=1

# dataframe setups ----

## True Responders Dataframe ----
df_tr=
  reactive({
    as.data.frame(mvrnorm(n=round(input$n*(1-IR_prop()),0), 
                          mu=c(true_xmean(), true_ymean()), 
                          Sigma=
                            rbind(
                              c(         sd, true_corr()),
                              c(true_corr(), sd)
                            )
                            )) %>% 
    mutate(IR="True Responders")
  })

## IR Dataframe ----

#how many IR cases? rounded to an integer
df_ir_nrow=reactive({round(input$n*(IR_prop()),0)}) 

df_ir=reactive({
  
  total_items_n=input$x_items+input$y_items
  
  #IR: make a dataframe of random responses
  temp.random=
  as.data.frame(matrix(sample(1:7, #randomly generate - uniform distribution
                              (total_items_n * df_ir_nrow() * IR_random_prop), #number of cells
                              replace = T), 
                        ncol=total_items_n * IR_random_prop, 
                        nrow=df_ir_nrow()))
  
  #IR: make a dataframe of true responses
  temp.cor=
  as.data.frame(matrix(sample(c(1), #correlations
                              size=total_items_n * df_ir_nrow() * round(1-IR_random_prop,5), #n cells. round bc floating point error
                              replace = T), 
                       ncol=(total_items_n) * round(1-IR_random_prop,5), 
                       nrow=df_ir_nrow()))    
  
  #make a dataframe of random + true responses
  temp=bind_cols(temp.random, temp.cor)
  
  #make the names
  names(temp)=c(paste0("x", 1:input$x_items),
                paste0("y", 1:input$y_items))
  

  #compute the mean score vars
  if(length(grep("x", names(temp)))>1) {
    temp$V1=rowMeans(temp[,grepl("x",names(temp))])
  } else {temp$V1=temp$x1}
  if(length(grep("y", names(temp)))>1) {
    temp$V2=rowMeans(temp[,grepl("y",names(temp))])
  } else {temp$V2=temp$y1}

  temp$IR="Inattentive Responders"
  
  # real code
  temp %>% dplyr::select(V1,V2,IR) %>% as_tibble()
  
  # for testing only
  # temp %>% as_tibble()
  
  # bind_cols(temp.cor)
  
  })


#for testing only
  #IR: true responses table
  output$df_r=renderTable(df_ir() %>% 
                            # filter(IR=="Inattentive Responders") %>% 
                            head())
  
  output$df_ir_nrow=renderText({
    df_ir_nrow()
  })
  
  #IR: ncol
  output$temp.cor.ncol=renderText({
    (input$x_items+input$y_items) * (1-IR_random_prop())
    })
  
  output$size=renderText({
    round((input$x_items+input$y_items) * df_ir_nrow() * (1-IR_random_prop()))  
    })


## Full Dataframe ----
df_r=
  reactive({
    bind_rows(df_tr(), 
              df_ir()) %>% 
    mutate(IR=factor(IR, levels=c("True Responders", "Inattentive Responders"))) 
  })



# Update slider based on plot clicking  ----

observeEvent(input$plot_click$x,  {
    updateSliderInput(session, "true_xmean", value = round(input$plot_click$x,digits=1))
})

observeEvent(input$plot_click$y,  {
    updateSliderInput(session, "true_ymean", value = round(input$plot_click$y,digits=1))
})

# Update parameters based on Effect butons ----

observeEvent(input$DilutedEffect,  {
  updateSliderInput(session, "true_corr",  value = .8)
  updateSliderInput(session, "true_xmean", value = 5)
  updateSliderInput(session, "true_ymean", value = 3)
})

observeEvent(input$InflatedEffect,  {
  updateSliderInput(session, "true_corr",  value = .4)
  updateSliderInput(session, "true_xmean", value = 2.5)
  updateSliderInput(session, "true_ymean", value = 2)
})

observeEvent(input$CreatedEffect,  {
  updateSliderInput(session, "true_corr",  value = 0)
  updateSliderInput(session, "true_xmean", value = 2)
  updateSliderInput(session, "true_ymean", value = 2.5)
})

#Update % Extent Random based on Total # of items ----

observeEvent(input$x_items,  {
  updateSliderInput(session, "IR_random_pct",  step = 100/(input$x_items + input$y_items))
})

observeEvent(input$y_items,  {
  updateSliderInput(session, "IR_random_pct",  step = 100/(input$x_items + input$y_items))
})

# Correlations ----

cor1=
  reactive({
    with(df_r() %>% filter(IR=="True Responders"),
         cor.test(V2,V1))
  })

cor2=
  reactive({
    with(df_r(),
         cor.test(V2,V1))
  })


myText1=
  reactive({
    paste0(
      if(cor1()$estimate>0) 
        "",
        numformat((cor1()$estimate))
        )
  })  

myText2=
  reactive({
    paste0(
      if(cor2()$estimate>0) 
        "",
      numformat((cor2()$estimate))
    )
  })  

myText3=
  reactive({
    paste0(
      if(
        (cor1()$estimate-cor2()$estimate)<0) "",
      numformat((cor1()$estimate-cor2()$estimate)*-1))
  })    


myText3_withplus=
  reactive({
    paste0(
           if(
             (cor1()$estimate-cor2()$estimate)<0) "+",
           numformat((cor1()$estimate-cor2()$estimate)*-1))
  })    


output$myText3_withplus=
  renderText({
    myText3_withplus()
  })


# Table output  ----

df_results=
  reactive({
    tibble(
      # `  ` = c("---","---",NA), #can i put lines here?
      ` ` = c("True Responders", "True + Inattentive Responders", "Correlation Change"),
      "r" = c(myText1(), myText2(), myText3()),
      p = c(numformat(cor1()$p.value,3), numformat(cor2()$p.value,3), NA)
    )
  })

output$df_results=
  renderTable(
    df_results(),
    na="",
    rownames=F,
    align="lrr"
  )
# Plot 1 ----

output$myPlot =
  renderPlot({
    df_r() %>% 
      ggplot(aes(x=V1, y=V2)) +
      geom_point(aes(color=IR, size=IR, shape=IR),
                 position = position_jitter(width=input$jitter,height=input$jitter)
                 ) +
      
      geom_smooth(data=. %>% filter(IR=="True Responders"),
                  method='lm',
                  se=as.logical(input$shaded_region),
                  color=input$col_true,
                  fill=input$col_true,
                  fullrange=as.logical(input$fullrange),
                  linetype="dashed",
                  size=.8) +
      
      geom_smooth(color=input$col_ir, #True + Inattentive Responders
                  method='lm',
                  se=as.logical(input$shaded_region), 
                  fullrange=as.logical(input$fullrange),
                  fill=input$col_ir) +

      scale_x_continuous(breaks=c(1:7), limits=c(1,7)) +
      scale_y_continuous(breaks=c(1:7), limits=c(1,7)) +
      coord_cartesian(xlim=c(1,7), ylim=c(1,7)) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            panel.border = element_rect(color="black"),
            axis.ticks = element_line(color="black"),
            axis.text = element_text(color="black"),
            legend.title = element_blank(),
            legend.position = "none"
      ) +

      scale_color_manual(values=c(input$col_true, input$col_ir)) +
      scale_shape_manual(values=c(19, 17)) +
      scale_size_manual(values=c(.7, 1.25)) +
      labs(x="X", y="Y")
    
  },
  scaling=1.5
  )

line_length=.05

# Plot 2 (legend) ----
output$myPlot2 =
  renderPlot({
    
    tibble(
      x=c(1,1,1,1),
      y=c(1,2,3,4),
      label=c("True Responders", "Inattentive Responders", "True Responders", "True + Inattentive Responders"),
      points=c(1,1,0,0),
      colors=c(T,F,T,F)) %>% 
      ggplot(aes(x=x, y=rev(y))) +
      geom_point(aes(color=colors, shape=colors, alpha=points)) +
      geom_text(aes(label=label), hjust=0, nudge_x=.05, size=4) +
      geom_segment(x=1 - (line_length/2), xend=1 + (line_length/2), y=2, yend=2, linetype="dashed",
                   color=input$col_true) +  
      geom_segment(x=1 - (line_length/2), xend=1 + (line_length/2), y=1, yend=1,
                   color=input$col_ir) +
      scale_x_continuous(limits=c(1,2)) +
      scale_y_continuous(limits=c(-2,8)) +
      scale_shape_manual(values=c(17, 19, 1, 1)) +
      coord_cartesian(xlim=c(.95, 2), ylim=c(-2,8), expand=F)+
      theme_void() +
      theme(legend.position ="none") +
      scale_color_manual(values=c(input$col_ir, input$col_true)) 
    
  }
  )


} # end bracket for server function


  

shinyApp(ui = ui, server = server)



