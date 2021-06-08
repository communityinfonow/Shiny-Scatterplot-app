load('thc.Rdata')


library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinythemes)

# Creating vectors to call on in plotly. This allows the axis labels to update as the user changes to their choices.
choiceVecx <- c("Cases / 10k" = "case_rate",
                "Hospitalizations / 10k" = "hosp_rate",
                "Deaths / 100k" = "mort_rate",
                "Total Doses Administered / 10k" = "total_dose_rate",
                "Single Dose / 10k" = "one_dose_rate",
                "Fully Vaccinated / 10k" = "full_vax_rate")

choiceVecy <- c("American Indian or Alaska Native %" = "popAmIndian_percent",
                "Asian %" = "popAsian_percent",
                "Black %" = "popBlack_percent",
                "Hispanic or Latino %" = "popHispanic_percent",
                "Native Hawaiian or Pacific Islander %" = "popNative_percent",
                "White %" = "popWhite_percent",
                "Other Race %" = "popOther_percent",
                "People of Color %" = "poc_percent",
                "Two or More Races, not hispanic %" = "poptwoormore_percent",
                "Population 65 and older %" = "pop65orOver_percent",
                "Asian, 65 or older %" = "popAsian65up_percent",
                "Black, 65 or older %" = "popBlack65up_percent",
                "Hispanic, 65 or older %" = "popHispanic65up_percent",
                "White, 65 or older %" = "popNHWhite65up_percent",
                "Senior Dependency %" = "seniorDepend_percent",
                "Under 5 %" = "popUnder5_percent",
                "Child Dependency %" = "childDepend_percent",
                "Less Than High School %" = "pop25LessHs_percent",
                "Associates or Higher %" = "pop25AssocOrHigher_percent",
                "Asian, 25 years or older with less than high school %" = "pop25LessHsAsian_percent",
                "Black, 25 years or older with less than high school %" = "pop25LessHsBlack_percent",
                "Hispanic, 25 years or older with less than high school %" = "pop25LessHsHispanic_percent",
                "White, 25 years or older with less than high school %" = "pop25LessHsWhite_percent",
                "Civilian Unemployed population 16 and over in the Labor Force, Asian %" = "popAsianUnemployed_percent",
                "Civilian Unemployed population 16 and over in the Labor Force, Black %" = "popBlackUnemployed_percent",
                "Civilian Unemployed population 16 and over in the Labor Force, Hispanic %" = "popHispanicUnemp_percent",
                "Civilian Unemployed population 16 and over in the Labor Force, White %" = "popNHWhiteUnemployed_percent",
                "Female unemployment claims / 10k" = "f_unemp_rate",
                "Male unemployment claims / 10k" = "m_unemp_rate",
                "Unemployment claims / 10k" = "unemp_rate",
                "In Poverty %" = "popInPov_percent",
                "Asian Population in Poverty %" = "povAsian_percent2",
                "Black Population in Poverty %" = "povBlack_percent2",
                "Hispanic Population in Poverty %" = "povHispanic_percent2",
                "White Population in Poverty %" = "povNHWhite_percent2",
                "In Poverty 18 to 64 %" = "pov18to64_percent2",
                "In Poverty 65 and Over %" = "pov65up_percent2",
                "In Poverty working full time, year round %" = "povFullTime_percent",
                "In Poverty working full time, year round (Percent of Full Time) %" = "povFullTime_percent2",
                "In Poverty with Disability %" = "povWithDisability_percent2",
                "Families with income in the past 12 months below poverty level %" = "familiesBelowPov_percent",
                "Income in the past 12 months below poverty level for Bachelor's degree or higher %" = "povBachelorOrMore_percent",
                "Income in the past 12 months below poverty level for Less than high school graduate %" = "povLessThanHs_percent",
                "Population by ratio of income below 150% of poverty level %" = "ratioPopBelow150Pov_percent",
                "Population by ratio of income below 200% of poverty level %" = "ratioPopBelow200Pov_percent",
                "Insured Population in Poverty %" = "insPovOver100_percent",
                "Uninsured %" = "uninsured_percent",
                "Insured Population with Public Coverage %" = "insPublic_percent",
                "Insured Population with Private Coverage %" = "insPrivate_percent",
                "Uninsured, Under 18 (Percent of U18) %" = "uninsuredUnder18_percent",
                "Uninsured, 19 to 34 %" = "pop19to34Uninsured_percent",
                "Uninsured, 65 and Older (Percent of 65 and Older) %" = "uninsured65andOlder_percent",
                "Asian population without health insurance coverage %" = "popAsianUnins_percent",
                "Black population without health insurance coverage %" = "popBlackUnins_percent",
                "Hispanic or Latino population without health insurance coverage %" = "popHispUnins_percent",
                "White population without health insurance coverage %" = "popNHWhiteUnins_percent",
                "Two or more races population without health insurance coverage %" = "popTwoMoreUnins_percent",
                "Unemployed Population with Insurance %" = "insUnemployed_percent",
                "Owner Units Paying 30% or more of Housing Costs %" = "OwnerOc30p_percent",
                "Renter Units Paying 30% or more of Housing Costs %" = "renterOc30p_percent",
                "Housing Units Paying 30% or more of Housing Costs, Age 65 and Older %" = "hBurden30p65Older_percent",
                "Owner Units Paying 30% or more of Housing Costs, Age 65 and Older %" = "OwnerOc30p65Older_percent",
                "Renter Units Paying 30% or more of Housing Costs, Age 65 and Older %" = "renterOc30p65Older_percent",
                "Paying 30% or more of Housing Costs, Householder 35 to 64 years %" = "hBurden30Pct35to64_percent",
                "Paying 30% or more of Housing Costs, Householder 65 or Over %" = "hBurden30Pct65Older_percent",
                "Household received Food Stamps/SNAP in the past 12 months %" = "assistSNAP_percent",
                "Household received Food Stamps/SNAP in the past 12 months with Asian Householder %" = "popSNAPAsian_percent",
                "Household received Food Stamps/SNAP in the past 12 months with Black Householder %" = "popSNAPBlack_percent",
                "Household received Food Stamps/SNAP in the past 12 months with Hispanic Householder %" = "popSNAPHispanic_percent",
                "Household received Food Stamps/SNAP in the past 12 months with Non-Hispanic White Householder %" = "popSNAPNHWhite_percent",
                "With SSI and/or cash public assistance income in the past 12 months %" = "assistSSI_percent",
                "Change in Firms % (Mar - Nov 2020)" = "Firms_pctdiff",
                "Change in Employees % (Mar - Nov 2020)" = "Employees_pctdiff",
                "Change in Sales % (Mar - Nov 2020)" = "Sales_pctdiff",
                "Change in Small Firms % (Mar - Nov 2020)" = "SmallFirms_pctdiff",
                "Change in Small Firms Sales % (Mar - Nov 2020)" = "SmallFirmSales_pctdiff",
                "Change in Female-Owned Firms % (Mar - Nov 2020)" = "FemaleOwned_pctdiff",
                "Total population with a disability %" = "disability_percent",
                "Total population ages 18 to 34 with a disability %" = "dis18to34_percent",
                "Total population ages 18 to 64 with a disability %" = "dis18to64_percent",
                "Total population ages 65 to 74 with a disability %" = "dis65to74_percent",
                "Total population 75 or older with a disability %" = "dis75AndOlder_percent",
                "Household of Male or Female householder With related children of the householder under 18 years %" = "totalHuSingleParent_percent",
                "Households with one or more people 65 years and over with one person in the household %" = "hu65Alone_percent",
                "Owner Housing Householder 65 and Older %" = "ownerOccupied65Older_percent",
                "Renter Housing Householder 65 and Older %" = "renterOccupied65up_percent",
                "Population in group quarters %" = "groupQuart_percent",
                "Carpool or Use Public Transit %" = "carpoolOrPublic_percent",
                "Average Commute Time (in minutes) %" = "aggTravel_percent",
                "No Vehicles Available in Household, Householder 65 and up %" = "nocar65up_percent",
                "Computer and Broadband in Household %" = "broadbandComp_percent")

# page set up
ui <- fluidPage(
  theme = shinytheme("yeti"),
  tags$br(), #add line break
  tags$h3("COVID-19 ZIP Code Correlations", align = "center"), #Centering title
  tags$br(),
  
  # Sidebar with 3 select inputs for filtering and plotting
  sidebarLayout(
    sidebarPanel(
      p("Choose two variables to explore relationships"),
      p(), # p() can also work as lines breaks
      p(),
      p(),
      # selectizeInput is the dropdown object used with the plotly function, 'xCol' and 'yCol' are the dropdown menu objects, you can create vectors to group your choices
      selectizeInput('xCol', 'Choose a COVID-19 or Vaccine Indicator*', 
                     choices = list(
                       COVID = c("Cases / 10k" = "case_rate",
                                 "Hospitalizations / 10k" = "hosp_rate",
                                 "Deaths / 100k" = "mort_rate"),
                       Vaccination = c("Total Doses Administered / 10k" = "total_dose_rate",
                                       "Single Dose / 10k" = "one_dose_rate",
                                       "Fully Vaccinated / 10k" = "full_vax_rate")
                                   )
                    ),
      selectizeInput('yCol', 'Choose a Community Indicator', 
                     choices = list(
                       "Race and Ethnicity" = c("American Indian or Alaska Native %" = "popAmIndian_percent",
                                              "Asian %" = "popAsian_percent",
                                              "Black %" = "popBlack_percent",
                                              "Hispanic or Latino %" = "popHispanic_percent",
                                              "Native Hawaiian or Pacific Islander %" = "popNative_percent",
                                              "White %" = "popWhite_percent",
                                              "Other Race %" = "popOther_percent",
                                              "People of Color %" = "poc_percent",
                                              "Two or More Races, not hispanic %" = "poptwoormore_percent"),
                       "Dependent Populations" = c("Population 65 and older %" = "pop65orOver_percent",
                                                   "Asian, 65 or older %" = "popAsian65up_percent",
                                                   "Black, 65 or older %" = "popBlack65up_percent",
                                                   "Hispanic, 65 or older %" = "popHispanic65up_percent",
                                                   "White, 65 or older %" = "popNHWhite65up_percent",
                                                   "Senior Dependency %" = "seniorDepend_percent",
                                                   "Under 5 %" = "popUnder5_percent",
                                                   "Child Dependency %" = "childDepend_percent"),
                       Education = c("Less Than High School %" = "pop25LessHs_percent",
                                     "Associates or Higher %" = "pop25AssocOrHigher_percent",
                                     "Asian, 25 years or older with less than high school %" = "pop25LessHsAsian_percent",
                                     "Black, 25 years or older with less than high school %" = "pop25LessHsBlack_percent",
                                     "Hispanic, 25 years or older with less than high school %" = "pop25LessHsHispanic_percent",
                                     "White, 25 years or older with less than high school %" = "pop25LessHsWhite_percent"),
                       Unemployment = c("Civilian Unemployed population 16 and over in the Labor Force, Asian %" = "popAsianUnemployed_percent",
                                        "Civilian Unemployed population 16 and over in the Labor Force, Black %" = "popBlackUnemployed_percent",
                                        "Civilian Unemployed population 16 and over in the Labor Force, Hispanic %" = "popHispanicUnemp_percent",
                                        "Civilian Unemployed population 16 and over in the Labor Force, White %" = "popNHWhiteUnemployed_percent",
                                        "Female unemployment claims / 10k" = "f_unemp_rate",
                                        "Male unemployment claims / 10k" = "m_unemp_rate",
                                        "Unemployment claims / 10k" = "unemp_rate"),
                       Poverty = c("In Poverty %" = "popInPov_percent",
                                   "Asian Population in Poverty %" = "povAsian_percent2",
                                   "Black Population in Poverty %" = "povBlack_percent2",
                                   "Hispanic Population in Poverty %" = "povHispanic_percent2",
                                   "White Population in Poverty %" = "povNHWhite_percent2",
                                   "In Poverty 18 to 64 %" = "pov18to64_percent2",
                                   "In Poverty 65 and Over %" = "pov65up_percent2",
                                   "In Poverty working full time, year round %" = "povFullTime_percent",
                                   "In Poverty working full time, year round (Percent of Full Time) %" = "povFullTime_percent2",
                                   "In Poverty with Disability %" = "povWithDisability_percent2",
                                   "Families with income in the past 12 months below poverty level %" = "familiesBelowPov_percent",
                                   "Income in the past 12 months below poverty level for Bachelor's degree or higher %" = "povBachelorOrMore_percent",
                                   "Income in the past 12 months below poverty level for Less than high school graduate %" = "povLessThanHs_percent",
                                   "Population by ratio of income below 150% of poverty level %" = "ratioPopBelow150Pov_percent",
                                   "Population by ratio of income below 200% of poverty level %" = "ratioPopBelow200Pov_percent",
                                   "Insured Population in Poverty %" = "insPovOver100_percent"),
                       "Health Insurance Coverage" = c("Uninsured %" = "uninsured_percent",
                                                       "Insured Population with Public Coverage %" = "insPublic_percent",
                                                       "Insured Population with Private Coverage %" = "insPrivate_percent",
                                                       "Uninsured, Under 18 (Percent of U18) %" = "uninsuredUnder18_percent",
                                                       "Uninsured, 19 to 34 %" = "pop19to34Uninsured_percent",
                                                       "Uninsured, 65 and Older (Percent of 65 and Older) %" = "uninsured65andOlder_percent",
                                                       "Asian population without health insurance coverage %" = "popAsianUnins_percent",
                                                       "Black population without health insurance coverage %" = "popBlackUnins_percent",
                                                       "Hispanic or Latino population without health insurance coverage %" = "popHispUnins_percent",
                                                       "White population without health insurance coverage %" = "popNHWhiteUnins_percent",
                                                       "Two or more races population without health insurance coverage %" = "popTwoMoreUnins_percent",
                                                       "Unemployed Population with Insurance %" = "insUnemployed_percent"),
                       "Housing Burden" = c("Owner Units Paying 30% or more of Housing Costs %" = "OwnerOc30p_percent",
                                          "Renter Units Paying 30% or more of Housing Costs %" = "renterOc30p_percent",
                                          "Housing Units Paying 30% or more of Housing Costs, Age 65 and Older %" = "hBurden30p65Older_percent",
                                          "Owner Units Paying 30% or more of Housing Costs, Age 65 and Older %" = "OwnerOc30p65Older_percent",
                                          "Renter Units Paying 30% or more of Housing Costs, Age 65 and Older %" = "renterOc30p65Older_percent",
                                          "Paying 30% or more of Housing Costs, Householder 35 to 64 years %" = "hBurden30Pct35to64_percent",
                                          "Paying 30% or more of Housing Costs, Householder 65 or Over %" = "hBurden30Pct65Older_percent"),
                       "Public Assistance" = c("Household received Food Stamps/SNAP in the past 12 months %" = "assistSNAP_percent",
                                               "Household received Food Stamps/SNAP in the past 12 months with Asian Householder %" = "popSNAPAsian_percent",
                                               "Household received Food Stamps/SNAP in the past 12 months with Black Householder %" = "popSNAPBlack_percent",
                                               "Household received Food Stamps/SNAP in the past 12 months with Hispanic Householder %" = "popSNAPHispanic_percent",
                                               "Household received Food Stamps/SNAP in the past 12 months with Non-Hispanic White Householder %" = "popSNAPNHWhite_percent",
                                               "With SSI and/or cash public assistance income in the past 12 months %" = "assistSSI_percent"),
                       Business = c("Change in Firms % (Mar - Nov 2020)" = "Firms_pctdiff",
                                    "Change in Employees % (Mar - Nov 2020)" = "Employees_pctdiff",
                                    "Change in Sales % (Mar - Nov 2020)" = "Sales_pctdiff",
                                    "Change in Small Firms % (Mar - Nov 2020)" = "SmallFirms_pctdiff",
                                    "Change in Small Firms Sales % (Mar - Nov 2020)" = "SmallFirmSales_pctdiff",
                                    "Change in Female-Owned Firms % (Mar - Nov 2020)" = "FemaleOwned_pctdiff"),
                       Disability = c("Total population with a disability %" = "disability_percent",
                                      "Total population ages 18 to 34 with a disability %" = "dis18to34_percent",
                                      "Total population ages 18 to 64 with a disability %" = "dis18to64_percent",
                                      "Total population ages 65 to 74 with a disability %" = "dis65to74_percent",
                                      "Total population 75 or older with a disability %" = "dis75AndOlder_percent"),
                       "Household and Group Quarters" = c("Household of Male or Female householder With related children of the householder under 18 years %" = "totalHuSingleParent_percent",
                                                          "Households with one or more people 65 years and over with one person in the household %" = "hu65Alone_percent",
                                                          "Owner Housing Householder 65 and Older %" = "ownerOccupied65Older_percent",
                                                          "Renter Housing Householder 65 and Older %" = "renterOccupied65up_percent",
                                                          "Population in group quarters %" = "groupQuart_percent"),
                       "Transportation and Broadband" = c("Carpool or Use Public Transit %" = "carpoolOrPublic_percent",
                                                          "Average Commute Time (in minutes) %" = "aggTravel_percent",
                                                          "No Vehicles Available in Household, Householder 65 and up %" = "nocar65up_percent",
                                                          "Computer and Broadband in Household %" = "broadbandComp_percent")
                                   )
                    ),
      
      submitButton(text = "Update Plot")
      
    ),
    
    # Shows the plot
    mainPanel(textOutput("info1"), #this will display the correlation coefficient
              textOutput("info2"), #this will display the direction of the relationship
              p(),
              plotlyOutput('plot'), #shows the plot
              p(),
              h6("*COVID case and death data current as of May 11, 2021. Vaccine data current as of May 3, 2021. COVID hospitalization data current as of November 15, 2020."),
              strong("Sources:"),
              p("San Antonio Metropolitan Health District, Texas Department of State Health Services, Texas Workforce Commission, American Community Survey 2019 5-year estimates, Data Axle USA 2020"),
              p(),
              img(src = "CI Now+UTH.png", height = 50,width = 390),
    )
  )
)


server <- function(input, output, session) {
  # Get the data from the variables declared on the ui.R file
  df <- reactive({thc[, c("Zip", input$xCol, input$yCol)]
  })
  
  output$plot <- renderPlotly({
    
    xlab <- names(choiceVecx)[choiceVecx == input$xCol]
    ylab <- names(choiceVecy)[choiceVecy == input$yCol]
    
    plot_ly(df(), 
            x =~get(input$xCol), 
            y =~get(input$yCol), 
            type='scatter', 
            mode='markers',
            text=~Zip,
            hovertemplate=paste(
              "<br> Zip Code: %{text}", 
              "<br> %{xaxis.title.text}: %{x}",
              "<br> %{yaxis.title.text}: %{y}",
              "<extra></extra>")
    )%>%
      layout(title="",
             xaxis=list(title=xlab),
             yaxis=list(title=ylab)
      )
  })
  
  #correlation coefficient text
  output$info1<-renderText({
    data <- df()
    coeff <- round((cor(data[input$xCol], data[input$yCol], method = "pearson", 
                        use = "complete.obs")), 2)
    if (coeff >= 0) {
      direction <- "Positive"
    } else (direction <- "Negative")
    paste0("Correlation coefficient: ", coeff)
  })
  
  output$info2<-renderText({
    data <- df()
    coeff <- round((cor(data[input$xCol], data[input$yCol], method = "pearson", 
                        use = "complete.obs")), 2)
    if (coeff >= 0) {
      direction <- "Positive"
    } else (direction <- "Negative")
    paste0("Direction: ", direction)
  })
  
}

shinyApp(ui = ui, server = server)
