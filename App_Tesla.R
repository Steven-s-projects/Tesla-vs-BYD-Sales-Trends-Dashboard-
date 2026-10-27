# The variables are based on the data analysis. If u can not run it, you should run the "analysis_report.qmd".
library(shiny); library(shinythemes); library(dplyr); library(tidyr); library(ggplot2)
library(scales); library(lubridate); library(forcats); library(stringr); library(readr)
library(grid); library(viridis); library(DT); library(shinyjs); library(shinyBS);library(gridExtra)


normalize_names <- function(v) {
  v |> stringr::str_trim() |> stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |> stringr::str_replace_all("^_|_$","")
}
z_safe <- function(x){xnum <- as.numeric(x); m<-mean(xnum,na.rm=TRUE); s<-sd(xnum,na.rm=TRUE); if(is.na(s)||s==0) return(rep(0,length(xnum))); (xnum-m)/s}

# ---- Load data ----
tesla <- read_csv("tesla_vehicle_deliveries.csv", show_col_types = FALSE)
comp  <- read_csv("electric_vehicles_dataset.csv", show_col_types = FALSE)
market_data <- read_csv("market_data.csv", show_col_types = FALSE)

# Keep lowercase for internal processing
names(tesla)       <- normalize_names(names(tesla))
names(comp)        <- normalize_names(names(comp))
names(market_data) <- normalize_names(names(market_data))

# ---- Cleaning (as in Part 4) ----
comp <- distinct(comp)
comp <- comp[, colSums(is.na(comp)) < nrow(comp)]
comp_clean <- comp %>%
  filter(year >= 2019 & year <= 2024) %>%
  filter(!is.na(manufacturer) & manufacturer != "", !is.na(model) & model != "") %>%
  mutate(model = gsub("\\s*\\([^\\)]+\\)", "", model), model = str_trim(model)) %>%
  distinct(manufacturer, model, year, .keep_all = TRUE) %>%
  arrange(manufacturer, year)

tesla_clean <- tesla %>%
  filter(year >= 2019 & year <= 2024) %>% distinct() %>%
  filter(if_any(everything(), ~ !is.na(.))) %>%
  filter(!is.na(model) & model != "", !is.na(region) & region != "") %>%
  arrange(year, region)

# ---- Objects for tabs ----
tesla_models <- tesla_clean %>% filter(grepl("Model 3|Model Y|Model S", model, ignore.case = TRUE))
model_deliveries <- tesla_models %>% group_by(year, model) %>%
  summarise(Total_Deliveries = sum(units_delivered, na.rm = TRUE), .groups = "drop") %>%
  mutate(model = factor(model, levels = c("Model 3","Model Y","Model S")))

battery_summary <- comp_clean %>% group_by(manufacturer, year) %>%
  summarise(avg_battery = mean(battery_capacity_kwh, na.rm = TRUE),
            median_battery = median(battery_capacity_kwh, na.rm = TRUE),
            n_models = n(), .groups = "drop") %>% arrange(desc(avg_battery))

price_country_summary <- comp_clean %>%
  group_by(country_of_manufacture, manufacturer, year) %>%
  summarise(avg_price = mean(price_usd, na.rm = TRUE),
            n_obs = sum(!is.na(price_usd)), .groups = "drop") %>%
  group_by(country_of_manufacture, manufacturer) %>% arrange(year) %>%
  mutate(yoy_pct = (avg_price/lag(avg_price)-1)*100) %>% ungroup()

selected_countries <- c("United States","China","Germany","United Kingdom","Japan","Australia")
price_for_plot <- price_country_summary %>% rename(country = country_of_manufacture) %>%
  filter(country %in% selected_countries)

tesla_price_demand <- tesla_clean %>% group_by(year) %>%
  summarise(Total_Deliveries = sum(as.numeric(units_delivered), na.rm = TRUE),
            Avg_Price = mean(as.numeric(average_price_usd), na.rm = TRUE))

correlation <- suppressWarnings(cor(tesla_price_demand$Avg_Price, tesla_price_demand$Total_Deliveries, use="complete.obs"))

model_summary <- tesla_clean %>% group_by(model) %>%
  summarise(Avg_Price = mean(average_price_usd, na.rm = TRUE),
            Avg_Efficiency = mean(efficiency_kwh_100km, na.rm = TRUE),
            Total_Deliveries = sum(units_delivered, na.rm = TRUE),
            Avg_Customer_Rating = mean(avg_customer_rating, na.rm = TRUE), .groups = "drop")
model_scores <- model_summary %>%
  mutate(z_Price = -z_safe(Avg_Price), z_Efficiency = -z_safe(Avg_Efficiency),
         z_Deliveries = z_safe(Total_Deliveries), z_Rating = z_safe(Avg_Customer_Rating))
weights <- c(Price=.25, Efficiency=.30, Deliveries=.30, Rating=.15)
results <- model_scores %>%
  mutate(Composite_Score = z_Price*weights["Price"] + z_Efficiency*weights["Efficiency"] +
           z_Deliveries*weights["Deliveries"] + z_Rating*weights["Rating"]) %>%
  arrange(desc(Composite_Score)) %>% mutate(Rank=row_number()) %>%
  select(Rank, model, Avg_Price, Avg_Efficiency, Total_Deliveries, Avg_Customer_Rating, Composite_Score) %>%
  mutate(Avg_Price=round(Avg_Price,2), Avg_Efficiency=round(Avg_Efficiency,2),
         Avg_Customer_Rating=round(Avg_Customer_Rating,2), Composite_Score=round(Composite_Score,3))

market_data <- market_data %>%
  mutate(market_share_usd = as.numeric(gsub("[\\$,]", "", market_share_usd)),
         market_cap_usd   = as.numeric(gsub("[\\$,]", "", market_cap_usd)))
market_share_summary <- market_data %>%
  filter(year >= 2019 & year <= 2024) %>%
  group_by(brand, year) %>%
  summarise(market_share_usd = mean(market_share_usd, na.rm = TRUE), .groups = "drop")
market_cap_summary <- market_data %>%
  group_by(brand, year) %>%
  summarise(market_cap_usd = mean(market_cap_usd, na.rm = TRUE), .groups = "drop")

best_tesla_by_region <- tesla_clean %>% group_by(region, model) %>%
  summarise(Total_Units = sum(units_delivered, na.rm = TRUE), .groups = "drop_last") %>%
  arrange(region, desc(Total_Units)) %>% slice_max(order_by = Total_Units, n = 1, with_ties = FALSE)


# ---- UI (fixed parentheses & commas) ----
ui <- navbarPage(
  title = "Tesla Market Performance Dashboard",
  theme = shinytheme("flatly"),
  
  # --- CSS fix for keeping all Business Insights text within the box ---
  header = tags$head(
    tags$style(HTML("
      /* Make all text inside panels wrap properly within the 700px container */
      .panel, .panel-body, .card-body {
        white-space: normal !important;      /* Allow text to wrap */
        word-break: break-word !important;   /* Force line breaks if needed */
        overflow-wrap: break-word !important;/* Prevent text from overflowing */
        max-width: 100%;                     /* Keep content inside the panel width */
        box-sizing: border-box;              /* Include padding in width calculation */
      }

      /* Ensure the bsCollapsePanel heading text also stays within box */
      .panel-heading, .card-header {
        white-space: normal !important;
        overflow-wrap: break-word;
      }

      /* Improve readability and spacing */
      .panel-body p {
        font-size: 15px;
        line-height: 1.6;
        color: #2C3E50;
        margin-bottom: 8px;
      }

      /* Optional: add subtle shadow and padding for better appearance */
      .panel {
        border-radius: 10px;
        box-shadow: 0 3px 6px rgba(0,0,0,0.1);
        padding: 5px;
      }

      /* Ensure the entire div stays contained visually */
      div[style*='display:inline-block'] {
        overflow: hidden;          /* Hide anything that might spill out */
        text-overflow: ellipsis;   /* Add '...' if absolutely needed */
      }
    "))
  ),
  
  # 1) Dashboard overview
  tabPanel(
    "Dashboard overview",
    fluidPage(
      br(),
      h3("Dashboard overview"),
      p("The aim of this dashboard is to provide a data-driven overview of Tesla's market and financial performance, focusing on profitability trends, delivery volumes, and competitive positioning in the global EV market. The dashboard supports decision-making by visualizing how Tesla's internal cost structures, global competition, and pricing strategies have affected its financial performance and operational management."),
      bsCollapse(
        bsCollapsePanel(
          "Purpose of the dashboard",
          p("The dashboard is developed to systematically analyze and monitor Tesla's performance indicators(KPI) including delivery trends, price competitiveness, battery capacity and market share over time. It transfers various sources into concise actionable intelligence to diagnose reasons of declining profitability, access delivery performance and evaluate competitive positioning. It assists to explain how Tesla's investing strategies such as price cut R&D investments could influence delivery growth, efficiency and financial results."),
          style = "info"
        ),
        bsCollapsePanel(
          "Who it helps",
          tags$ul(
            tags$li("Executives and managers access profitability trends and make decisions"),
            tags$li("Investor and analysts value financial health, market shares and enhance their investing confidence."),
            tags$li("Business consultants evaluate Tesla's positioning strategy in the global market"),
            tags$li("Market observers have a basic understanding of how competition and regulations influence the growth of Tesla.")
          ),
          style = "primary"
        ),
        bsCollapsePanel(
          "Where to use",
          p("The dashboard could be used in the corporate meeting to evaluate quarterly and annual performance. It gives a brief explanation for investors to have a better understanding of market development and Tesla financial performance. It could be used in an educational or academic setting as a learning tool to help students and researchers understand how to apply data-driven methods. Industries including consulting firms, market analysts or business advisors could use the dashboard to analyze Tesla's position in the EV market and offer insights to clients and investors."),
          style = "success"
        ),
        bsCollapsePanel(
          "When to use",
          tags$ul(
            tags$li("During quarterly and annual financial reviews to understand the progress against targets"),
            tags$li("formulating pricing, production or investment strategies (e.g: Robotaxi and AI projects)"),
            tags$li("During market regulatory changes to access risk exposure and performance trends"),
            tags$li("Long-term planning including evaluating the shift from premium EVs to affordable Tesla models.")
          ),
          style = "warning"
        ),
        bsCollapsePanel(
          "How to use",
          p("Users can identify the correlations between different types of cars and vehicle deliveries in the global market to adjust the strategies of investment in developing a new series of cars, like robotaxi or something else. The stakeholders can use the correlations between layoffs and Tesla sales to adjust the number of employees in different types of cars. Users can identify the correlation between price and gross margin to support pricing optimization and decide how to adjust the price in different types of cars. A comparative analysis of various automotive brands against Tesla vehicles, primarily to illustrate the mainstream automotive market across different brands and provide strategic insights for Tesla's future automotive development. Monitoring the best Tesla model allows Tesla to adjust its business strategy based on where the model performs best."),
          p("Integrating visualization and data interpretation, the dashboard enables evidence-based conclusions on strategic performance and supports data-driven managerial decisions to improve competitiveness in the EV market."),
          style = "danger"
        )
      )
    )
  ),
  
  # =========================================================
  # Business Insights Tab – Scrollable Tesla Insight Gallery
  # =========================================================
  
  tabPanel(
    "Business Insights",
    fluidPage(
      # --- Page styling ---
      style = "overflow-y:auto; max-height:95vh; padding-right:20px;",
      br(),
      h3("Business Insights: Data & Interpretation"),
      p("Each insight is paired with its corresponding visualization to help interpret Tesla's strategic and operational trends."),
      
      # --- Add radio buttons for category selection ---
      radioButtons("plotCategory", "Select Category:",
                   choices = c("Different types of tesla cars" = 1,
                               "Layoffs" = 2,
                               "cars Price" = 3,
                               "competitors" = 4,
                               "Regions" = 5),
                   selected = 1, inline = TRUE),
      
      hr(),
      
      # ---- Conditional panels for different plot categories ----
      
      # Category 1: Different types of tesla cars (p1 and p2)
      conditionalPanel(
        condition = "input.plotCategory == 1",
        div(
          style = "overflow-x:auto; white-space:nowrap; padding-bottom:20px; scroll-behavior:smooth;",
          
          # ======= INSIGHT 1 =======
          div(
            style = "display:inline-block; vertical-align:top; width:700px; margin-right:25px;
                   background-color:#f8f9fa; border-radius:15px; padding:15px; box-shadow:0 3px 6px rgba(0,0,0,0.15);",
            plotOutput("p1", height = 500),
            bsCollapse(
              bsCollapsePanel(
                "1. Tesla Deliveries Comparison: Affordable vs Luxury Models (2019–2024)",
                p(strong("Interpretations:"), "From the chart, we can observe that affordable car deliveries increased steadily until 2022. Meanwhile, luxury cars experienced strong growth until 2022, reaching a peak of 327,073 units, then declined from 2022 to 2023 before recovering to 326,359 units in 2024."),
                p(strong("Recommendations:"), "Based on the bar chart analysis, we recommend that Tesla continue investing in affordable vehicle segments while not neglecting the budget EV market. Additionally, the company should focus on analyzing different luxury models to identify which ones boost deliveries and which ones hinder growth."),
                style = "primary"
              )
            )
          ),
          
          # ======= INSIGHT 2 =======
          div(
            style = "display:inline-block; vertical-align:top; width:700px; margin-right:25px;
                   background-color:#f8f9fa; border-radius:15px; padding:15px; box-shadow:0 3px 6px rgba(0,0,0,0.15);",
            plotOutput("p2", height = 500),
            bsCollapse(
              bsCollapsePanel(
                "2. Tesla Luxury Models Global Deliveries (2022–2024)",
                p(strong("Interpretation:"), "From 2022 to 2023, Model S deliveries continued to decline from 112,991 to 85,581 units. Model X experienced a decrease but slightly recovered, reaching 134,543 deliveries. Meanwhile, Cybertruck demonstrated strong growth, increasing from 86,458 to 106,244 deliveries."),
                p(strong("Insights:"), "Given these trends, we recommend reducing investment in Model S and reallocating the cost savings to initiatives such as Robotaxi and humanoid robots. This strategic shift would help mitigate the risk of losing market share in the affordable EV segment while continuing to innovate in emerging technologies."),
                style = "info"
              )
            )
          )
        )
      ),
      
      # Category 2: Layoffs (p3)
      conditionalPanel(
        condition = "input.plotCategory == 2",
        div(
          style = "overflow-x:auto; white-space:nowrap; padding-bottom:20px; scroll-behavior:smooth;",
          
          # ======= INSIGHT 3 =======
          div(
            style = "display:inline-block; vertical-align:top; width:700px; margin-right:25px;
                   background-color:#f8f9fa; border-radius:15px; padding:15px; box-shadow:0 3px 6px rgba(0,0,0,0.15);",
            plotOutput("p3", height = 500),
            bsCollapse(
              bsCollapsePanel(
                "3. The Correlation between Layoffs and Sales in Tesla (2019-2024)",
                p(strong("Interpretation:"), "The diagram indicates a strong linear correlation between the number of employees and sales performance for both total sales and Model 3/Y sales. However, employee layoffs do not appear to significantly impact sales of other vehicle models."),
                p(strong("Insights and recommendations:"), "Based on the diagram, we can identify a notable turning point in 2023, which coincides with Tesla's workforce reduction of over 10%. Therefore, we suggest strategically reducing staff in departments supporting other vehicle models while maintaining or even expanding the workforce dedicated to Model 3/Y operations. This targeted approach could potentially enhance the company's overall sales performance."),
                style = "success"
              )
            )
          )
        )
      ),
      
      # Category 3: cars Price (p4 and p5)
      conditionalPanel(
        condition = "input.plotCategory == 3",
        div(
          style = "overflow-x:auto; white-space:nowrap; padding-bottom:20px; scroll-behavior:smooth;",
          
          # ======= INSIGHT 4 =======
          div(
            style = "display:inline-block; vertical-align:top; width:700px; margin-right:25px;
                   background-color:#f8f9fa; border-radius:15px; padding:15px; box-shadow:0 3px 6px rgba(0,0,0,0.15);",
            plotOutput("p4", height = 500),
            bsCollapse(
              bsCollapsePanel(
                "4. Tesla Model Gross Margin Analysis",
                p(strong("Interpretation:"), "These two charts with linear trend lines indicate that vehicle price (USD) has a stronger positive correlation with gross margin than the correlation between sales volume and gross margin."),
                p(strong("Insights:"), "Based on these two diagrams, we suggest that Tesla should focus on the pricing strategies of different vehicle types, as this approach shows greater potential for delivering higher gross margins in the future."),
                style = "warning"
              )
            )
          ),
          
          # ======= INSIGHT 5 =======
          div(
            style = "display:inline-block; vertical-align:top; width:700px; margin-right:25px;
                   background-color:#f8f9fa; border-radius:15px; padding:15px; box-shadow:0 3px 6px rgba(0,0,0,0.15);",
            plotOutput("p5", height = 500),
            bsCollapse(
              bsCollapsePanel(
                "5. Price vs Gross Margin Trend by Tesla Model",
                p(strong("Interpretation:"), "These six charts with linear trend lines reveal a positive correlation between vehicle price (USD) and gross margin, particularly for the Model 3 and Model Y."),
                p(strong("Insights and recommendations:"), "Based on these findings, we recommend that Tesla strategically increase the prices of the Model 3 and Model Y to a reasonable extent, as this could enhance future gross margins."),
                style = "danger"
              )
            )
          )
        )
      ),
      
      # Category 4: competitors (p6 and p7)
      conditionalPanel(
        condition = "input.plotCategory == 4",
        div(
          style = "overflow-x:auto; white-space:nowrap; padding-bottom:20px; scroll-behavior:smooth;",
          
          # ======= INSIGHT 6 =======
          div(
            style = "display:inline-block; vertical-align:top; width:700px; margin-right:25px;
                   background-color:#f8f9fa; border-radius:15px; padding:15px; box-shadow:0 3px 6px rgba(0,0,0,0.15);",
            plotOutput("p6", height = 500),
            bsCollapse(
              bsCollapsePanel(
                "6. BYD vs Tesla 2024 Sales Distribution",
                p(strong("Interpretation:"), "Based on the two pie charts, BYD's primary sales drivers are the Atto 3, Dolphin, and Han EV models, while Tesla's main sales contributors in the global market are the Model X, Model Y, and Cybertruck."),
                p(strong("Insights:"), "The diagrams indicate that Tesla focuses more on premium vehicles rather than affordable models, which may explain its competitive disadvantage compared to BYD. Notably, the Model 3—despite being Tesla's most affordable car—recorded the lowest sales in 2024."),
                p(strong("Recommendation:"), "We suggest that developing more affordable vehicles equipped with new technologies (like battery capacity or something else) could help enhance Tesla's competitiveness in the EV market."),
                style = "primary"
              )
            )
          ),
          
          # ======= INSIGHT 7 =======
          div(
            style = "display:inline-block; vertical-align:top; width:700px; margin-right:25px;
                   background-color:#f8f9fa; border-radius:15px; padding:15px; box-shadow:0 3px 6px rgba(0,0,0,0.15);",
            plotOutput("p7", height = 500),
            bsCollapse(
              bsCollapsePanel(
                "7. Battery Capacity Comparison (2019–2024)",
                p(strong("Insights:"), "Tesla's median battery capacity shows a significant fluctuation in 2019-2024 compared with other main EV manufactures BMW, BYD, Nissan. It reaches the peak battery capacity of 125 kWh in 2022, then declining from 2023 onward. Tesla invested heavily in high capacity battery to maintain leadership in EV market until 2022. The median capacity drops after 2022 showing a potential shift to cost-efficient models rather than focusing on range performance. Hyundai and Toyota have steady improvement in battery capacity in recent years, showing positive investment and production. BYD median capacity waves due to diversified model lineup and blade battery production. Although Tesla battery capacity drops significantly after 2022, it is still in the leadership position of battery and energy efficiency."),
                p(strong("Recommendation:"), "Since Tesla's battery technology is competitive, the company should invest budget beyond battery capacity. For example, The company could focus on regional research particularly in Asian and other emerging markets and improve charging accessibility and service networks to enhance customer loyalty and strength its reputation for innovation and convenience."),
                style = "info"
              )
            )
          )
        )
      ),
      
      # Category 5: Regions (p8)
      conditionalPanel(
        condition = "input.plotCategory == 5",
        div(
          style = "overflow-x:auto; white-space:nowrap; padding-bottom:20px; scroll-behavior:smooth;",
          
          # ======= INSIGHT 8 =======
          div(
            style = "display:inline-block; vertical-align:top; width:700px; margin-right:25px;
                   background-color:#f8f9fa; border-radius:15px; padding:15px; box-shadow:0 3px 6px rgba(0,0,0,0.15);",
            plotOutput("p8", height = 500),
            bsCollapse(
              bsCollapsePanel(
                "8. Predicted Best-Selling Tesla Model by Region (2024)",
                p(strong("Interpretations:"), "The XGBoost prediction predict each region's top-selling Tesla model in 2024. Model 3 is the top selling in South America and North America, indicating a strong affordability. In EU, people prefer Model Y showing SUV demand is higher. In Australia, people tend to choose Model X(premium) while Asia sees growing interest in cybertruck segment. Tesla performance is different by region, showing various consumer preferences."),
                p(strong("Insights and Recommendations:"), "Tesla could sustain Model 3 in emerging market as it has higher demand in America, expanding Model Y supply in EU, and ensure Cybertruck position in Asia. Future forecasting could be enhanced by including economic growth indicators."),
                style = "success"
              )
            )
          )
        )
      ),
      
      br(), br()
    )
  ), 
  
  # 4) Conclusion
  tabPanel(
    "Conclusion",
    fluidPage(
      br(),
      h3("Conclusion"),
      p(
        "The dashboard analysis offers a comprehensive, data-driven view of Tesla's market performance between 2019 and 2024, ",
        "revealing the key dynamics behind its profitability decline and competitive positioning. ",
        "The results highlight Tesla's heavy dependence on the Model Y for growth, the negative impact of aggressive price-cutting on margins, ",
        "and the reduced R&D investment in battery innovation since 2022. Moreover, competitors such as BYD and Toyota demonstrate steady expansion ",
        "through affordability, technological consistency, and diversified portfolios."
      ),
      p(
        "To sustain long-term competitiveness, Tesla should prioritize production stability, balanced pricing, and renewed battery innovation ",
        "while improving regional diversification to strengthen its global reach. A strategic balance between cost efficiency and technological advancement, ",
        "supported by transparent investor communication and localized market adaptation, will allow Tesla to maintain profitability, ",
        "reinforce its leadership in the EV industry, and drive sustainable growth in an increasingly competitive global market."
      ),
      
      
      br(),
      h3("Key Recommendations"),
      tags$ul(
        tags$li(strong("Pricing Strategy:"), 
                " Implement moderate price increases for Model 3 and Model Y to improve gross margins."),
        tags$li(strong("Product Portfolio:"), 
                " Develop more affordable vehicles equipped with new technologies to compete effectively in the budget EV segment."),
        tags$li(strong("Workforce Optimization:"), 
                " Expand workforce dedicated to Model 3/Y sales while strategically reducing staff supporting other models."),
        tags$li(strong("Investment Reallocation:"), 
                " Redirect investment from Model S to develop Robotaxi and humanoid robot initiatives.")
      ),
      
      br(),
      h3("Expected Outcomes"),
      tags$ul(
        tags$li("Improve gross margins through optimized pricing."),
        tags$li("Enhance market competitiveness in affordable EV segments."),
        tags$li("Optimize operational efficiency through targeted workforce allocation."),
        tags$li("Position for future growth in autonomous and robotics technologies.")
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  output$p1 <- renderPlot({
    ggplot(delivery_summary, aes(x = factor(year), y = Total_Deliveries, fill = Category)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = scales::comma(Total_Deliveries)),
        position = position_dodge(width = 0.9),
        vjust = -0.3,
        size = 3.5
      ) +
      scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
      labs(
        title = "Tesla Deliveries Comparison: Affordable vs Luxury Models (2019–2024)",
        subtitle = "Affordable: Model 3 & Model Y | Luxury: Model X, Model S, Cybertruck",
        x = "Year",
        y = "Total Deliveries (Units)",
        fill = "Vehicle Category"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "top"
      )
  })
  # 2 Tesla Luxury Models Global Deliveries (2022–2024)
  output$p2 <- renderPlot({
    ggplot(luxury_summary, aes(x = Year, y = Total_Deliveries, color = Model, group = Model)) +
      geom_line(size = 1.4) +
      geom_point(size = 3) +
      geom_text(
        aes(label = comma(Total_Deliveries)),
        vjust = -0.8,
        size = 4
      ) +
      scale_color_manual(values = c("Model S" = "#20b2aa", "Model X" = "#6a5acd", "Cybertruck" = "#ff8c00")) +
      scale_x_continuous(breaks = 2022:2024) +
      labs(
        title = "Tesla Luxury Models Global Deliveries (2022–2024)",
        subtitle = "Comparison of Model X, Model S, and Cybertruck",
        x = "Year",
        y = "Total Deliveries (Units)",
        color = "Luxury Model"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "top"
      )
  })
  
  
  # 3 lay-off
  output$p3 <- renderPlot({
    # Create line chart and add employee count labels
    ggplot(employee_vs_sales, aes(x = Year)) +
      # Three sales lines
      geom_line(aes(y = total_sales, color = "Total Sales"), size = 1.2) +
      geom_line(aes(y = sales..Model.3.Model.Y., color = "Model 3/Y Sales"), size = 1.2) +
      geom_line(aes(y = sales.others., color = "Other Models Sales"), size = 1.2) +
      # Add employee count labels
      geom_label(aes(y = total_sales, label = paste("Employees:", Employee/1000, "k")), 
                 vjust = -0.5, size = 3, fill = "white", alpha = 0.7) +
      labs(
        title = "Tesla Sales Trend and Employee Count by Model (2019-2024)",
        x = "Year",
        y = "Sales (Units)",
        color = "Sales Type"
      ) +
      scale_color_manual(
        values = c(
          "Total Sales" = "red",
          "Model 3/Y Sales" = "blue",
          "Other Models Sales" = "green"
        )
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) + # Use comma as thousand separator
      theme(legend.position = "bottom")
  })
  
  # 4 Tesla Model Gross Margin Analysis
  output$p4 <- renderPlot({
    # Plot 1: Sales vs Gross Margin (with model labels)
    p1 <- ggplot(profitability_matrix, aes(x = Units_Sold_2024, y = estimated_gross_margin)) +
      geom_point(aes(color = Model), size = 4, alpha = 0.7) +
      geom_text_repel(aes(label = Model), size = 3.5, 
                      box.padding = 0.5, point.padding = 0.3) +
      geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
      labs(title = "Tesla Model Sales vs Gross Margin Relationship",
           x = "Sales", y = "Gross Margin (%)",
           color = "Model") +
      theme_minimal() +
      theme(legend.position = "none")
    
    # Plot 2: Price vs Gross Margin (with model labels)
    p2 <- ggplot(profitability_matrix, aes(x = Price_USD, y = estimated_gross_margin)) +
      geom_point(aes(color = Model), size = 4, alpha = 0.7) +
      geom_text_repel(aes(label = Model), size = 3.5,
                      box.padding = 0.5, point.padding = 0.3) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
      labs(title = "Tesla Model Price vs Gross Margin Relationship",
           x = "Price (USD)", y = "Gross Margin (%)",
           color = "Model") +
      theme_minimal() +
      theme(legend.position = "none")
    
    grid.arrange(p1, p2, ncol = 2)
  })
  # 5 Composite Score 
  output$p5 <- renderPlot({
    # Create the faceted plot
    ggplot(profitability_matrix, aes(x = Price_USD, y = estimated_gross_margin, color = Model)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
      facet_wrap(~ Model, scales = "free_x") +
      scale_color_brewer(palette = "Set2") +
      labs(
        title = "Price vs Gross Margin Trend by Tesla Model",
        x = "Price (USD)",
        y = "Estimated Gross Margin (%)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "none"
      )
  })
  
  # 6 Shares Outstanding (fixed brackets + renamed dataset)
  output$p6 <- renderPlot({
    # BYD pie chart
    byd_data <- combined_sales %>% filter(Manufacturer == "BYD")
    p1 <- ggplot(byd_data, aes(x = "", y = Total_Units_Sold_2024, fill = Model)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(
        title = "BYD 2024 Sales Distribution by Model",
        fill = "Model"
      ) +
      theme_void() +
      geom_text(
        aes(label = paste0(round(Percentage, 1), "%")), 
        position = position_stack(vjust = 0.5),
        size = 4
      ) +
      scale_fill_brewer(palette = "Set3")
    
    # Tesla pie chart
    tesla_data <- combined_sales %>% filter(Manufacturer == "Tesla")
    p2 <- ggplot(tesla_data, aes(x = "", y = Total_Units_Sold_2024, fill = Model)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(
        title = "Tesla 2024 Sales Distribution by Model",
        fill = "Model"
      ) +
      theme_void() +
      geom_text(
        aes(label = paste0(round(Percentage, 1), "%")), 
        position = position_stack(vjust = 0.5),
        size = 4
      ) +
      scale_fill_brewer(palette = "Set2")
    
    
    grid.arrange(p1, p2, ncol = 2)
  })
  
  # 7 Market Cap
  output$p7 <- renderPlot({
    ggplot(battery_focus, aes(x = year, y = median_battery, color = manufacturer, group = manufacturer)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      
      # Highlight Tesla line in thicker red
      geom_line(data = tesla_data, aes(x = year, y = median_battery), 
                color = "#E82127", linewidth = 1) +
      geom_point(data = tesla_data, aes(x = year, y = median_battery),
                 color = "#E82127", size = 2) +
      
      # Add gold point + label at Tesla's peak
      geom_point(data = tesla_peak, aes(x = year, y = median_battery),
                 color = "gold", fill = "gold", shape = 15, size = 2, stroke = 1) +
      geom_text(data = tesla_peak,
                aes(x = year, y = median_battery,
                    label = paste0("Peak: ", round(median_battery, 1), " kWh (", year, ")")),
                vjust = 1.8, color = "black", fontface = "bold", size = 3.5) +
      
      # Customize legend colors
      scale_color_manual(values = c(
        "Tesla" = "#E82127",
        "BYD" = "#FFD700",
        "Toyota" = "#6E83A7",
        "BMW" = "#8BA6A9",
        "Hyundai" = "#4DB6AC"
      )) +
      
      labs(
        title = "Median Battery Capacity Comparison (2019–2024)",
        subtitle = "Tesla highlighted in red; peak capacity marked in gold",
        x = "Year",
        y = "Median Battery Capacity (kWh)",
        color = "Manufacturer"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "black"),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        legend.position = "bottom"
      )
  })
  
  # 8 Regional Best 
  output$p8 <- renderPlot({
    ggplot(best_model_2024, aes(x = reorder(region, -predicted_units), 
                                y = predicted_units, fill = model)) +
      geom_col(width = 0.6, color = "white") +
      geom_text(aes(label = model), 
                vjust = 0.5, size = 3, fontface = "bold", color = "gray10") +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = "Predicted Best-Selling Tesla Model by Region (2024)",
        subtitle = "Based on forecasted deliveries from 2019–2023 data",
        x = "Region",
        y = "Predicted Deliveries (Units)",
        fill = "Model"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = "none"
      )
  })
  
}


shinyApp(ui, server)
