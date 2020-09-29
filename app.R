library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DBI)
library(RSQLite)
library(gridExtra)

filterByDate <- function(inDf, dateSpan) {
    minTime <- as.POSIXct(dateSpan[[1]], tz="CET") - 60*60
    maxTime <- as.POSIXct(dateSpan[[2]], tz="CET") + 23*60*60
    filteredDf <- inDf %>%
        filter(dt_form > minTime) %>%
        filter(dt_form < maxTime)
    return(filteredDf)
}

findDateSpan <- function(inDf) {
    minDate <- as.Date(min(inDf$dt_form))
    maxDate <- as.Date(max(inDf$dt_form))
    return(list(minDate, maxDate))
}

retrieveQCTable <- function(dbPath, tableName) {
    con <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
    dbExecute(con, "PRAGMA foreign_keys = ON")
    sqlQ <- paste("SELECT * FROM", tableName)
    res <- dbSendQuery(con, sqlQ)
    dfQC <- dbFetch(res)
    dbClearResult(res)
    dbDisconnect(con)
    dfQC$dt_form  <- as.POSIXct(dfQC$file_date, format="%m/%d/%Y %I:%M:%OS %p", tz="CET")
    dfQC$msms1000  <- dfQC$msms_number / 1000
    dfQC$id_rate100  <- dfQC$id_rate * 100
    print("Loaded QC data frame of size:")
    print(dim(dfQC))
    #print(head(dfQC))
    return(dfQC)
}

retrieveServTable <- function(dbPath) {
    con <- dbConnect(RSQLite::SQLite(), dbname = dbPath)
    dbExecute(con, "PRAGMA foreign_keys = ON")
    sqlQ <- paste("SELECT * FROM service")
    res <- dbSendQuery(con, sqlQ)
    dfServ <- dbFetch(res)
    dbClearResult(res)
    dbDisconnect(con)
    dfServ$dt_form  <- as.POSIXct(dfServ$date, format="%d/%m/%Y", tz="CET")
    dfServ$h  <- 1
    print("Loaded service data frame of size:")
    print(dim(dfServ))
    #print(dfServ)
    return(dfServ)
}

qcDBs <- list("Lumos" = c("/mnt/protoeomics/Lumos/QC/QC_Reports/QC_DB/qc_lumos_st191029.db", "lumos"),
            "Lumos_FAIMS" = c("/mnt/protoeomics/Lumos/QC/QC_Reports/QC_DB/qc_lumos_st191029.db", "lumos_faims"),
            "Fusion" = c("/mnt/protoeomics/Fusion/QC/QC_Reports/QC_DB/qc_fusion_st191029.db", "fusion"),
            "QEHF" = c("/mnt/protoeomics/QExactive HF/QC/QC_Reports/QC_DB/qc_qehf_st191107.db", "qehf"))

#Margins for all plots
PLOT_MARGINS <- c(0.5, 0.5, 0.5, 0.5)

# Create tabs with the scatter plots and box plots
ui <- dashboardPage(

    dashboardHeader(title="QC Dashboard", titleWidth=320),

    dashboardSidebar(
        width=320,
        sidebarMenu(
            # Select the project to display the files
            selectInput("instrument",
                        label = "Select instrument to report:", names(qcDBs),
                        selected = "Lumos", multiple = FALSE
            ),
            
            br(),
            
            # Select date range for the QC report
            uiOutput("qcRange")
        )
    ),

    # Main panel for displaying outputs ----
    dashboardBody(
        fluidRow(
            uiOutput("latestFileBox"),
            uiOutput("latestPSMsBox"),
            uiOutput("maxPSMsBox"),
            uiOutput("maxEverPSMsBox"),
            uiOutput("entryCountBox"),
            uiOutput("displayingBox")
        ),
        #fluidRow(
        #    box(width = 8, h4(textOutput("selQCDates") ) )
        #),
        fluidRow(
            box(title = "Service/cleaning timeline:",
                status = "primary", solidHeader = TRUE,
                width = 12, height = "200px", plotOutput("servPlot") ),
            box(width = 12, height = "1050px", status = "primary", plotOutput("qcPlots") ),
            box(width = 12, height = "350px", status = "primary", plotOutput("rtsPlot") ),
            box(title = "General relationships between varables:",
                status = "primary", solidHeader = TRUE,
                width = 12, height = "350px", plotOutput("generalPlots") )
        )
    )
)


server <- function(input, output) {

    output$qcRange <- renderUI({
        qcMaxSpan <- findDateSpan(updateqQCDF())
        dateRangeInput("qcRange",
                        label = "Select date range to report:",
                        start = qcMaxSpan[[1]], end = qcMaxSpan[[2]],
                        min = qcMaxSpan[[1]], max = qcMaxSpan[[2]],
                        weekstart = 1,
            )
        }
    )
    
    updateqQCDF <- reactive({
            dfQC <- retrieveQCTable(qcDBs[[input$instrument]][1], qcDBs[[input$instrument]][2])
        }
    )
    
    updateServDF <- reactive({
            dfServ <- retrieveServTable(qcDBs[[input$instrument]][1])
        }
    )
    
    updatedQCSpan <- reactive({
            #print("Input QC Range 1:")
            #print(input$qcRange[1])
            list(as.Date(input$qcRange[1], origin = "1970-01-01"),
                as.Date(input$qcRange[2], origin = "1970-01-01"))
        }
    )
    
    #Info boxes with useful bits of data
    output$latestFileBox <- renderUI({
        qcDF <- updateqQCDF()
        latestFName <- qcDF[rev( order( qcDF["dt_form"] ) ),][1,]["raw_file"]
        n <- substr(latestFName, 1, nchar(latestFName)-4)

        infoBox( "Latest file", n, icon = icon("file"),
            width = 4, color = "orange", fill = TRUE )
    })
    output$latestPSMsBox <- renderUI({
        qcDF <- updateqQCDF()
        latestPSMs <- qcDF[rev( order( qcDF["dt_form"] ) ),][1,]["psm_number"]
        infoBox( "Latest PSMs", latestPSMs, icon = icon("calculator"),
            width = 4, color = "orange", fill = TRUE )
    })
    output$maxPSMsBox <- renderUI({
        qcDF <- updateqQCDF()
        updSpan <- updatedQCSpan()
        minTime <- as.POSIXct(updSpan[[1]], tz="CET") - 60*60
        maxTime <- as.POSIXct(updSpan[[2]], tz="CET") + 23*60*60
        dfF <- qcDF %>%
            filter(dt_form >= minTime ) %>%
            filter(dt_form <= maxTime)
        maxPeriodPSM <- dfF["psm_number"][order( -dfF["psm_number"] ),][1]
        infoBox( "Max PSMs in interval", maxPeriodPSM, icon = icon("calculator"),
            width = 4, fill = TRUE )
    })
    output$maxEverPSMsBox <- renderUI({
        qcDF <- updateqQCDF()
        maxPSMallTime <- qcDF["psm_number"][order( -qcDF["psm_number"] ),][1]
        infoBox( "All-time Max PSMs", maxPSMallTime, icon = icon("chart-line"),
            width = 4, fill = TRUE )
    })
    output$entryCountBox <- renderUI({
        infoBox( "Entries in DB", nrow(updateqQCDF()), icon = icon("database"),
            width = 4, fill = TRUE )
    })
    output$displayingBox <- renderUI({
            text1 <- paste("Displaying plots for the period ",
                paste(input$qcRange, collapse = " - "),
                ":"
            )
        infoBox( input$instrument, text1, icon = icon("chart-area"),
            width = 9, fill = TRUE )
    })
    #The plot showing the latest service/cleaning events
    output$servPlot <- renderPlot({
            servDF <- updateServDF()
            updSpan <- updatedQCSpan()
            minTime <- as.POSIXct(updSpan[[1]], tz="CET") - 60*60
            maxTime <- as.POSIXct(updSpan[[2]], tz="CET") + 23*60*60
            
            themeElement <- theme(text = element_text(size = 14), axis.text.x=element_text(angle=45, hjust=1),
                                    axis.title.x=element_blank(), axis.title.y=element_blank(),
                                    axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                    plot.margin = unit(PLOT_MARGINS, "cm") )
            xDateElement <- scale_x_datetime(date_labels= "%Y-%m-%d", limits= c(minTime, maxTime) )
            
            pServ <- ggplot(servDF, aes(x=dt_form, y=h, label=type))  +
                geom_bar(stat="identity",width = 500000, fill="red") + 
                theme_bw() + geom_text(size=5, nudge_y=0.2, color="red") +
                ylim(0, 1.3) +
                themeElement + xDateElement
                
            grid.arrange(pServ, ncol=1)
        },
        height = 140
    )
    
    #Global plots that show insights into the relationships in the data 
    output$generalPlots <- renderPlot({
            qcDF <- updateqQCDF()
            
            p10 <- ggplot(qcDF, aes(x=mean_sengine_score, y=psm_number)) +
                geom_point(shape=19, color="blue", alpha=0.5) +
                theme_bw() +  xlab("Search engine score") + ylab("PSMs") +
                theme(text = element_text(size = 14), plot.margin = unit(PLOT_MARGINS, "cm"))
                
            p11 <- ggplot(qcDF, aes(x=mean_peak_width, y=psm_number)) +
                geom_point(shape=19, color="orange", alpha=0.8) +
                theme_bw() +  xlab("Mean peak width, s") + ylab("PSMs") +
                theme(text = element_text(size = 14), plot.margin = unit(PLOT_MARGINS, "cm")) 
                
            grid.arrange(p10,p11, ncol=2)
        },
        height = 290
    )
    #The panel of QC plots in a grid
    output$qcPlots <- renderPlot({
            qcDF <- updateqQCDF()
            updSpan <- updatedQCSpan()
            minTime <- as.POSIXct(updSpan[[1]], tz="CET") - 60*60
            maxTime <- as.POSIXct(updSpan[[2]], tz="CET") + 23*60*60
            
            themeElement <- theme(text = element_text(size = 14), axis.text.x=element_text(angle=45, hjust=1),
                                    plot.margin = unit(PLOT_MARGINS, "cm"))
            xDateElement <- scale_x_datetime(date_labels= "%Y-%m-%d", limits= c(minTime, maxTime))
            
            p1 <- ggplot(qcDF, aes(x=dt_form, y=psm_number)) + geom_line(color="blue", size=1) +
                theme_bw() +  xlab("Date") + ylab("PSMs") +
                themeElement + xDateElement
                
            p2 <- ggplot() +
                geom_line(data=qcDF, aes(y=id_rate100, x=dt_form,color="darkred"), size=1) + 
                geom_line(data=qcDF, aes(y=msms1000, x=dt_form,color="steelblue"), size=1, linetype="dashed") +
                scale_color_discrete(name="", labels = c("ID rate, %", "MSMS/1000")) +
                theme_bw() + xlab("Date") + ylab("") +
                themeElement + xDateElement
                
            p3 <- ggplot(qcDF, aes(x=dt_form, y=mean_mz_err_ppm)) + geom_line(color="steelblue", size=1) +
                theme_bw() +  xlab("Date") + ylab("Mass error, ppm") +
                themeElement + xDateElement
                
            p4 <- ggplot(qcDF, aes(x=dt_form, y=mz_err_ppm_stdev)) + geom_line(color="#db7979", size=1) +
                theme_bw() +  xlab("Date") + ylab("Mass error st. dev, ppm") +
                themeElement + xDateElement
                
            p5 <- ggplot(qcDF, aes(x=dt_form, y=mean_peak_width)) + geom_line(color="blue", size=1) +
                theme_bw() + xlab("Date") + ylab("Mean peak width, s") +
                themeElement + xDateElement
                
            p6 <- ggplot(qcDF, aes(x=dt_form, y=median_psm_it_ms)) + geom_line(color="steelblue", size=1) +
                theme_bw() +  xlab("Date") + ylab("Median PSM IT, ms") +
                themeElement + xDateElement
                
            p7 <- ggplot(qcDF, aes(x=dt_form, y=mean_prec_intensity)) + geom_line(color="steelblue", size=1) +
                theme_bw() +  xlab("Date") + ylab("Mean precursor intensity") +
                themeElement + xDateElement
                
            p8 <- ggplot(qcDF, aes(x=dt_form, y=mean_sengine_score)) + geom_line(color="#db7979", size=1) +
                theme_bw() +  xlab("Date") + ylab("Search engine score") +
                themeElement + xDateElement
                
            grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, ncol=2)
        },
        height = 1000
    )
    #Retention times of the selected QC peptides
    output$rtsPlot <- renderPlot({
            qcDF <- updateqQCDF()
            updSpan <- updatedQCSpan()
            minTime <- as.POSIXct(updSpan[[1]], tz="CET") - 60*60
            maxTime <- as.POSIXct(updSpan[[2]], tz="CET") + 23*60*60
            
            themeElement <- theme(text = element_text(size = 14), axis.text.x=element_text(angle=45, hjust=1),
                                    plot.margin = unit(PLOT_MARGINS, "cm"))
            xDateElement <- scale_x_datetime(date_labels= "%Y-%m-%d", limits= c(minTime, maxTime) )
            
            pRT <- ggplot() +
                geom_line(data=qcDF, aes(y=pept_416, x=dt_form,color="darkred"), size=1) + 
                geom_line(data=qcDF, aes(y=pept_425, x=dt_form,color="orange"), size=1) +
                geom_line(data=qcDF, aes(y=pept_488, x=dt_form,color="red"), size=1) +
                geom_line(data=qcDF, aes(y=pept_495, x=dt_form,color="steelblue"), size=1) +
                geom_line(data=qcDF, aes(y=pept_567, x=dt_form,color="blue"), size=1) +
                geom_line(data=qcDF, aes(y=pept_652, x=dt_form,color="purple"), size=1) +
                geom_line(data=qcDF, aes(y=pept_655, x=dt_form,color="green"), size=1) +
                scale_color_discrete(name="Peptide m/z",
                    labels = c("416","425","488","495","567","652","655") ) +
                theme_bw() + xlab("Date") + ylab("RT, min") +
                themeElement + xDateElement
                
            grid.arrange(pRT, ncol=1)
        },
        height = 320
    )
    
}

shinyApp(ui, server)
