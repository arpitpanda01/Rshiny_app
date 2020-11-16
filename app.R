# 01-Bar-plots

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
library(dplyr)
library(DESeq2)
library(ggrepel)

SBMA_IPSCs <- readRDS("iPSC_logged_normcounts_purified_transposed_for_bar_plots.Rds")

rat_and_human_genes <- readRDS('Rat_to_human_gene_symbols.rds')
rat_and_human_genes <- data.frame(lapply(rat_and_human_genes, function(x) {gsub("MT-", "MT.", x)}))
autofill_list <- rat_and_human_genes$human_gene_symbol

# This is the user interface that user interacts with
ui <- fluidPage(
  headerPanel('Gene Expression Visualization'),
  # This is where we have 2 sets of inputs for now (pick dataset of interest and pick gene of interest)
  # Dataset of interest has specific options (we have finite number of public datasets) - So selectInput
  # Gene name can be entered as a text input - textInput makes sense
  sidebarPanel(
    selectInput(inputId = 'xcol', label = 'Dataset',c('SBMA iPSCs Okada 2020 Purified')),
    selectInput(inputId = "ycol", label = "Select Gene", 
                choices = unique(autofill_list), 
                selected = 1), 
    ),                      
  mainPanel(
    plotOutput('barplot_1')
  )
)


# This is the server for carrying out the back-end tasks
server <- function(input, output) {
  
  #Download our dataset of interest
  
  selectedData <- reactive({
    SBMA_IPSCs_subset <- data.frame(SBMA_IPSCs[ , colnames(SBMA_IPSCs) == c(input$ycol)])
    rownames(SBMA_IPSCs_subset) <- rownames(SBMA_IPSCs)
    colnames(SBMA_IPSCs_subset) <- c(input$ycol)
    SBMA_IPSCs_subset$sample_name <- rownames(SBMA_IPSCs_subset)
    SBMA_IPSCs_subset$sample_name <- factor(SBMA_IPSCs_subset$sample_name, levels = SBMA_IPSCs_subset$sample_name)
    SBMA_IPSCs_subset
  })
  
  
  output$barplot_1 <- renderPlot({
    p <- ggplot(data = selectedData(), aes_string(x = "sample_name", y = input$ycol)) 
    p + geom_bar(stat = "identity") +
    theme(text = element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlab("Conditions") +
    ylab("Log Normalized Counts") +
    ggtitle(paste(input$ycol)) +
    theme(plot.title = element_text(hjust = 0.5)) 
    
  })
  
}

shinyApp(ui = ui, server = server)
