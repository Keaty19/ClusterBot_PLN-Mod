library(shiny)

# Row of the shiny app that contains the title of the application
Row1 <- fluidRow(
  # Application title
  titlePanel("ClusterBot - Phenotypic Linkage Network Web App")
  )

# Row of the shiny app that contains the data input, plot and statistical data
Row2 <- fluidRow(width=12,
                 # Tabset containing the Example Data and User Data panel
                 column(width=2,tabsetPanel(tabPanel("Example Data", # Example data panel
                                                     sidebarPanel(width=12,h4("Example Data"),
                                                                  selectInput("pln", "Choose a Phenotypic Linkage Network:",
                                                                              choices = c("General PLN", "Metabolic PLN", "Nervous PLN")),
                                                                  conditionalPanel(
                                                                    condition="input.pln == 'General PLN'",
                                                                    selectInput("gene", "Choose a Set of Genes:",
                                                                                choices = c("coexpr_gse", "sem_goabp")),
                                                                    checkboxGroupInput("genesCheck", h3("Select a Set of Genes:"),
                                                                                       choices = list("coexpr_gse" , "sem_goabp"),selected = FALSE)),
                                                                  conditionalPanel(
                                                                    condition="input.pln == 'Metabolic PLN'",
                                                                    selectInput("gene", "Choose a Set of Genes:",
                                                                                choices = c("pd_de_1","s9_genes")),
                                                                    checkboxGroupInput("genesCheck", h3("Select a Set of Genes:"),
                                                                                       choices = list("pd_de_1", "s9_genes"),selected = FALSE)),
                                                                  conditionalPanel(
                                                                    condition="input.pln == 'Nervous PLN'",
                                                                    selectInput("gene", "Choose a Set of Genes:",
                                                                                choices = c("Nervous PLN 1","Nervous PLN 2"),selected=FALSE)),
                 )),
                 tabPanel("User Data", # User data panel
                          sidebarPanel(width=12,h4("User Data"),
                                       # Input: Select a file ----
                                       fileInput("file1", "Choose Genes File",
                                                 multiple = FALSE, # Only accept one singular upload
                                                 accept = c("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv")), # Allow text/CSV/plain text format
                                       
                                       fileInput("file2", "Choose Relationships File",
                                                 multiple = FALSE, # Only accept one singular upload
                                                 accept = c("text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv")), # Allow text/CSV/plain text format
                                       # Horizontal line ----
                                       tags$hr(),
                                       
                                       # Input: Checkbox if file has header ----
                                       checkboxInput("header", "Header", TRUE), 
                                       
                                       # Input: Select separator ----
                                       radioButtons("sep", "Separator",
                                                    choices = c(Comma = ",",
                                                                Semicolon = ";",
                                                                Tab = "\t"),
                                                    selected = ","), # Set as comma seperated values by default
                                       
                                       # Input: Select quotes ----
                                       radioButtons("quote", "Quote",
                                                    choices = c(None = "",
                                                                "Double Quote" = '"',
                                                                "Single Quote" = "'"),
                                                    selected = '"'), # Set as speech mark quotes by default
                                       # Horizontal line ----
                                       tags$hr(),
                                       
                                       # Input: Select number of rows to display ----
                                       radioButtons("disp", "Display",
                                                    choices = c(Head = "head", # Just display the head of the upload file
                                                                All = "all"), # Display all of the file
                                                    selected = "head")
                          )
                 )),
                 wellPanel(
                   h4("Gene Network Parameters"),
                   selectInput("layout", "Graph Layout:", # Select input for the PLN graph layout
                               choices = c("Hierarchical","Improved"),selected="Improved"),
                   h4("Set of Genes to Visualise"),
                   radioButtons("geneset","Set of Genes:",
                                choices = c(User = "user", # Just display the head of the upload file
                                            Curated = "curated"), # Display all of the file
                                selected = "curated"),
                   radioButtons("sources","Sources of Dataset:",
                                choices = c("User Gene Relationship" = "userSource", # Just display the head of the upload file
                                            "PLN & PLN Source" = "plnSource"), # Display all of the file
                                selected = "PLN & PLN Source")),
                 class="col-md-2"), # Define the column as bootstrap grid system width 3 (medium pixelation =>768)
                 
                 column(width=8, # Column containing the visNetwork PLN gene network
                        mainPanel(
                          h4("Phenotypic Linkage Network"),
                          visNetworkOutput("visNet",height="700px"),width="100%"),
                          sliderInput("nodeSlider",h3("Number of Points to Display"), # Slider to select the number of nodes to display
                                    min=0,max=300,value=50,width="100%"), # Output the visNetwork PLN
                        class="col-md-8"), # Define the column as bootstrap grid system width 6 (medium pixelation =>768)
                 column(width=2, # Column containing 
                          wellPanel(
                            h3("Clustering Gene Set"),
                            selectInput("relationship", h4("Select Relationship:"),
                                        choices = c("Relationship1", "Relationship2")),
                            checkboxGroupInput("geneSetCheck", h4("Gene Set:"),
                                               choices = list("Gene Set 1" , "Gene Set 2"),selected = FALSE)),
                            tabsetPanel(
                              tabPanel("Gene Ontology",sidebarPanel(width=12,
                                                                    h5("Gene Ontology"),
                                                                    tableOutput("ontology"))), # Container for the gene ontology for each gene
                              tabPanel("Tissue Pathway",sidebarPanel(width=12,
                                                                     h5("Tissue Pathway"),
                                                                     tableOutput("pathway"))), # Tissue pathway information
                              tabPanel("Extra Tab (Statistics)",sidebarPanel(width=12,
                                                                     h5("Extra Tab (Statistics)"),
                                                                     textOutput("quickCluster"),
                                                                     plotOutput("hist"),
                                                                     tableOutput("statistics"))), # Tissue pathway information
                              tabPanel("User Data",sidebarPanel(width=12,
                                                                h5("User Input Data"),
                                                                tableOutput("userData"),
                                                                tableOutput("userDataEdges")))),
                        class="col-md-2")
                 )

# Row containing the update button and PLN customisation
Row3 <- fluidRow(width=12,
                    column(width=2, # Column containing the update button
                        actionButton("update", "Update View"),class="col-md-2")# Update button
                               #  column(width=6,
                                #        wellPanel(
                                #          h4("Gene Network Parameters"),
                                #          selectInput("layout", "Graph Layout:", # Select input for the PLN graph layout
                                #                      choices = c("Hierarchical","Improved"),selected="Improved"),
                                #          h4("Set of Genes to Visualise"),
                                #          radioButtons("geneset","Set of Genes:",
                                #                       choices = c(User = "user", # Just display the head of the upload file
                                #                                   Curated = "curated"), # Display all of the file
                                #                       selected = "curated"),
                                #          radioButtons("sources","Sources of Dataset:",
                                #                       choices = c("User Gene Relationship" = "userSource", # Just display the head of the upload file
                                #                                   "PLN & PLN Source" = "plnSource"), # Display all of the file
                                #                       selected = "PLN & PLN Source")
                                #        ),class="col-md-6")
                                 #column(width=3, # Column containing 
                                 #       tabsetPanel(
                                #          tabPanel("Gene Ontology",sidebarPanel(width=12,
                                #                                                h5("Gene Ontology"),
                                #                                                tableOutput("ontology"))), # Container for the gene ontology for each gene
                                #          tabPanel("Tissue Pathway",sidebarPanel(width=12,
                                #                                                 h5("Tissue Pathway"),
                                #                                                 tableOutput("pathway"))), # Tissue pathway information
                                #          tabPanel("User Data",sidebarPanel(width=12,
                                #                                            h5("User Input Data"),
                                #                                            tableOutput("userData"),
                                #                                            tableOutput("userDataEdges")))), # User data display
                                #        class="col-md-3")
                 )