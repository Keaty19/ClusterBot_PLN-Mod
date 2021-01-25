library(shiny)

# Row of the shiny app that contains the title of the application
Row1 <- fluidRow(
  # Application title
  titlePanel("Parkinson's Disease Phenotypic Linkage Network")
  )

# Row of the shiny app that contains the data input, plot and statistical data
Row2 <- fluidRow(width=12,
                 # Tabset containing the Example Data and User Data panel
                 column(width=3,tabsetPanel(tabPanel("Example Data", # Example data panel
                                                     sidebarPanel(width=12,h4("Example Data"),
                                                                  selectInput("gene", "Choose a Set of Genes:", # Selection box for the example set of gebes
                                                                              choices = c("coexpr_gse", "sem_goabp","pd_de_1","s9_genes","SN Atlas1","SN Atlas2")),
                                                                  checkboxGroupInput("genesCheck", h3("Select a Set of Genes:"), # Checkbox for the example set of genes
                                                                                     choices = list("coexpr_gse" = 1, "sem_goabp" = 2,"pd_de_1" = 3, "s9_genes" = 4,"SN Atlas1" = 5, "SN Atlas2" = 6),selected = 1))
                 ),
                 tabPanel("User Data", # User data panel
                          sidebarPanel(width=12,h4("User Data"),
                                       # Input: Select a file ----
                                       fileInput("file1", "Choose CSV File",
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
                 )
                 ),
                 class="col-md-3"), # Define the column as bootstrap grid system width 3 (medium pixelation =>768)
                 
                 column(width=6, # Column containing the visNetwork PLN gene network
                        mainPanel(
                          h4("Phenotypic Linkage Network"),
                          visNetworkOutput("visNet"),width="100%"), # Output the visNetwork PLN
                        sliderInput("nodeSlider",h3("Number of Nodes to Display"), # Slider to select the number of nodes to display
                                    min=0,max=300,value=50,width="100%"),
                        class="col-md-6"), # Define the column as bootstrap grid system width 6 (medium pixelation =>768)
                 column(width=3, # Column containing 
                        tabsetPanel(
                          tabPanel("Gene Ontology",sidebarPanel(width=12,
                            h5("Gene Ontology"),
                            tableOutput("ontology"))), # Container for the gene ontology for each gene
                          tabPanel("Tissue Pathway",sidebarPanel(width=12,
                                   tableOutput("pathway"))), # Tissue pathway information
                          tabPanel("User Data",sidebarPanel(width=12,
                                   tableOutput("userData")))), # User data display
                        class="col-md-3")
                    
                 )

# Row containing the update button and PLN customisation
Row3 <- fluidRow(width=12,
                                 column(width=3, # Column containing the update button
                                        actionButton("update", "Update View"),class="col-md-3"), # Update button
                                 column(width=6,
                                        wellPanel(
                                          h4("Gene Network Parameters"),
                                          selectInput("layout", "Graph Layout:", # Select input for the PLN graph layout
                                                      choices = c("Hierarchical","Improved"),selected="Improved"),
                                        ),class="col-md-6")
                 )