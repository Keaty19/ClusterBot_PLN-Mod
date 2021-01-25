library(shiny)
library(dplyr)
library(igraph)
library(network)
library(visNetwork)
library(shinythemes)

#source("plnFunctions.R",local=TRUE)

plnServer <- function(input,output,session) {
  # Load in the example data
  semgoaNode=read.csv("SEM_NODES.csv",header=T) # sem_goabp nodes and edges
  semgoaEdge=read.csv("SEM_EDGES.csv",header=T)
  
  coexprNode=read.csv("COEXPR_NODES.csv",header=T) # coexpr_gse nodes and edges
  coexprEdge=read.csv("COEXPR_EDGES.csv",header=T)
  
  pdNode=read.csv("PD1_NODES.csv",header=T) # pd_de_1 nodes and edges
  pdEdge=read.csv("PD1_EDGES.csv",header=T)
  
  sNode=read.csv("S9_NODES.csv",header=T) # s9_genes nodes and edges
  sEdge=read.csv("S9_EDGES.csv",header=T)
  
  #sliderValues
  #checkInput
  #datasetInput
  #layoutInput
  #clearUser
  #output$userData
  #output$visNet
  
  sliderValues <- reactive({ # Code for the slider that selects number of nodes
    nodeNum=input$nodeSlider
  })
  
  checkInput=reactive({ # Code for the check boxes to select gene sets
    geneInput=input$genesCheck
  })
  
  datasetInput <- eventReactive(input$update, { # Update reactive input for the dataset
    switch(input$gene, # Switch each of the datasets on update
           "sem_goabp" = "semgoa",
           "coexpr_gse" = "coexpr",
           "pd_de_1" = "pd1",
           "s9_genes" = "s9",
           "SN Atlas1" = "sn1",
           "SN Atlas2" = "sn2")
  }, ignoreNULL = FALSE)
  
  layoutInput <- eventReactive(input$update, { # Update reactive input for the PLN layout
    switch(input$layout,
           "Hierarchical"=1,
           "Improved"=2)
  }, ignoreNULL = FALSE)
  
  clearUser <- eventReactive(input$update, { # Clear the user uploaded data
    rm(input$file1)
  }, ignoreNULL = FALSE)
  
  # Create an output channel for the user entered data
  output$userData <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    #req(input$file1)
    
    userNodes <- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = input$sep,
                          quote = input$quote)
    
    if(input$disp == "head") {
      return(head(userNodes))
    }
    else {
      return(userNodes)
    }
  })
  
  output$visNet <- renderVisNetwork({ # Output for creating the visNetwork PLN
    num=sliderValues() # Set the values of the node slider as num
    
    num=as.numeric(num) # Ensure the node slider input is numeric
    
    if (datasetInput() == "semgoa") { # Set each of the input datasets to the nodes and edges variables
      nodes=as.data.frame(semgoaNode)
      edges=as.data.frame(semgoaEdge)
    }
    else if (datasetInput() == "coexpr") {
      nodes=as.data.frame(coexprNode)
      edges=as.data.frame(coexprEdge)
    } 
    else if (datasetInput() == "pd1") {
      nodes=as.data.frame(pdNode)
      edges=as.data.frame(pdEdge)
    } 
    else if (datasetInput() == "s9") {
      nodes=as.data.frame(sNode)
      edges=as.data.frame(sEdge)
    } 
    else if (datasetInput() == "sn1") {
      nodes=as.data.frame(pdNode)
      edges=as.data.frame(pdEdge)
    } 
    else if (datasetInput() == "sn2") {
      nodes=as.data.frame(sNode)
      edges=as.data.frame(sEdge)
    }
    
    layout=c(TRUE,FALSE)[as.numeric(layoutInput())] # Convert the numeric input to boolean for hierarchical layout
    
    nodes=nodes[1:num,] # Limit the number of nodes (rows) based on the node slider
    nodes=nodes[!duplicated(nodes$id),] # Remove duplicated nodes (crucial for visNetwork graphs)
    
    nodes=as.data.frame(nodes) # Ensure input nodes are in data.frame format
    edges=as.data.frame(edges) # Ensure input edges are in data.frame format
    
    vis.nodes <- nodes # copy nodes to seperate variable for customisation
    vis.edges <- edges # copy edges to seperate variable for customisation
    
    vis.nodes$shape  <- c("dot","square","diamond","triangle","triangleDown")[nodes$node.type] # Shape of node depends on node type 
    vis.nodes$shadow <- TRUE # Nodes will drop shadow
    vis.nodes$title  <- vis.nodes$node.label # Text on click
    vis.nodes$label  <- vis.nodes$node # Node label # Node size
    vis.nodes$borderWidth <- 2 # Node border width
    
    vis.nodes$color.background <- c("blue", "green", "red","green","red")[nodes$node.type] # Colour of node depends on node type
    vis.nodes$color.border <- "black" # Nodes will have black border
    vis.nodes$color.highlight.background <- "orange" # Nodes will appear orange when highlighted
    vis.nodes$color.highlight.border <- "darkred" # Node border will appear dark red when highlighted
    vis.nodes$group=nodes$vis.nodes.label # Nodes are grouped by node label (GENE, PROTEIN etc.)
    
    vis.edges$color <- "gray"    # line color  
    vis.edges$title <- edges$weight # what the line displays on click
    vis.edges$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
    vis.edges$smooth <- FALSE    # should the edges be curved?
    vis.edges$shadow <- FALSE    # edge shadow
    
    if (datasetInput() == "pd1") { # Settings for the pd_de_1 dataset
      vis.nodes$size=abs(nodes$log2FoldChange)*20 # Set the size of the node to be proportional to the log2Fold change
      
      # Test that appears when you select the node (including statistics)
      vis.nodes$title  <- paste(vis.nodes$node.label,paste("Log2 Fold Change: ",nodes$log2FoldChange,sep=""),
                                paste("p-Value: ",nodes$pvalue,sep=""),paste("FDR Adjusted p-Value: ",
                                                                             nodes$padj,sep=""),paste("Regulated: ",nodes$Regulated,sep=""),sep="<br />")
      
      # Plot the visNetwork graph
      visNetwork(nodes=vis.nodes,edges=vis.edges,width="100%",height="400px") %>%
        visOptions(autoResize=TRUE,highlightNearest=TRUE,selectedBy="Regulated") %>% # Highlighting selects nearest gene. Select by up/down regulated genes
        visGroups(groupname="GENE",shape="dot",color=list(background="blue",border="black")) %>% # Group based on GENE
        visLegend(main="Legend",position="right",ncol=1) %>% # Add legend based on groups
        visLayout(hierarchical=layout) # Change the layout of the graph (if TRUE, lay out in hierarchical format
    } 
    else if (datasetInput() == "s9") { # Settings for the s9_genes dataset
      # Add annotations for when the node is clicked on
      vis.nodes$title  <- paste(vis.nodes$node.label,paste("Monogenic: ",nodes$Mono,sep=""),paste("T2D GWA: ",nodes$GWA,sep=""),paste("Exome: ",nodes$Exome,sep=""),paste("African American: ",nodes$AfrAmer,sep=""),
                                paste("Hispanic: ",nodes$Hispanic,sep=""),paste("South Asian: ",nodes$SAsian,sep=""),paste("East Asian: ",nodes$EAsian,sep=""),sep="<br />")
      
      visNetwork(nodes=vis.nodes,edges=vis.edges,width="100%",height="400px") %>%
        visOptions(autoResize=TRUE,highlightNearest=TRUE,selectedBy="Mono") %>% # Select based on presence on monogenic and syndromic gene
        visGroups(groupname="GENE",shape="dot",color=list(background="blue",border="black")) %>%
        visLegend(main="Legend",position="right",ncol=1)%>%
        visLayout(hierarchical=layout)
    } 
    else { # Settings for the other datsets
      vis.edges$width <- 68+((log(edges$weight)+1)*100) # line width
      
      visNetwork(nodes=vis.nodes,edges=vis.edges,width="100%",height="400px") %>%
        visOptions(autoResize=TRUE,highlightNearest=TRUE,selectedBy="node.label") %>% # Select by node label (e.g. GENE)
        visGroups(groupname="GENE",shape="dot",color=list(background="blue",border="black")) %>%
        visLegend(main="Legend",position="right",ncol=1)%>%
        visLayout(hierarchical=layout)
    }
  })
}