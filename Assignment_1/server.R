

library(shiny)

shinyServer(function(input, output,session) {
    
    output$Summary1 <- renderPrint({
        summary(dataset)
    })
    output$Summary2 <- renderPrint({
        descr(dataset)
    })
    output$Summary3 <- renderPrint({
        glimpse(dataset)
    })
    output$Summary4 <- renderPrint({
        skim(dataset)
    })
    output$Summary5 <- renderUI({
        print(dfSummary(dataset),method="render")
    })
    output$discrete <- DT::renderDataTable({
        DT::datatable(data = as.data.frame(dataset[discrete_columns]))
    })
    output$continuous <- DT::renderDataTable({
        DT::datatable(data = as.data.frame(dataset[continuous_columns]))
    })
    output$all_data <- DT::renderDataTable({
        DT::datatable(data = as.data.frame(dataset))
    })
    output$Mosaic <- renderPlot({
        formula <- as.formula(paste("~",paste(input$mosaic_choice, collapse = " + ")))
        vcd::mosaic(formula, data = dataset,
                    main = "Mosaic Plot", shade = TRUE, legend = TRUE)
    })
    output$Missing <- renderPlot({
        vis_dat(dataset) +
            ggtitle("Missing Value Distribution")
    })
    output$rising <- renderPlot({
        d <- dataset[input$risingvalue]  
        for (col in 1:ncol(d)) {
            d[,col] <- d[order(d[,col]),col] 
        }
        d <- scale(x = d, center = TRUE, scale = TRUE)  
        mypalette <- rainbow(ncol(d))
        matplot(x = seq(1, 100, length.out = nrow(d)), y = d, type = "l", xlab = "Percentile", ylab = "Values", lty = 1, lwd = 1, col = mypalette, main = "Rising value chart")
        legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.3))
    })
    
    output$Boxplot <- renderPlot({
        data <- as.matrix(dataset[continuous_columns])
        data <- scale(data, center = input$standardise, scale = input$standardise)
        car::Boxplot(y = data, use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
                     horizontal = FALSE, outline = TRUE, 
                     col = brewer.pal(n = dim(dataset[continuous_columns])[2], name = "RdBu"),
                     range = input$range, main = "Boxplots for continuous variables",las=2)
    })
    
    
    output$Corrgram <- renderPlot({
        corrgram(dataset, 
                 order = input$Group, 
                 abs = input$abs, 
                 cor.method = input$CorrMeth,
                 text.panel = panel.txt,
                 main = "Correlation of given dataset")
    })
    
    output$ggpairs1 <-renderPlot({
        if(input$ggplot=="spearman"){
            GGally::ggpairs(data = spearman, title = "Pairs for spearman Correlation - Set1")
        }
        else if(input$ggplot=="pearson"){
            GGally::ggpairs(data = pearson, title = "Pairs for pearson Correlation- Set1")
        }
        else{
            GGally::ggpairs(data = kendall, title = "Pairs for kendall Correlation- Set1")
        }
    })
    output$ggpairs2 <-renderPlot({
        if(input$ggplot=="spearman"){
            GGally::ggpairs(data = spearman2, title = "Pairs for spearman Correlation- Set2")
        }
        else if(input$ggplot=="pearson"){
            GGally::ggpairs(data = pearson2, title = "Pairs for pearson Correlation- Set2")
        }
        else{
            GGally::ggpairs(data = kendall2, title = "Pairs for Kendall Correlation- Set2")
        }
    })
    output$ggpairs3 <-renderPlot({
        if(input$ggplot=="spearman"){
            GGally::ggpairs(data = spearman3, title = "Pairs for spearman Correlation- Set3")
        }
        else if(input$ggplot=="pearson"){
            GGally::ggpairs(data = pearson3,  title = "Pairs for pearson Correlation- Set3")
        }
        else{
            GGally::ggpairs(data = kendall3,title = "Pairs for Kendall Correlation- Set3")
        }
    })
    output$ggpairs4 <-renderPlot({
        if(input$ggplot=="pearson"){
            GGally::ggpairs(data = pearson4,  title = "Pairs for pearson Correlation- Set4")
        }
    })
    output$tabplot<-renderPlot({
        if(input$tab=="Numeric"){
            tableplot(dataset[continuous_columns])
        }
        else if(input$tab=="Catagorical"){
            tableplot(dataset[discrete_columns])
        }
    })
})
