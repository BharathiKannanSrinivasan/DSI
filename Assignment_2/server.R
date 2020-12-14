

library(shiny)
shinyServer(function(input, output) {
    output$Missing <- renderPlot({
        vis_dat(dataset) +
            ggtitle("")
    })
    output$Corrgram <- renderPlot({
        corrgram(dataset, 
                 order = input$Group, 
                 abs = input$abs, 
                 cor.method = input$CorrMeth,
                 text.panel = panel.txt,
                 main = "Correlation of given dataset")
    })
    output$Histogram <- renderPlotly({
        plot <- ggplot2::ggplot(data = dataset, mapping = aes_string(x = input$hist)) +
            geom_histogram(bins = input$bin) +
            labs(title = paste(c("HISTOGRAM FOR ",input$hist),collapse = " "))
        
        ggplotly(plot,width=1000,Height=300)
    })
    output$Boxplot <- renderPlot({
        coef <- input$range
        limits <- boxplot.stats(x = unlist(dataset[input$box]), coef = coef)$stats
        dataset$label <- ifelse(unlist(dataset[input$box]) < limits[1] | unlist(dataset[input$box]) > limits[5], rownames(dataset), NA)
        ggplot(data = dataset, mapping = aes(x = unlist(dataset[input$box]), y = 0, label = label)) +
            ggrepel::geom_text_repel() +
            geom_boxplot(coef = coef) +
            labs(title = paste("X Boxplot using", coef, "as IQR Multplier")) +xlab(input$box)+
            theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
    })
    
    
    output$Missing2<-renderPlot({
        naniar::gg_miss_upset(dataset,nsets=input$nset)
    })
    
    output$Scatter<-renderPlot({
        GGally::ggpairs(data = continuous_columns,  mapping = aes(colour = dataset$GOVERNMENT), title = "Pairs of Corona data", progress = FALSE)
    })
    output$Missing_Pattern<-renderPlot({
        rpart.plot(tree$finalModel, main = "TUNED: Predicting the number of missing variables in an observation", roundint = TRUE, clip.facs = TRUE)
        
    })
    output$Residual_graph<-renderPlot({
        
        coef <- 2.2
        limits <- boxplot.stats(x = residuals, coef = coef)$stats
        test$label <- ifelse(residuals < limits[1] | residuals > limits[5], rownames(test), NA)
        
        rang <- range(c(test$DEATHRATE, pred))
        ggplot(test, mapping = aes(x = pred, y = test$DEATHRATE, label = label)) +
            geom_point() +
            ggrepel::geom_text_repel() +
            geom_abline(slope = 1, col = "blue") +
            labs(title = "Model with independent X & Y outliers", y = "actual", x = "predicted") +
            coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE)
    })
  
    output$random<-renderPlot({
        plot<-randomForest(dataset3$DEATHRATE~.,dataset3,importance = TRUE,na.action=na.exclude,ntree=1000)
        varImpPlot(plot,main="")
        
    })
    output$hist2<-renderPlot({
        ggplot(test) +
            geom_histogram(aes(x=residuals), binwidth = input$bin2)  +
            labs(title = "Histogram of residuals showing outliers")
        
    })
    output$Rbox<-renderPlot({
        coef <- 2.2
        limits <- boxplot.stats(x = residuals, coef = coef)$stats
        test$label <- ifelse(residuals < limits[1] | residuals > limits[5], rownames(test), NA)
        
        ggplot(test, aes(y = 1, x = residuals, label = label)) +
            geom_boxplot(coef = coef) +
            ggrepel::geom_label_repel() +
            labs(title = paste("Boxplot using", coef, "as IQR Multplier")) +
            theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
        
    })
    output$Summary1 <- DT::renderDataTable({
        DT::datatable(data = as.data.frame(dataset))
    })
    output$Summary2 <- DT::renderDataTable({
        DT::datatable(data = as.data.frame(train))
    })
    output$Summary3 <- DT::renderDataTable({
        DT::datatable(data = as.data.frame(test))
    })
    output$scatter2<-renderPlotly({
        ploty<-ggplot(dataset,mapping = aes(x = unlist(dataset[input$scat1]), 
                                            y = unlist(dataset[input$scat2]),
        )) + 
            geom_point()+
            labs(title = "Scatter Plot For Variables")+xlab(input$scat1)+ylab(input$scat2)
        ggplotly(ploty)
    })
    output$miscor<-renderPlot({
        m <- is.na(dataset) + 0 # this is a trick that transforms Logical to Binary
        cm <- colMeans(m)
        m <- m[, cm > 0 & cm < 1, drop = FALSE]
        corrgram::corrgram(cor(m), order = "OLO", abs = TRUE)
        title(main = "Variable missing value correlation",
              sub = "Notice whether variables are missing in sets")
        
    })
})


