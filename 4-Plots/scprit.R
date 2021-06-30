library(ggplot2)
library(scales)

typeOfFramework = "Code Samples"
frameworkName1 = "Android"
frameworkName2 = "AWS"
frameworkName3 = "Azure"
frameworkName4 = "Spring"
factorPositionMedianLabel = 1.2
mainDirectory = "/home/gabriel/Documentos/gabrielsmenezes/VEM2021-PullResquest/"

plotGraphic <- function ()  {
  p1 <- ggplot(all, aes) + 
    #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
    #scale_y_log10(labels = comma) +
    geom_violin(width=1, trim=TRUE, fill="#87CEFA") + 
    geom_boxplot(width=0.7,alpha=0.7) + ggtitle(title) + 
    xlab(typeOfFramework) + 
    ylab(verticalTitle) + 
    annotate("text", x = 1.1, y = framework1_median*factorPositionMedianLabel, label = round(framework1_median, 2), size = 6) + 
    annotate("text", x = 2.1, y = framework2_median*factorPositionMedianLabel, label = round(framework2_median, 2), size = 6) +
    annotate("text", x = 3.1, y = framework3_median*factorPositionMedianLabel, label = round(framework3_median, 2), size = 6) +
    annotate("text", x = 4.1, y = framework4_median*factorPositionMedianLabel, label = round(framework4_median, 2), size = 6) +
    theme(plot.title=element_text(size=20,face="bold") ,axis.title=element_text(size=18),axis.text=element_text(size=16))  
  return(p1)
}

framework1=read.csv(paste(mainDirectory, "2-AgregarDadosPorCodeSample/android.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "2-AgregarDadosPorCodeSample/aws.csv", sep = ""), sep=",",header=T)
framework3=read.csv(paste(mainDirectory, "2-AgregarDadosPorCodeSample/azure.csv", sep = ""), sep=",",header=T)
framework4=read.csv(paste(mainDirectory, "2-AgregarDadosPorCodeSample/spring.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2, framework3, framework4)

#### PRs abertas
dataFramework1=framework1$relative_open_count
dataFramework2=framework2$relative_open_count
dataFramework3=framework3$relative_open_count
dataFramework4=framework4$relative_open_count
title = "Relativo Número de PRs\nAbertos"
verticalTitle = "Porcentagem de PRs"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework2_median =  median(unlist(dataFramework2), na.rm = TRUE)
framework3_median =  median(unlist(dataFramework3), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2, frameworkName3, frameworkName4)), relative_open_count)
plotGraphic()
ggsave(paste(mainDirectory, "4-Plots/prs_abertos.pdf", sep = ""), width = 4.5, height = 4.5)


#### Prs fechados
dataFramework1=framework1$relative_closed_count
dataFramework2=framework2$relative_closed_count
dataFramework3=framework3$relative_closed_count
dataFramework4=framework4$relative_closed_count
title = "Relativo Número de PRs\nFechados"
verticalTitle = "Porcentagem de PRs"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework2_median =  median(unlist(dataFramework2), na.rm = TRUE)
framework3_median =  median(unlist(dataFramework3), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2, frameworkName3, frameworkName4)), relative_closed_count)
plotGraphic()
ggsave(paste(mainDirectory, "4-Plots/prs_fechados.pdf", sep = ""), width = 4.5, height = 4.5)

#### PRs mergeadas
dataFramework1=framework1$relative_merged
dataFramework2=framework2$relative_merged
dataFramework3=framework3$relative_merged
dataFramework4=framework4$relative_merged
title = "Relativo Número de PRs\nMergeados"
verticalTitle = "Porcentagem de PRs"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework2_median =  median(unlist(dataFramework2), na.rm = TRUE)
framework3_median =  median(unlist(dataFramework3), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2, frameworkName3, frameworkName4)), relative_merged)
plotGraphic()
ggsave(paste(mainDirectory, "4-Plots/prs_mergeados.pdf", sep = ""), width = 4.5, height = 4.5)


#### Tempo para Fechar
dataFramework1=framework1$tempo.para.fechamento
dataFramework2=framework2$tempo.para.fechamento
dataFramework3=framework3$tempo.para.fechamento
dataFramework4=framework4$tempo.para.fechamento
title = "Tempo de Fechamento de\nPRs"
verticalTitle = "Tempo em Dias"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework2_median =  median(unlist(dataFramework2), na.rm = TRUE)
framework3_median =  median(unlist(dataFramework3), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2, frameworkName3, frameworkName4)), tempo.para.fechamento)
plotGraphic()
ggsave(paste(mainDirectory, "4-Plots/tempo-para-fechar.pdf", sep = ""), width = 4.5, height = 4.5)


#### Tempo para Merge
dataFramework1=framework1$tempo.merge
dataFramework2=framework2$tempo.merge
dataFramework3=framework3$tempo.merge
dataFramework4=framework4$tempo.merge
title = "Tempo Para Merge de\nPR"
verticalTitle = "Tempo em Dias"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework2_median =  median(unlist(dataFramework2), na.rm = TRUE)
framework3_median =  median(unlist(dataFramework3), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2, frameworkName3, frameworkName4)), tempo.merge)
plotGraphic()
ggsave(paste(mainDirectory, "4-Plots/tempo-para-merge.pdf", sep = ""), width = 4.5, height = 4.5)


#### Número de seguidores
dataFramework1=framework1$merge.user.seguidores
dataFramework2=framework2$merge.user.seguidores
dataFramework3=framework3$merge.user.seguidores
dataFramework4=framework4$merge.user.seguidores
title = "Número de Seguidores de\nQuem Faz Merge"
verticalTitle = "Número de Seguidores"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework2_median =  median(unlist(dataFramework2), na.rm = TRUE)
framework3_median =  median(unlist(dataFramework3), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2, frameworkName3, frameworkName4)), merge.user.seguidores)
plotGraphic()
ggsave(paste(mainDirectory, "4-Plots/numero-de-seguidores.pdf", sep = ""), width = 4.5, height = 4.5)


#### Tempo de GitHub Quando Fez o Merge
dataFramework1=framework1$merge.user.tempo.no.GitHub
dataFramework2=framework2$merge.user.tempo.no.GitHub
dataFramework3=framework3$merge.user.tempo.no.GitHub
dataFramework4=framework4$merge.user.tempo.no.GitHub
title = "Tempo de GitHub Quando\nFez o Merge"
verticalTitle = "Tempo em Dias"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework2_median =  median(unlist(dataFramework2), na.rm = TRUE)
framework3_median =  median(unlist(dataFramework3), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2, frameworkName3, frameworkName4)), merge.user.tempo.no.GitHub)
plotGraphic()
ggsave(paste(mainDirectory, "4-Plots/tempo-de-github.pdf", sep = ""), width = 4.5, height = 4.5)
