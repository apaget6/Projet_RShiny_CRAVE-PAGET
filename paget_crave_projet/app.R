
library(shiny)
library(dplyr)
library(ggplot2)
library(reshape)
library(plotly)
library(data.table)
library(shinydashboard)


#Importation des tables
conso_dep<-fread("conso_dep.csv",header = TRUE)
conso_dep[,c(1:7)]<-lapply(conso_dep[,c(1:7)],as.factor)

conso_reg<-fread("conso_reg.csv",header = TRUE)
conso_reg[,c(1:5)]<-lapply(conso_reg[,c(1:5)],as.factor)

conso_com<-fread("conso_com2.csv",header = TRUE)
conso_com[,c(1:9,12)]<-lapply(conso_com[,c(1:9,12)],as.factor)

prod_dep<-fread("prod_dep.csv",header = TRUE)
prod_dep[,c(1:6)]<-lapply(prod_dep[,c(1:6)],as.factor)

prod_reg<-fread("prod_reg.csv",header = TRUE)
prod_reg[,c(1:4)]<-lapply(prod_reg[,c(1:4)],as.factor)

prod_com<-fread("prod_com2.csv",header = TRUE)
prod_com[,c(1:8,15)]<-lapply(prod_com[,c(1:8,15)],as.factor)


# Definition de l'UI 
ui <- fluidPage(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    # Titre de l'application
    titlePanel(textOutput("Titre")),
    
    sidebarLayout(
        
        # Barre latérale
        sidebarPanel(
            selectInput(inputId = "maille",
                        label = "Choisissez la maille:",
                        choices = c('Commune', 'Département', 'Région'),
                        selected = 'Département'),
            
            ## UI dynamique
            uiOutput(outputId = "ui_dynamique"),
            
            checkboxGroupInput(inputId="Annee",
                               label="Choisissez les années",
                               choices=unique(conso_dep$annee),
                               selected=c(2019, 2020))
        ),
        
        mainPanel(
            # Output: onglets 'résumé' et 'détails'
            tabsetPanel(type = "tabs",
                        tabPanel("Résumé", plotlyOutput("consoBar"), plotlyOutput("prodBar"), 
                                 plotlyOutput("consoPlot"), plotlyOutput("prodPlot"),
                                 valueBoxOutput("consoBox"), valueBoxOutput("prodBox")),
                        
                        tabPanel("Détails", tableOutput("consoTable"), 
                                 downloadButton('consoDownload',"Télécharger la table"), tableOutput("prodTable"),
                                 downloadButton('prodDownload',"Télécharger la table")
                                 )
            )
        )
    )
)

# Définition du serveur

server <- function(input, output) {
    
    # UI dynamique
    output$ui_dynamique<- renderUI({
        if(input$maille == 'Département'){
            collectivite <- unique(conso_dep$nom_departement)
        }else if (input$maille == 'Région'){
            collectivite <- unique(conso_reg$nom_region)
        }else{
            collectivite <- unique(prod_com$nom_commune)
        }
        
        selectInput(inputId = 'collec',
                    label = 'Choissez une collectivité',
                    choices = collectivite,
                    selected = collectivite[1])
        
    })
    
    #Diagramme en barres des consommations par secteur (moyennes des années sélectionnées)
    output$consoBar <- renderPlotly(
        ggplotly(
            if(input$maille == 'Département'){
                ggplot(
                    conso_dep %>% filter(nom_departement == input$collec) %>%
                        filter(annee%in% input$Annee) %>% group_by(code_grand_secteur, annee) %>%
                        summarise(conso_tot_cat_an=sum(`conso_totale_(mwh)`)) %>% 
                        group_by(code_grand_secteur) %>%
                        summarise(conso_moy_cat=mean(`conso_tot_cat_an`)),
                    aes('Moyenne par an sur la période',conso_moy_cat,fill=code_grand_secteur)) +
                    geom_bar(stat="identity")+
                    ylab("Consommation du secteur en MWh") +
                    ggtitle((label <- "Consommations moyennes par secteur"))
            }else if(input$maille == 'Région'){
                ggplot(
                    conso_reg %>% filter(nom_region == input$collec)%>%
                        filter(annee%in% input$Annee)%>%
                        group_by(code_grand_secteur)%>%na.omit()%>%
                        summarise(conso_tot_cat_an=sum(`conso_totale_(mwh)`)) %>% 
                        group_by(code_grand_secteur) %>%
                        summarise(conso_moy_cat=mean(`conso_tot_cat_an`)),
                    aes('Moyenne par an sur la période',conso_moy_cat,fill=code_grand_secteur)) +
                    geom_bar(stat="identity") +
                    ylab("Consommation du secteur en MWh") +
                    ggtitle((label <- "Consommations moyennes par secteur"))
            }else{
                ggplot(
                    conso_com %>% filter(nom_commune == input$collec)%>%
                        filter(annee%in% input$Annee)%>%
                        group_by(code_grand_secteur)%>%na.omit()%>%
                        summarise(conso_tot_cat_an=sum(`conso_totale_(mwh)`)) %>% 
                        group_by(code_grand_secteur) %>%
                        summarise(conso_moy_cat=mean(`conso_tot_cat_an`)),
                    aes('Moyenne par an sur la période',conso_moy_cat,fill=code_grand_secteur)) +
                    geom_bar(stat="identity") +
                    ylab("Consommation du secteur en MWh") +
                    ggtitle((label <- "Consommations moyennes par secteur"))
            }
        )
    )
    
    #Diagramme en barres des productions par secteur (moyennes des années sélectionnées)
    output$prodBar <- renderPlotly(
        ggplotly(
            if(input$maille == 'Département'){
                ggplot(
                    melt(
                        prod_dep %>% filter(nom_departement == input$collec) %>% filter(annee%in% input$Annee)
                        %>% select(annee, contains("prod_")),
                        id = "annee"
                        ) %>% group_by(variable, annee) %>% na.omit() %>% summarise(tot = sum(value)) %>% 
                        group_by(variable) %>% summarise(moyenne = mean(tot)),
                        aes('Moyenne par an sur la période', moyenne, fill = variable)
                        ) +
                        geom_bar(stat = "identity") +
                        ylab("Production du secteur en MWh") +
                        ggtitle((label <- "Productions moyennes par secteur"))
            }else if(input$maille == 'Région'){
                ggplot(
                    melt(
                        prod_reg %>% filter(nom_region == input$collec) %>% filter(annee%in% input$Annee)
                        %>% select(annee, contains("prod_")),
                        id = "annee"
                        ) %>% group_by(variable, annee) %>% na.omit() %>% summarise(tot = sum(value)) %>% 
                        group_by(variable) %>% summarise(moyenne = mean(tot)),
                        aes('Moyenne par an sur la période', moyenne, fill = variable)
                        ) +
                        geom_bar(stat = "identity") +
                        ylab("Production du secteur en MWh") +
                        ggtitle((label <- "Productions moyennes par secteur"))
            }else{
                ggplot(
                    melt(
                        prod_com %>% filter(nom_commune == input$collec) %>% filter(annee%in% input$Annee)
                        %>% select(annee, contains("prod_")),
                        id = "annee"
                        ) %>% group_by(variable, annee) %>% na.omit() %>% summarise(tot = sum(value)) %>% 
                        group_by(variable) %>% summarise(moyenne = mean(tot)),
                        aes('Moyenne par an sur la période', moyenne, fill = variable)
                        ) +
                        geom_bar(stat = "identity") +
                        ylab("Production du secteur en MWh") +
                        ggtitle((label <- "Productions moyennes par secteur"))                
            }
        )
    )
    
    # Graphique de l'évolution des consommations par secteur
    output$consoPlot <- renderPlotly(
        ggplotly(
            if(input$maille == 'Département'){
                ggplot(data = conso_dep  %>% filter(nom_departement == input$collec) %>%
                           filter(annee%in% input$Annee) %>% group_by(annee, code_grand_secteur) %>%
                           summarise(conso_tot_mwh=sum(`conso_totale_(mwh)`)))  + 
                    xlab("Année") +
                    ylab("Consommation du secteur en MWh") +
                    ggtitle((label <- "Evolution des consommations par secteur")) +
                    geom_line(aes(y = conso_tot_mwh, x = as.numeric(annee)+2010, col = code_grand_secteur))
            }else if(input$maille == 'Région'){
                ggplot(data = conso_reg  %>% filter(nom_region == input$collec) %>%
                           filter(annee%in% input$Annee) %>% group_by(annee, code_grand_secteur) %>%
                           summarise(conso_tot_mwh=sum(`conso_totale_(mwh)`)))  + 
                    xlab("Année") +
                    ylab("Consommation du secteur en MWh") +
                    ggtitle((label <- "Evolution des consommations par secteur")) +
                    geom_line(aes(y = conso_tot_mwh, x = as.numeric(annee)+2010, col = code_grand_secteur))
            }else{
                ggplot(data = conso_com  %>% filter(nom_commune == input$collec) %>%
                           filter(annee%in% input$Annee) %>% group_by(annee, code_grand_secteur) %>%
                           summarise(conso_tot_mwh=sum(`conso_totale_(mwh)`)))  + 
                    xlab("Année") +
                    ylab("Consommation du secteur en MWh") +
                    ggtitle((label <- "Evolution des consommations par secteur")) +
                    geom_line(aes(y = conso_tot_mwh, x = as.numeric(annee)+2010, col = code_grand_secteur))                
            }
        )
    )
    
    # Graphique de l'évolution des productions par secteur
    output$prodPlot <- renderPlotly(
        ggplotly(
            if(input$maille == 'Département'){
                ggplot(data = melt(
                    prod_dep %>% filter(nom_departement == input$collec) %>% filter(annee%in% input$Annee)
                    %>% select(annee, contains("prod_")),
                    id = "annee"
                ) %>% na.omit() %>% group_by(annee, variable) %>% summarise(total = sum(value))
                )  + 
                    xlab("Année") +
                    ylab("Production du secteur en MWh") +
                    ggtitle((label <- "Evolution des productions par secteur")) +
                    geom_line(aes(y = total, x = as.numeric(annee)+2010, col = variable))
            }else if(input$maille == 'Région'){
                ggplot(data = melt(
                    prod_reg %>% filter(nom_region == input$collec) %>% filter(annee%in% input$Annee)
                    %>% select(annee, contains("prod_")),
                    id = "annee"
                ) %>% na.omit() %>% group_by(annee, variable) %>% summarise(total = sum(value))
                )  + 
                    xlab("Année") +
                    ylab("Production du secteur en MWh") +
                    ggtitle((label <- "Evolution des productions par secteur")) +
                    geom_line(aes(y = total, x = as.numeric(annee)+2010, col = variable))
            }else{
                ggplot(data = melt(
                    prod_com %>% filter(nom_commune == input$collec) %>% filter(annee%in% input$Annee)
                    %>% select(annee, contains("prod_")),
                    id = "annee"
                ) %>% na.omit() %>% group_by(annee, variable) %>% summarise(total = sum(value))
                )  + 
                    xlab("Année") +
                    ylab("Production du secteur en MWh") +
                    ggtitle((label <- "Evolution des productions par secteur")) +
                    geom_line(aes(y = total, x = as.numeric(annee)+2010, col = variable))                
            }
        )
    )
    
    # Valuebox de la consommation totale (sur les années sélectionnées)
    output$consoBox <- renderValueBox({
        if(input$maille == 'Département'){
            valueBox(
                round(conso_dep %>% filter(nom_departement == input$collec)%>%
                          filter(annee%in% input$Annee)%>%
                          na.omit()%>%
                          summarise(conso_tot=sum(`conso_totale_(mwh)`))),
                "Consommation totale en MWh"
            )
        }else if(input$maille == 'Région'){
            valueBox(
                round(conso_reg %>% filter(nom_region == input$collec)%>%
                          filter(annee%in% input$Annee)%>%
                          na.omit()%>%
                          summarise(conso_tot=sum(`conso_totale_(mwh)`))),
                "Consommation totale en MWh"
            )
        }else{
            valueBox(
                round(conso_com %>% filter(nom_commune == input$collec)%>%
                          filter(annee%in% input$Annee)%>%
                          na.omit()%>%
                          summarise(conso_tot=sum(`conso_totale_(mwh)`))),
                "Consommation totale en MWh"
            )                
        }
    })
    
    # Valuebox de la production totale (sur les années sélectionnées)
    output$prodBox <- renderValueBox({
        if(input$maille == 'Département'){
            valueBox(
                round(melt(
                    prod_dep %>% filter(nom_departement == input$collec) %>% filter(annee%in% input$Annee)
                    %>% select(annee, contains("prod_")),
                    id = "annee"
                ) %>% na.omit() %>% summarise(total = sum(value))),
                "Production totale en MWh"
            )
        }else if(input$maille == 'Région'){
            valueBox(
                round(melt(
                    prod_reg %>% filter(nom_region == input$collec) %>% filter(annee%in% input$Annee)
                    %>% select(annee, contains("prod_")),
                    id = "annee"
                ) %>% na.omit() %>% summarise(total = sum(value))),
                "Production totale en MWh"
            )
        }else{
            valueBox(
                round(melt(
                    prod_com %>% filter(nom_commune == input$collec) %>% filter(annee%in% input$Annee)
                    %>% select(annee, contains("prod_")),
                    id = "annee"
                ) %>% na.omit() %>% summarise(total = sum(value))),
                "Production totale en MWh"
            )            
        }
    })

    # Fonction reactive qui va construire le tableau des consommations
    construit_df_conso <- reactive({
        if(input$maille == 'Département'){
            reg <- conso_dep %>%
                filter(nom_departement == input$collec) %>%
                pull(nom_region)
            reg_from_dep <- toString(reg[1])
            nb_dep <- dim(
                unique(
                    conso_dep %>% filter(nom_region==reg_from_dep) %>% select(nom_departement)
                )
            )[1]
            conso_moy_reg <- conso_dep %>%
                filter(nom_region == reg_from_dep) %>%
                filter(annee%in% input$Annee) %>%
                group_by(code_grand_secteur)%>%
                summarise(conso_moy_reg=sum(`conso_totale_(mwh)`)/nb_dep)
            merge(
                conso_dep %>% filter(nom_departement == input$collec) %>% filter(annee%in% input$Annee) %>%
                    group_by(code_grand_secteur)%>%summarise(conso_collectivite=sum(`conso_totale_(mwh)`)),
                conso_dep %>% filter(annee%in% input$Annee) %>% group_by(code_grand_secteur)%>%
                    summarise(conso_moy_france=sum(`conso_totale_(mwh)`)/length(unique(conso_dep$nom_departement)))
                , by="code_grand_secteur"
            ) %>% 
                merge(conso_moy_reg, by="code_grand_secteur")%>% 
                mutate(ecart_moy_france_pc=round(((conso_collectivite-conso_moy_france)/conso_moy_france*100),2))
        } else if (input$maille == 'Région'){
            merge(
                conso_reg %>% filter(nom_region == input$collec) %>% filter(annee%in% input$Annee) %>%
                    group_by(code_grand_secteur)%>%summarise(conso_collectivite=sum(`conso_totale_(mwh)`)),
                conso_reg %>% filter(annee%in% input$Annee) %>% group_by(code_grand_secteur)%>%
                    summarise(conso_moy_france=sum(`conso_totale_(mwh)`)/length(unique(conso_reg$nom_region))), 
                by="code_grand_secteur"
            ) %>% 
                mutate(ecart_moy_percent=round(((conso_collectivite-conso_moy_france)/conso_moy_france*100),2))
        }else{
            conso_com %>% filter(nom_commune == input$collec) %>% filter(annee%in% input$Annee) %>%
                group_by(code_grand_secteur) %>% na.omit() %>%  summarise(conso_collectivite=sum(`conso_totale_(mwh)`))
        }
    })
    
    # Fonction reactive qui va construire le tableau des productions
    construit_df_prod <- reactive({
        if(input$maille == 'Département'){
            reg <- conso_dep %>%
                filter(nom_departement == input$collec) %>%
                pull(nom_region)
            reg_from_dep<-toString(reg[1])
            nb_dep<-dim(
                unique(
                    conso_dep %>% filter(nom_region==reg_from_dep)%>%select(nom_departement)
                )
            )[1]
            prod_moy_reg<-melt(
                prod_dep %>%
                    filter(nom_region==reg_from_dep)%>%
                    filter(annee%in% input$Annee) %>%
                    select(annee, contains("prod_")),
                id = "annee"
            ) %>%
                group_by(variable)%>% na.omit() %>% summarise(prod_moy_reg = sum(value)/nb_dep)
            merge(
                melt(
                    prod_dep %>%
                        filter(nom_departement == input$collec) %>% 
                        filter(annee%in% input$Annee) %>%
                        select(annee, contains("prod_")),
                    id = "annee"
                ) %>%
                    group_by(variable)%>% na.omit() %>% summarise(prod_collectivite = sum(value)),
                melt(
                    prod_dep %>%
                        filter(annee%in% input$Annee) %>%
                        select(annee, contains("prod_")),
                    id = "annee"
                ) %>%
                    group_by(variable)%>% na.omit() %>% summarise(prod_moy_france = sum(value)/length(unique(prod_dep$nom_departement))),
                by="variable"
            ) %>% 
                merge(prod_moy_reg, by="variable")%>%
                mutate(ecart_moy_france_pc=round(((prod_collectivite-prod_moy_france)/prod_moy_france*100),2))
        }else if(input$maille == 'Région'){
            merge(
                melt(
                    prod_reg %>%
                        filter(nom_region == input$collec) %>% 
                        filter(annee%in% input$Annee) %>%
                        select(annee, contains("prod_")),
                    id = "annee"
                ) %>%
                    group_by(variable) %>% na.omit() %>% summarise(prod_collectivite = sum(value)),
                melt(
                    prod_reg %>%
                        filter(annee%in% input$Annee) %>%
                        select(annee, contains("prod_")),
                    id = "annee"
                ) %>%
                    group_by(variable)%>% na.omit() %>% summarise(prod_moy_france = sum(value)/length(unique(conso_reg$nom_region))),
                by="variable"
            ) %>% 
                mutate(ecart_moy_percent=round(((prod_collectivite-prod_moy_france)/prod_moy_france*100),2))
        }else{
            melt(
                prod_com %>%
                    filter(nom_commune == input$collec) %>% 
                    filter(annee%in% input$Annee) %>%
                    select(annee, contains("prod_")),
                id = "annee"
            ) %>%
                group_by(variable)%>% na.omit() %>% summarise(prod_collectivite = sum(value))
        }
    })    
        
    # Table des consommations par secteur
    output$consoTable <- renderTable({
        construit_df_conso()
    })
    
    # Table des productions par secteur
    output$prodTable <- renderTable({
        construit_df_prod()
    })
    
    # Téléchargement de la table des consommations
    output$consoDownload <- downloadHandler(
        filename = function(){"conso.csv"}, 
        content = function(fname){
            write.csv(construit_df_conso(), fname)
        }
    )
    
    # Téléchargement de la table des productions
    output$prodDownload <- downloadHandler(
        filename = function(){"prod.csv"}, 
        content = function(fname){
            write.csv(construit_df_prod(), fname)
        }
    )
    
    # Titre
    output$Titre <- renderText(input$collec)
}

# Lancement de l'application
shinyApp(ui = ui, server = server)


