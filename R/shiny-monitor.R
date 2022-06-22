library(shiny)
library(shinydashboard)

#' Launch instance monitoring app
#'
#' Launches a shiny app for real-time monitoring of one or more EC2 instances
#' and the jobs running on them.
#'
#' This feature requires both the \code{top} and \code{mpstat} programs be
#' installed on the instance.
#'
#' @param instances Either an \code{instance} objected created with
#'   \code{\link{launchInstance}} or \code{\link{getInstanceDescription}} or a
#'   list containing one or more \code{instance} objects.
#' @param updateFrequency Status update frequency (in seconds).  Note: this sets
#'   the delay between update attempts; server lag may increase the effective
#'   update rate.
#'
#' @export
#' 
monitor <- function(instances, updateFrequency = 2) {
  
  if (!is.null(instances$id)) {
    instances <- list(instances)
  }
  names(instances) <- sapply(instances, function(l) { l$id })
  
  shinyApp(ui(instances, updateFrequency), server(instances))
  
}

# Shiny UI and server
ui <- function(instances, updateFrequency) {
  
  # menu items for instances
  instanceMenuItems <- lapply( instances
                             , function(instance) {
                                 menuSubItem(instance$id, tabName = instance$id, icon = NULL)
                               }
                             )
  
  sidebar <- sidebarMenu( menuItem(text = "Instances", icon = icon("server"), startExpanded = TRUE, instanceMenuItems)
                        , sliderInput( inputId = 'updateFrequency'
                                     , label   = "Update frequency (seconds)"
                                     , value   = updateFrequency
                                     , min     = 1
                                     , max     = 60*10
                                     )
                        )
  
  instanceTabs <- lapply( names(instances)
                        , function(id) {
                            tabItem( fluidRow( tabBox( title = tagList(icon("server"), id)
                                                     , width = 12
                                                     , tabPanel(title = "Status", uiOutput(outputId = paste0(id, "-overview")))
                                                     , tabPanel( title = "Load"
                                                               , plotOutput(outputId = paste0(id, "-load"))
                                                               )
                                                     , tabPanel(title = "Details", tableOutput(outputId = paste0(id, "-info")))
                                                     )
                                              )
                                     , tabName = id
                                     )
                          }
                        )
  tabs <- do.call(tabItems, instanceTabs)
  
  dashboardPage( dashboardHeader(title = "AWSTools monitor")
               , dashboardSidebar(sidebar)
               , dashboardBody( tags$head(tags$style(HTML('.recalculating { opacity: 1; }')))
                              , tabs
                              )
               )
}

server <- function(instances) {
  
  for (id in names(instances)) {
    instances[[id]]$session <- ssh::ssh_connect(instances[[id]]$host, instances[[id]]$keyfile)
  }
  
  function(input, output, session) {
    
    store  <- do.call(reactiveValues, instances)
    update <- reactive({
      store$lastUpdate
      for (id in names(instances)) {
        isolate(store[[id]]$lastUpdate <- store$lastUpdate)
        isolate(store[[id]]$running    <- checkAlive(instances[[id]]))
        isolate(store[[id]]$status     <- getStatus( instances[[id]]
                                                   , session = instances[[id]]$session
                                                   )
               )
        isolate({
          store[[id]]$history <- if (is.null(store[[id]]$history)) {
            store[[id]]$status$system$mpstat
          } else {
            rbind(store[[id]]$history, store[[id]]$status$system$mpstat)
          }
          
        })
        
      }
      
      store
    })
    
    # observer run at updateFrequency
    observe({
      isolate(store$lastUpdate <- Sys.time())
      invalidateLater(input$updateFrequency * 1000)
    })

    # setup output bindings
    for (id in names(instances)) {
      
      # overview tab
      output[[paste0(id, "-overview")]] <- renderUI({
        
        instance <- update()[[id]]
        if (instance$running) {
          jobs   <- instance$status$jobs
          system <- instance$status$system
          tagList(
          fluidRow( valueBox( paste0((sum(jobs$finished)), "/", nrow(jobs))
                            , "Tasks completed."
                            , icon  = icon("thumbs-up")
                            , color = "green"
                            , width = 4
                            )
                  , valueBox( paste0(100 - system$cpu.id, "%")
                            , paste0("CPU usage (", nrow(system$mpstat) - 1, " logical cores).")
                            , icon  = icon("microchip")
                            , color = "purple"
                            , width = 4
                            )
                  , valueBox( paste0(round(system$mem.used / system$mem.total, 2), "%")
                            , paste0("Memory usage (", round(system$mem.total / 1024^2, 0), " GB total).")
                            , icon  = icon("memory")
                            , color = "yellow"
                            , width = 4
                            )
                  )
          , br()
          , p(em("Pending jobs:"))
          , tableOutput(outputId = paste0(id, "-jobs"))
          )
          
        } else {
          p("Instance is not running.")
        }
        
      })
      
      output[[paste0(id, "-jobs")]] <- renderTable({
      
        instance <- update()[[id]]
        if (instance$running) {
          dplyr::filter(instance$status$jobs, !finished)
        } else {
          p("Instance is not running.")
        }
        
      })
      
      # cpu load tab
      output[[paste0(id, "-load")]] <- renderUI({
        
        # instance <- update()[[id]]

        
      })
      
      # instance info tab
      output[[paste0(id, "-info")]] <- renderTable({
        
        instance <- update()[[id]]
        data.frame( name  = names(instance)
                  , value = sapply(instance
                                  , function(value) { 
                                      if ( is.numeric(value)   |
                                           is.character(value) |
                                           is.logical(value)   |
                                           any(class(value) == "POSIXct")
                                         ) {
                                        paste0(value, collapse = ", ") 
                                      } else {
                                        "<not displayed>"
                                      }
                                    }
                                  )
                  )
        
      })
      
    }
    
  }
}
  

