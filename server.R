server <- function(input, output, session) {
  # 0. Реактивы                                                             ####
  log_user = reactiveVal(FALSE)
  name_user = reactiveVal()
  params = reactiveVal()
  namber_data = reactiveVal()
  need_loc = reactiveVal("Троицкий округ")
  # 1. Логотип                                                              ####
  #    1.1 Создание логотипа                                                ####
    output$logo <- renderImage({
      list(src = '2.Pic/15.png',
           width = 150,
           vspace = 1,
           hspace = 20
           )
    }, deleteFile = FALSE)
  #    1.2 Изменение в зависимоти от ширины боковой панели                  ####
  observeEvent({input$sidebar},{
    if(input$sidebar == TRUE){
      output$logo <- renderImage({
        list(src = '2.Pic/15.png',
             width = 150,
             vspace = 1,
             hspace = 20
        )
      }, deleteFile = FALSE)
    }else{
      output$logo <- renderImage({
        list(src = '2.Pic/16.png',
             width = 50,
             vspace = 1,
             hspace = 0
        )
      }, deleteFile = FALSE)
    }
  })
    
    
  # 2. Главный экран                                                        ####
  #    2.1 Вкладка Analyst                                                  ####
  output$ui_0 <- renderUI({
    fluidPage(
      fluidRow(
        column(width = 3,tagAppendAttributes(bs4ValueBox(h4(""),width = 12,
                                                         icon = icon("money-bill"),
                                                                  h5('Итого расходов'),
                                                                  color = "white"),id = "invest11")
                                                                   %>%
                 bs_embed_tooltip(title = "Общее количество расходов с учетом всех вводных\nпараметров проекта (от - до)")),
        column(width = 3, tagAppendAttributes(bs4ValueBox(h4(""),width = 12,
                                                          icon = icon("money-bill"),
                                                          h5('Расходы на персонал'),
                                                          color = "danger"),id = "invest12") %>%
                 bs_embed_tooltip(title = "Общее количество расходов на персонал с учетом всех\nвводных параметров проекта (от - до)")),
        column(width = 3, tagAppendAttributes(bs4ValueBox(h4(""),width = 12,
                                                          icon = icon("money-bill"),
                                                          h5('Строительство/Аренда объектов недвижимости'),
                                                          color = "gray-dark"),id = "invest13")%>%
                 bs_embed_tooltip(title = "Общее количество расходов на строительство/аренду объектов недвижимости с учетом всех\nвводных параметров проекта (от - до)")),
        column(width = 3, tagAppendAttributes(bs4ValueBox(h4(""),width = 12,
                                                          icon = icon("money-bill"),
                                                          h5('Налоги'),
                                                          color = "teal"),id = "invest14")%>%
                 bs_embed_tooltip(title = "Общее количество расходов на налоги с учетом всех\nвводных параметров проекта (от - до)")),
        
        
        column(width = 4,tagAppendAttributes(bs4ValueBox(h4(""),width = 12,
                                                         icon = icon("money-bill"),
                                                         h5('Стоимость услуг (выбранных пользователем)'),
                                                         color = "gray-dark"),id = "invest15")
               %>% bs_embed_tooltip(title = "Общее количество расходов на услуги\n(выбранных пользователем на интерфейсе для своего проекта) с учетом всех\nвводных параметров проекта (от - до)")),

        column(width = 4, tagAppendAttributes(bs4ValueBox(h4(""),width = 12,
                                                          icon = icon("money-bill"),
                                                          h5('Пенсионное страхование'),
                                                          color = "white"),id = "invest17")%>%
                 bs_embed_tooltip(title = "Общее количество расходов на пенсионное страхование сотрудников с учетом\nвсех вводных параметров проекта (от - до)")),
        column(width = 4, tagAppendAttributes(bs4ValueBox(h4(""),width = 12,
                                                          icon = icon("money-bill"),
                                                          h5('Медицинское страхование'),
                                                          color = "danger"),id = "invest18")%>%
                 bs_embed_tooltip(title = "Общее количество расходов на медицинское страхование сотрудников с учетом\nвсех вводных параметров проекта (от - до)")),
        
        bs4Card(width = 4, title = "Входные параметры предприятия", collapsible = TRUE, background = 'white',
                maximizable = TRUE,
                dropdownMenu = cardDropdown(
                  cardDropdownItem(
                    span("Изменяйте входные параметры "),
                    br(),
                    span("для точной оценки затрат вашего проекта."),
                    br(),
                    span("Для зарегистрированных пользователей доступен"),
                    br(),
                    span("отчет чтобы его получить зарегистрируйтесь.")
                  ), icon = shiny::icon("info")
                ),
                uiOutput("ui_output0"),
                # downloadButton(outputId = "down_report", label = "Скачать подготовленный отчет",style = button_style),
                selectizeInput(inputId = 'industry',
                               label = 'Отрасль ведения хозяйственной деятельности',
                               choices = industry$industry,
                               selected = industry$industry[1],
                               options = list(create = TRUE,
                                              plugins = list('restore_on_backspace')),
                               width = '100%') %>%
                  bs_embed_tooltip(title = "Выберите отрасль ведения хозяйственной деятельности"),
                
                selectizeInput(inputId = 'org_type',
                               label = 'Тип организации',
                               choices = c("ИП", "ООО"),
                               selected = "ИП",
                               options = list(create = TRUE,
                                              plugins = list('restore_on_backspace')),
                               width = '100%') %>%
                  bs_embed_tooltip(title = "Выберите тип организации"),
                numericInput(inputId= "employees",
                             label = "Штатная численность сотрудников",
                             value = 150,
                             width = '100%'
                  ),
                selectizeInput(inputId = 'tax_type',
                               label = 'Вид налогооблажения',
                               choices = c('УСН' ,'ОСН', 'Патент'),
                               selected = "УСН",
                               options = list(create = TRUE,
                                              plugins = list('restore_on_backspace')),
                               width = '100%') %>%
                  bs_embed_tooltip(title = "Выберите вид налогооблажения"),
                numericInput(inputId= "square",
                             label = "Предполагаемая площадь земельного участка для расположения промышленного производства (в квадратных метрах)",
                             value = 7000,
                             width = '100%'
                ) %>% bs_embed_tooltip(title = "Выберите площадь земельного участка"),
                numericInput(inputId= "square2",
                             label = "Планируемая площадь объектов капитального строительства",
                             value = 4000,
                             width = '100%'
                ) %>% bs_embed_tooltip(title = "Выберите площадь объектов капитального строительства"),
                selectizeInput(inputId = 'machine',
                               label = 'Предпологаемое оборудование',
                               choices = machine$type,
                               selected = machine$type[1],
                               multiple = TRUE,
                               options = list(create = TRUE,
                                              plugins = list('restore_on_backspace')),
                               width = '100%') %>%
                  bs_embed_tooltip(title = "Выберите предпологаемое оборудование"),
                uiOutput("ui_output1"),
                
                radioGroupButtons(inputId= "accounting",
                             label = "Затраты на Бугалтерские услуги",
                             choices  = c("Да", "Нет"),
                             selected = "Да",
                             size = "normal",
                             checkIcon = list(
                               yes = icon("square-check"),
                               no = icon("square")
                             ),
                             status = "warning",
                             width = '100%'),
                selectizeInput(inputId = 'other_services',
                               label = 'Дополнительные услуги',
                               choices = unique(other_services$service),
                               selected = unique(other_services$service)[1],
                               multiple = TRUE,
                               options = list(create = TRUE,
                                              plugins = list('restore_on_backspace')),
                               width = '100%') %>%
                  bs_embed_tooltip(title = "Выберите дополнительные услуги"),
                
                selectizeInput(inputId = 'build_type',
                               label = 'Планируемый тип зданий/сооружений и их площади',
                               choices = c('Жилые здания', 'Общественные здания', 'Промышленные здания', 'Сельскохозяйственные здания'),
                               selected = 'Жилые здания',
                               multiple = TRUE,
                               options = list(create = TRUE,
                                              plugins = list('restore_on_backspace')),
                               width = '100%') %>%
                  bs_embed_tooltip(title = "Выберите планируемый тип зданий/сооружений и их площади"),
                uiOutput("ui_output2")
                
                ),
    
        bs4Card(width = 8, title = "Карта интерактивного выбора округа", collapsible = TRUE, background = 'white',
                maximizable = TRUE,
                dropdownMenu = cardDropdown(
                  cardDropdownItem(
                    span("Вы можете щелкнуть по карте или выбрать округ")
                  ), icon = shiny::icon("info")
                ),
        #         fluidRow(
        #           # column(width = 2,
        #           #        selectizeInput(inputId = 'ad_level_map',
        #           #                       label = 'Choose Admin level',
        #           #                       choices = c("Country","Region", "Municipality", "Districts"),
        #           #                       selected = "Districts",
        #           #                       options = list(create = TRUE,
        #           #                                      plugins = list('restore_on_backspace')),
        #           #                       width = '100%') %>% 
        #           #          bs_embed_tooltip(title = "Choose the level of detail in the map and analytic")
        #           # ),
        #           # column(width = 3,uiOutput("ui_output4")),
        #         ),
                fluidRow(
                  column(width = 3,uiOutput("ui_output3")),
                  column(width = 12,
                         mapdeckOutput(outputId = "map_msk", height  = "800px") %>%
                           withSpinner(type = getOption("spinner.type", default = 6),color = "#FFA500"))
                ))
      )
    )
  })
  # 3. ObserveEventы для поддерржание связнаости между всеми элементами     ####
  # #    3.1 Обновление реактивов локации                                   ####
  observeEvent({input$location_map},priority = 2,{
    if(!is.null(input$location_map)){
      if(input$location_map != ""){
        need_loc(input$location_map)
      }
    }
  })
  #    3.2 Обновление реактивов локации по клипу на карту                   ####
  observeEvent({input$map_msk_polygon_click},priority = 2,{
    exsample <- c(strsplit(input$map_msk_polygon_click, split = '"')[[1]])
    index <- exsample[which(exsample == "tooltip") + 2]
    need_loc(index)
  })
  #    3.2 Обновление реактивов количества оборудования                     ####
  observeEvent(eventExpr = {
    buttons <- paste0("nember_",machine$type)
    list_of_buttons = NULL
    for(var in buttons) {
      list_of_buttons <- append(list_of_buttons, input[[var]])
    }
    list_of_buttons
  },{
    imp_data <- data.table(name = input$machine)
    for (i in imp_data$name) {
      var <- paste0("nember_",i)
      imp_data[name == i,n := input[[var]]]
    }
    namber_data(imp_data)
  })
  # 4. Изменяемый UI                                                        ####
  # #    4.1 Вывод селоктора локаций 1 вкладка                              ####
  output$ui_output3 <- renderUI({
    
    selectizeInput(inputId = 'location_map',
                          label = 'Выберите округ из списка',
                          choices = polygons$name,
                          selected = need_loc(),
                          options = list(create = TRUE,
                                         plugins = list('restore_on_backspace')),
                          width = '100%') %>%
      bs_embed_tooltip(title = "Выберите округ из списка. Он обновлитьтся на карте так же для выбора можете нажимать на карту")
  })
  output$ui_output1 <- renderUI({
    if(!is.null(input$machine)){
      list_ui <- list(NULL)
      n <- 1
      for (i in input$machine) {
        list_ui[[n]] <- sliderInput(inputId = paste0('nember_',i),
                                    label = paste0('Выберите количество для объекта: ', i),
                                    min = 0,
                                    max = 100,
                                    value = 1,
                                    width = '100%',
                                    sep = '')
        n <- n +1
      }
      list_ui
    }
  })
  
  output$ui_output2 <- renderUI({
    if(!is.null(input$build_type)){
      list_ui <- list(NULL)
      n <- 1
      for (i in input$build_type) {
        list_ui[[n]] <- sliderInput(inputId = paste0('sc_',i),
                                    label = paste0('Выберите площать для объекта: ', i),
                                    min = 100,
                                    max = 5000,
                                    value = 150,
                                    width = '100%',
                                    sep = '')
        n <- n +1
      }
      list_ui
    }
  })
  
  output$ui_output0 <- renderUI({
    if(log_user() == TRUE){
      downloadButton(outputId = "down_report", label = "Скачать подготовленный отчет",style = button_style)
    }
  })

  # 5. Карта                                                                ####
  #    5.1 Создание основоной карты                                         ####
     output$map_msk <- renderMapdeck({
         
         map_view_change = c(37.557054,55.591535, 8)
         
         m1 <- grDevices::colorRamp(c("#1F1F1FBB","#FFA500BB","#FFFAEEBB"), alpha = TRUE)((1:12)/12)
         
         df_map1 <- copy(polygons) %>% 
           .[,color := colourvalues::convert_colours(m1)] %>% 
           .[name == need_loc(), color := "#30D5C8FF"]
         
         # l1 <- legend_element(
         #   variables = c("High","Medium", "Low")
         #   , colours = c("#FFFAEE","#FFA500","#1F1F1F")
         #   , colour_type = "fill"
         #   , variable_type = "continuous"
         # )
         # js <- mapdeck_legend(l1)
         
         mapdeck(token = key, style ="mapbox://styles/nikita-burakov/clhyke06p02by01pgg4156c19") %>%
           add_polygon(
             data = df_map1
             , polyline = "geometry"
             , layer = "polygon_layer"
             , fill_colour = "color"
             , stroke_colour = "#000000FF"
             , stroke_width = 300
             , tooltip = "name"
             , auto_highlight = TRUE
             # , palette = m1
             , highlight_colour = "#FFFFFFFF"
               # , legend = js
             ) %>%
           mapdeck_view(
             location = c(map_view_change[1], map_view_change[2]),
             zoom = map_view_change[3])
      
     })
  #    5.2 Обновление ихсодя из изменений                                   ####
     observeEvent(priority = 1,{c(input$location_map)},{
       map_view_change = c(37.557054,55.591535, 8)
       
       m1 <- grDevices::colorRamp(c("#1F1F1FBB","#FFA500BB","#FFFAEEBB"), alpha = TRUE)((1:12)/12)
       
       df_map1 <- copy(polygons) %>% 
         .[,color := colourvalues::convert_colours(m1)] %>% 
         .[name == need_loc(), color := "#30D5C8FF"]
       
       # l1 <- legend_element(
       #   variables = c("High","Medium", "Low")
       #   , colours = c("#FFFAEE","#FFA500","#1F1F1F")
       #   , colour_type = "fill"
       #   , variable_type = "continuous"
       # )
       # js <- mapdeck_legend(l1)
       
       mapdeck_update(map_id = "map_msk") %>% 
         add_polygon(
           data = df_map1
           , polyline = "geometry"
           , layer = "polygon_layer"
           , fill_colour = "color"
           , stroke_colour = "#000000FF"
             , stroke_width = 300
           , tooltip = "name"
           , auto_highlight = TRUE
           # , palette = m1
           , highlight_colour = "#FFFFFFFF"
             # , legend = js
         ) %>%
         mapdeck_view(
           location = c(map_view_change[1], map_view_change[2]),
           zoom = map_view_change[3])
       
     })
  # 7. Инфобоксы                                                            ####
  #    7.1 Заголовок всего стенда                                           ####
  output$texttitle <- renderText({paste0("Расчет инвестиций в промышленное предприятие")})
  
  output$textuser <- renderUI({
    HTML(as.character(paste0(
      span("Информация: ", style = "font-weight: bold; color:#1A1814;"), 
      br(),
      span("ФИО: ", paste0(users_data[email == name_user()]$first_name," ", users_data[email  == name_user()]$second_name," ", users_data[email  == name_user()]$last_name)),
      br(),
      span("Компания: ", users_data[email  == name_user()]$company),
      br(),
      span("ИНН: ", users_data[email  == name_user()]$inn),
      br(),
      span("Страна: ", users_data[email  == name_user()]$country),
      br(),
      span("Город: ", users_data[email  == name_user()]$city),
      br(),
      span("Должность: ", users_data[email  == name_user()]$job),
      br(),
      br()
    )))})
  #    7.2 Инфо текст встречающий пользователя                              ####
  output$ui_output_info_text <- renderUI({
    HTML(as.character(paste0(
      span("Excerpt about the chosen location: ", style = "font-size: 16px; font-weight: bold; color:#1A1814;"),
      span(need_loc(), style = "font-size: 16px; color:#1A1814;"),
      br(),
      br(),
      span("Population: ", style = "font-size: 16px; font-weight: bold; color:#1A1814;"),
      span(paste0(format(round(all_data_forecast %>% 
                          .[indicator == "C013" & reg_coded == need_loc_code()] %>% 
                          .[!is.na(fact)] %>% 
                          .[.N] %>% 
                          .[,c("fact")] %>% deframe()),format="d", big.mark=" ",  scientific = FALSE)), style = "font-size: 16px; color:#1A1814;"),
      br(),
      br(),
      span("GDP: ", style = "font-size: 16px; font-weight: bold; color:#1A1814;"),
      span(paste0(format(round(all_data_forecast %>% 
                          .[indicator == "C707" & reg_coded == need_loc_code()] %>% 
                          .[!is.na(fact)] %>% 
                          .[.N] %>% 
                          .[,c("fact")] %>% deframe()),format="d", big.mark=" ",  scientific = FALSE)), style = "font-size: 16px; color:#1A1814;"),
      br(),
      br(),
      span("City budget expenditures: ", style = "font-size: 16px; font-weight: bold; color:#1A1814;"),
      span(paste0(format(round(all_data_forecast %>% 
                          .[indicator == "C065" & reg_coded == need_loc_code()] %>% 
                          .[!is.na(fact)] %>% 
                          .[.N] %>% 
                          .[,c("fact")] %>% deframe()),format="d", big.mark=" ",  scientific = FALSE)), style = "font-size: 16px; color:#1A1814;"),
      br(),
      br(),
      span("Some facts (Chat GPT): ", style = "font-size: 16px; font-weight: bold; color:#1A1814;"),
      span(GPT_opinion(), style = "font-size: 16px; color:#1A1814;")
    )))
    
    # br(), #hr()
    # tags$li("ГИС ТЭК", style = "color:#1A1814;")
  })
  #    7.3 Демографический текст                                            ####
  # output$ui_output_info_text2 <- renderUI({
  #   HTML(as.character(paste0(
  #     span("Distribution of the population into age groups in: ", style = "font-size: 16px; font-weight: bold; color:#1A1814;"),
  #     span(need_loc(), style = "font-size: 16px; color:#1A1814;"),
  #     br()
  #   )))
  #   
    # br(), #hr()
    # tags$li("ГИС ТЭК", style = "color:#1A1814;")
  # })
  #    7.9 Текст для бокса инвестиций poten_invest                          ####
  observeEvent({c(input$location_map, input$industry, namber_data(), input$org_type, input$employees, input$tax_type, input$square, input$square2, input$accounting, input$other_services)},{
  # observeEvent({input$location_map},{
  
    
    industry <- input$industry
    if(industry == ''){industry = NULL}
    
    location <- list_data1$cadastr_price %>% 
      .[input$location_map, on = 'district_name'] %>% 
      .$district
    
    if(is.null(location)){location <- 'ТАО'}
    
    if(is.null(namber_data())){
      machine <- ''
      number_machines <- 0
    } else {
      machine <- namber_data()$name
      number_machines <- namber_data()$n
    }

    out <-
      calc$calculateInvestments(
        industry = industry,
        org_type = input$org_type,
        number_employees = input$employees,
        district_name = location,
        tax_type = input$tax_type,
        land_area = input$square,
        building_area = input$square2,
        machine = machine,
        number_machines = number_machines,
        accounting = ifelse(input$accounting == 'Да', TRUE, FALSE),
        other_services = input$other_services
      )
    # browser()
    # out <- qread("out")
    params(out)
    if(class(out)[1] == 'character'){
      sendSweetAlert(
        session = session,
        title =  '',
        btn_labels = "Хорошо",
        btn_colors = "#FFA500",
        text = tags$div(h3(out)),
        type = "error",
        width = "20%"
      )
    } else {
    
    session$sendCustomMessage("anim", list(id = "invest11", value = round( out$total_costs_min, digits = 1), value2 =  round(out$total_costs_max, digits = 1), units = " млн.руб")) # Все затраты
  # })
  # observeEvent({input$location_map},{
    session$sendCustomMessage("anim", list(id = "invest12", value = round( out$total_staff_costs_min, digits = 1), value2 =  round(out$total_staff_costs_max, digits = 1), units = " млн.руб")) # Затраты на персонал
  # })
  # observeEvent({input$location_map},{
    session$sendCustomMessage("anim", list(id = "invest13", value = round( out$building_costs_min, digits = 1), value2 =  round(out$building_costs_max, digits = 1), units = " млн.руб")) # Строительство/Аренда объектов недвижимости
  # })
  # observeEvent({input$location_map},{
    session$sendCustomMessage("anim", list(id = "invest14", value = round( out$taxes_min, digits = 1), value2 =  round(out$taxes_max, digits = 1), units = " млн.руб")) # Налоги
  # })
  # 
  # 
  # observeEvent({input$location_map},{
    session$sendCustomMessage("anim2", list(id = "invest15", value = round( out$services_costs, digits = 1), units = " млн.руб")) # Стоимость услуг
  # })
  # observeEvent({input$location_map},{
    session$sendCustomMessage("anim", list(id = "invest17", value = round( out$CPI_min, digits = 1), value2 =  round(out$CPI_max, digits = 1), units = " млн.руб")) # Пенсионное страхование
  # })
  # observeEvent({input$location_map},{
    session$sendCustomMessage("anim", list(id = "invest18", value = round( out$CHI_min, digits = 1), value2 =  round(out$CHI_max, digits = 1), units = " млн.руб")) # Медицинское страхование
    
    }
  })
  
  # 8. Кнопки                                                               ####
  #    8.1 Кнопка вызова помощи на странице                                 ####
  observeEvent({input$help_button},{
    if(input$sbMenu == "analyst"){
      showModal(modalDialog(
        p("Добро пожаловать на страницу описания сайта!", style = "color:#1A1814;"),
        br(),
        h5("Стенд разроботан командой DigitalTwinTeam", style = "color:#0098C5;",onclick ="location.href='https://dtwin.ru';"),
        br(),
        p("Стенд предназначен для решения задачи оценки затрат в промышленное предприятие в городе Москва в рамках хакатона. Для стимулирования создания новых предприятий, развития существующих промышленных производств и увеличения числа потенциальных инвесторов в городе Москва было необходимо создание инструмента, который позволил бы быстро и качественно рассчитать объем требуемых вложений. В этой связи был разработан новый цифровой сервис, который в автоматическом режиме осуществляет необходимые расчеты и к которому будут иметь доступ заинтересованные в развитии инвестиционной и промышленной политики города Москва лица", style = "color:#1A1814;"),
        br(),
        p("Проект является начальной стадией более большой задачи. Идея состоит в том, что анализируя затраты(этот стенд) можно понять также и возврат инвестиций и подобрать оптимальные параметры для каждого конкретного предприятия, чтобы получить наибольшую эффективность вложений.", style = "color:#1A1814;"),
        br(),
        p("Следующий этап развития сервиса заключается в управлении множеством инвестиционных проектов формируя портфель и решать задачи распределении денежных потоков по портфелю с учетом оценки вероятности реализации каждого конкретного проекта", style = "color:#1A1814;"),
        br(),
        p("Первое что вы видите это инфобоксы со искомыми значениями. Значения динамические и пересчитываются каждый раз, когда пользователь меняет входные параметры. У каждого инфобокса есть подсказка, которая появляется при наведении на соответствующий раздел.", style = "color:#1A1814;"),
        br(),
        p("Ниже по странице находится меню, где пользователь может менять входные данные. Значения могут: выбираться из выпадающего меню, вписываться пользователем с клавиатуры, изменяться с помощью ползунка, отмечаться на интерактивной карте (об этом ниже).", style = "color:#1A1814;"),
        br(),
        p("Интерактивная карта позволяет нажать на район города Москва или выбрать его из выпадающего списка здесь же на карте. Значения будут пересчитываться.", style = "color:#1A1814;"),
        br(),
        p("Зарегистрированный пользователь может скачать полный отчет в виде .docx файла. Что позволит пользователи редактировать отчет", style = "color:#1A1814;"),
        br(),
        p("В правом верхнем углу можно зарегистрироваться, войти в личный кабинет, оставить обратную связь и найти ссылку на сайт нашей организации. Регистрация и вход осуществляются нажатием на кнопку “Войти”", style = "color:#1A1814;"),
        br(),
        p("При регистрации поля ФИО, email, ИНН и пароль являются обязательными.", style = "color:#1A1814;"),
        br(),
        p("После входа есть возможность скачать подготовленный отчет по введенным данным. Кнопка “Скачать подготовленный отчет” находится в модуле для ввода данных", style = "color:#1A1814;"),
        br(),
        p("После входа есть возможность скачать подготовленный отчет по введенным данным. Кнопка “Скачать подготовленный отчет” находится в модуле для ввода данных", style = "color:#1A1814;"),
        br(),
        p("Мы надеемся, что эта страница была полезной для предоставления обзора проекта. Спасибо за посещение!", style = "color:#1A1814;"),
        title = tags$p(paste0("Инструкции по работе со стендом"), style = "color:#1A1814; font-weight: bold;"),
        footer = modalButton("Понятно"),
        size = 'l',
        easyClose = TRUE
      ))}
  })
  #    8.3 Кнопка вызова формы обратной связи                               ####
  observeEvent({input$contact},{
    showModal(modalDialog(
      div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px; font-family: Panton;",
          wellPanel(
            tags$h2("Форма обратной связи", class = "text-center", 
                    style = "padding-top: 0;color:#0F0F0F; font-weight:600; font-family: Panton;"),
            textInput("feedbackName", placeholder="Ваше имя", label = tagList(icon("user"), "Имя")),
            textInput("feedbackcontact", placeholder="Укажите ваши контактные данные", label = tagList(icon("address-book"), "Email/Telegram/другое")),
            textAreaInput("feedbacktext", placeholder="Сообщение", label = tagList(icon("pen"), "Сообщение")),
            br(),
            div(
              style = "text-align: center;",
              actionButton("send", 
                           "Отправить", 
                           style = button_style),
              br()))), 
      footer = modalButton("Отмена")))
  })
  #    8.4 Кнопка отпраквки обратной связи                                  ####
  observeEvent(priority = 3,{input$send},{
    if(input$feedbackName != ""){
      if(input$feedbackcontact != ""){
        if(input$feedbacktext != ""){
          MobiDickMessage(chat_id = "232289398", text = paste0("Feedback from site Dtwin.city","\n",
                                                               "Name: ",input$feedbackName,"\n",
                                                               "Contact: ",input$feedbackcontact,"\n",
                                                               "Text: ",input$feedbacktext,"\n"))
          sendSweetAlert(
            session = session,
            title =  "Отправленно",
            btn_labels = "Хорошо",
            btn_colors = "#FFA500",
            text = tags$div(h6(paste0("Спасибо, что прислали свой отзыв, он очень важен для нас. Мы делаем все возможное, чтобы сделать обслуживание максимально удобным."))),
            type = "success",
            width = "20%"
          )
          removeModal(session = getDefaultReactiveDomain())
        }else{
          sendSweetAlert(
            session = session,
            title =  "Вы не заполнили текст отзыва",
            btn_labels = "Хорошо",
            btn_colors = "#FFA500",
            text = tags$div(h6("Пожалуйста, напишите текст вашего отзыва")),
            type = "error",
            width = "20%"
          )
        }}else{
          sendSweetAlert(
            session = session,
            title =  "Вы не оставили никаких контактных данных",
            btn_labels = "Хорошо",
            btn_colors = "#FFA500",
            text = tags$div(h6("Пожалуйста, напишите свои контактные данные")),
            type = "error",
            width = "20%"
          )
        }
    }else{
      sendSweetAlert(
        session = session,
        title =  "Вы не заполнили имя",
        btn_labels = "Хорошо",
        btn_colors = "#FFA500",
        text = tags$div(h6("Пожалуйста, напишите свое имя")),
        type = "error",
        width = "20%"
      )
    }
  })
  #    8.5 Кнопка отпраквки отчета                                          ####
  # observeEvent({input$down_report},priority = 5,{
  #   if(log_user() == TRUE){
  #     docx_rmd <- paste0('---\ntitle: \"ОТЧЕТ\nО ВОЗМОЖНЫХ ЗАТРАТАХ НА ЗАПУСК ПРОМЫШЛЕННОГО ПРОИЗВОДСТВА В ГОРОДЕ МОСКВЕ\"\noutput:\n  word_document:\n    reference_docx: report_reverence.docx\n---\n\n```{r setup, include=FALSE}\nknitr::opts_chunk$set(echo = TRUE)\n```\n\n# ПРИВЕТСТВЕННОЕ СЛОВО\n\nСпасибо, что воспользовались электронным сервисом\\\n«Инвестиционный калькулятор города Москвы»!\\\nМы надеемся, что предоставленная возможность предварительного расчета расходов на содержание персонала организации, размещение промышленных объектов на территории города Москвы, а также необходимые регистрационные и прочие услуги была полезна для Вас.\\\nСегодня Москва является ведущим промышленным регионом России, где работает более 3 500 производственных предприятий, продукция которых экспортируется в 186 стран мира. Правительство Москвы предоставляет более 150 мер государственной поддержки промышленным организациям, с которыми можно подробно ознакомиться и подать соответствующую заявку в специализированном разделе Инвестиционного портала города Москвы.\\\nВы также можете получить льготные условия для ведения промышленной деятельности, получив статус резидента особой экономической зоны «Технополис Москва».\\\nМы убеждены, что вместе с Вами сможем открыть новые горизонты современной отечественной промышленности!\n\n##### ОБЗОР ПРЕДВАРИТЕЛЬНЫХ РАСХОДОВ\n\n# ИНФОРМАЦИЯ О ВАШЕЙ ОРГАНИЗАЦИИ\n#### ОТРАСЛЬ: ',
  #                        input$industry,
  #                        '\n#### ТИП ОРГАНИЗАЦИИ: ', input$org_type,
  #                        '\n#### КОЛИЧЕСТВО СОТРУДНИКОВ: ',input$employees,' человек',
  #                        '\n#### РАЙОН РАСПОЛОЖЕНИЯ ПРОИЗВОДСТВА: ', input$location_map,'\n#### \n#### \n#### \n\n# ИТОГОВЫЕ ЗНАЧЕНИЯ ВОЗМОЖНЫХ ЗАТРАТ\n#### ИТОГО ВОЗМОЖНЫХ РАСХОДОВ:',
  #                        ' от ',round(params()$total_costs_min, digits = 1),' до ',round(params()$total_cost_max, digits = 1),' млн.руб',
  #                        '\n#### ПЕРСОНАЛ:',' от ',round(params()$total_staff_costs_min, digits = 1),' до ',round(params()$total_staff_costs_max, digits = 1),' млн.руб',
  #                        '\n#### АРЕНДА:',' от ',round(params()$building_costs_min, digits = 1),' до ',round(params()$building_costs_max, digits = 1),' млн.руб',
  #                        '\n#### НАЛОГИ:',' от ',round(params()$taxes_min, digits = 1),' до ',round(params()$taxes_max, digits = 1),' млн.руб',
  #                        '\n#### УСЛУГИ:',round(params()$services_costs, digits = 1),' млн.руб',
  #                        '\n#### \n#### \n#### \n#### \n#### \n#### \n\n\n##### ОБЗОР ПРЕДВАРИТЕЛЬНЫХ РАСХОДОВ\n\n# ПЕРСОНАЛ ОРГАНИЗАЦИИ\n#### Москва является лидирующим регионом Российской Федерации с наибольшим количеством экономически активного населения.Уровень безработицы в столице – один из самых низких по стране и составляет менее 1%. Специалисты многих направлений заняты в самых разных областях деятельности: автомобилестроение, пищевая промышленность, приборостроение, станкоинструментальная промышленность, легкая промышленность и другие. Правительство Москвы способствует развитию новых специальностей и компетенций.Благодаря проекту «Московская техническая школа» Вы можете подать заявку на обучение своих сотрудников необходимым профессиональным навыкам.\n\n#### \n#### \n#### \n\n#### ИТОГО ВОЗМОЖНЫХ РАСХОДОВ НА СОДЕРЖАНИЕ ПЕРСОНАЛА ОРГАНИЗАЦИИ:',
  #                        'от расходы на персонал',
  #                        '\n#### ПЛАНИРУЕМАЯ ЧИСЛЕННОСТЬ ПЕРСОНАЛА:',input$employees,' человек',
  #                        '\n#### СТРАХОВЫЕ ВЗНОСЫ (ПЕНСИОННОЕ СТРАХОВАНИЕ):',' от ',round(params()$CPI_min, digits = 1),' до ',round(params()$CPI_max, digits = 1),' млн.руб',
  #                        '\n#### СТРАХОВЫЕ ВЗНОСЫ (МЕДИЦИНСКОЕ СТРАХОВАНИЕ):',' от ',round(params()$CHI_min, digits = 1),' до ',round(params()$CHI_max, digits = 1),' млн.руб'
  #     )
  #     
  #     write_file(x = docx_rmd,file = "3.Data/report/report_temp.Rmd")
  #     rmarkdown::render("3.Data/report/report_temp.Rmd",output_dir = "3.Data/report/",output_file = paste0("Report_avto.docx"))
  #   }else{
  #     sendSweetAlert(
  #       session = session,
  #       title =  "Ошибка",
  #       btn_labels = "Хорошо",
  #       btn_colors = "#FFA500",
  #       text = tags$div(h6("Скачивание возможно только для зарегистрированных пользователей")),
  #       type = "error",
  #       width = "20%"
  #     )
  #   }
  # })
  
  output$down_report <- downloadHandler(
    filename = function() {
      paste0("3.Data/report/04.DTT.full_report.docx")
    },
    content = function(file) {
      if(log_user() == TRUE){
        docx_rmd <- paste0('---\ntitle: \"ОТЧЕТ\nО ВОЗМОЖНЫХ ЗАТРАТАХ НА ЗАПУСК ПРОМЫШЛЕННОГО ПРОИЗВОДСТВА В ГОРОДЕ МОСКВЕ\"\noutput:\n  word_document:\n    reference_docx: report_reverence.docx\n---\n\n```{r setup, include=FALSE}\nknitr::opts_chunk$set(echo = TRUE)\n```\n\n# ПРИВЕТСТВЕННОЕ СЛОВО\n\nСпасибо, что воспользовались электронным сервисом\\\n«Инвестиционный калькулятор города Москвы»!\\\nМы надеемся, что предоставленная возможность предварительного расчета расходов на содержание персонала организации, размещение промышленных объектов на территории города Москвы, а также необходимые регистрационные и прочие услуги была полезна для Вас.\\\nСегодня Москва является ведущим промышленным регионом России, где работает более 3 500 производственных предприятий, продукция которых экспортируется в 186 стран мира. Правительство Москвы предоставляет более 150 мер государственной поддержки промышленным организациям, с которыми можно подробно ознакомиться и подать соответствующую заявку в специализированном разделе Инвестиционного портала города Москвы.\\\nВы также можете получить льготные условия для ведения промышленной деятельности, получив статус резидента особой экономической зоны «Технополис Москва».\\\nМы убеждены, что вместе с Вами сможем открыть новые горизонты современной отечественной промышленности!\n\n##### ОБЗОР ПРЕДВАРИТЕЛЬНЫХ РАСХОДОВ\n\n# ИНФОРМАЦИЯ О ВАШЕЙ ОРГАНИЗАЦИИ\n#### ОТРАСЛЬ: ',
                           input$industry,
                           '\n#### ТИП ОРГАНИЗАЦИИ: ', input$org_type,
                           '\n#### КОЛИЧЕСТВО СОТРУДНИКОВ: ',input$employees,' человек',
                           '\n#### РАЙОН РАСПОЛОЖЕНИЯ ПРОИЗВОДСТВА: ', input$location_map,'\n#### \n#### \n#### \n\n# ИТОГОВЫЕ ЗНАЧЕНИЯ ВОЗМОЖНЫХ ЗАТРАТ\n#### ИТОГО ВОЗМОЖНЫХ РАСХОДОВ:',
                           ' от ',round(params()$total_costs_min, digits = 1),' до ',round(params()$total_costs_max, digits = 1),' млн.руб',
                           '\n#### ПЕРСОНАЛ:',' от ',round(params()$total_staff_costs_min, digits = 1),' до ',round(params()$total_staff_costs_max, digits = 1),' млн.руб',
                           '\n#### АРЕНДА:',' от ',round(params()$building_costs_min, digits = 1),' до ',round(params()$building_costs_max, digits = 1),' млн.руб',
                           '\n#### НАЛОГИ:',' от ',round(params()$taxes_min, digits = 1),' до ',round(params()$taxes_max, digits = 1),' млн.руб',
                           '\n#### УСЛУГИ: ',round(params()$services_costs, digits = 1),' млн.руб',
                           '\n#### \n#### \n#### \n#### \n#### \n#### \n\n\n##### ОБЗОР ПРЕДВАРИТЕЛЬНЫХ РАСХОДОВ\n\n# ПЕРСОНАЛ ОРГАНИЗАЦИИ\n#### Москва является лидирующим регионом Российской Федерации с наибольшим количеством экономически активного населения.Уровень безработицы в столице – один из самых низких по стране и составляет менее 1%. Специалисты многих направлений заняты в самых разных областях деятельности: автомобилестроение, пищевая промышленность, приборостроение, станкоинструментальная промышленность, легкая промышленность и другие. Правительство Москвы способствует развитию новых специальностей и компетенций.Благодаря проекту «Московская техническая школа» Вы можете подать заявку на обучение своих сотрудников необходимым профессиональным навыкам.\n\n#### \n#### \n#### \n\n#### ИТОГО ВОЗМОЖНЫХ РАСХОДОВ НА СОДЕРЖАНИЕ ПЕРСОНАЛА ОРГАНИЗАЦИИ:',
                           ' от ',round(params()$total_staff_costs_min, digits = 1),' до ',round(params()$total_staff_costs_max, digits = 1),' млн.руб',
                           '\n#### ПЛАНИРУЕМАЯ ЧИСЛЕННОСТЬ ПЕРСОНАЛА:',input$employees,' человек',
                           '\n#### СТРАХОВЫЕ ВЗНОСЫ (ПЕНСИОННОЕ СТРАХОВАНИЕ):',' от ',round(params()$CPI_min, digits = 1),' до ',round(params()$CPI_max, digits = 1),' млн.руб',
                           '\n#### СТРАХОВЫЕ ВЗНОСЫ (МЕДИЦИНСКОЕ СТРАХОВАНИЕ):',' от ',round(params()$CHI_min, digits = 1),' до ',round(params()$CHI_max, digits = 1),' млн.руб',
                           '\r\n#### \r\n#### \r\n\r\n##### ОБЗОР ПРЕДВАРИТЕЛЬНЫХ РАСХОДОВ\r\n\r\n# АРЕНДА\r\n#### В 2020 году на рынке аренды коммерческих помещений в Москве наблюдался спад из-за пандемии COVID-19 и ограничений, связанных с карантином. Средняя цена аренды офисных помещений в Москве снизилась на 10-15%, а цены на аренду торговых помещений упали на 20-30%. Однако, в 2021 году наблюдается постепенное восстановление рынка аренды коммерческих помещений в Москве. Согласно отчетам, цены на аренду офисных помещений в Москве в первом квартале 2021 года выросли на 1,5-2%, а цены на аренду торговых помещений остались на уровне 2020 года. В целом, рынок аренды коммерческих помещений в Москве остается одним из самых дорогих и перспективных в России.\r\n\r\n#### \r\n#### \r\n#### \r\n\r\n#### ИТОГО ВОЗМОЖНЫХ РАСХОДОВ НА АРЕНДУ ПОМОЩЕНИЙ: ',
                           ' от ',round(params()$building_costs_min, digits = 1),' до ',round(params()$building_costs_max, digits = 1),' млн.руб',
                           '\n#### В том числе:', paste0(input$build_type, collapse=", "),
                           '\r\n\r\n#### \r\n#### \r\n#### \r\n#### \r\n#### \r\n#### \r\n#### \r\n\r\n##### ОБЗОР ПРЕДВАРИТЕЛЬНЫХ РАСХОДОВ\r\n\r\n# НАЛОГОВЫЕ ОТЧИСЛЕНИЯ\r\n#### В 2021 году в Москве введены новые налоговые льготы для предпринимателей, направленные на поддержку малого и среднего бизнеса. В частности, с 1 января 2021 года введена льгота по налогу на имущество для предприятий, занимающихся производством товаров на территории Москвы. Также введены льготы по налогу на прибыль для предприятий, осуществляющих инвестиционную деятельность в Москве. В целом, Москва является одним из наиболее благоприятных регионов для создания и развития бизнеса в России, благодаря высокому уровню инфраструктуры, доступности квалифицированных кадров и наличию различных налоговых льгот для предпринимателей.\r\n\r\n#### \r\n#### \r\n#### \r\n#### \r\n\r\n#### ИТОГО ВОЗМОЖНЫХ РАСХОДОВ НА НАЛОГОВЫЕ ОТЧИСЛЕНИЯ: ',
                           ' от ',round(params()$taxes_min, digits = 1),' до ',round(params()$taxes_max, digits = 1),' млн.руб',
                           '\r\n\r\n#### \r\n#### \r\n#### \r\n#### \r\n####\r\n\r\n##### ОБЗОР ПРЕДВАРИТЕЛЬНЫХ РАСХОДОВ\r\n\r\n# УСЛУГИ\r\n#### Москва предоставляет ряд преимуществ и услуг для бизнеса при создании предприятия. В частности, в Москве действует единое окно для предоставления государственных и муниципальных услуг, что упрощает процедуру регистрации и создания предприятия. Также в Москве действует программа поддержки малого и среднего бизнеса, которая включает в себя финансовую поддержку, консультации и обучение предпринимателей. В 2021 году в Москве введены новые меры поддержки бизнеса, направленные на снижение административных барьеров и упрощение процедур. В частности, введена услуга по выдаче электронных сертификатов для предпринимателей, что позволяет сократить время на получение разрешительной документации.\r\n\r\n#### \r\n#### \r\n#### \r\n#### \r\n\r\n#### ИТОГО ВОЗМОЖНЫХ РАСХОДОВ НА УСЛУГИ: ',
                           round(params()$services_costs, digits = 1),' млн.руб',
                           '\r\n\r\n#### \r\n#### \r\n####\r\n#### \r\n####\r\n####\r\n\r\n##### ОБЗОР РЕСУРСОВ\r\n\r\n# ОБЩЕЕ ОПИСАНИЕ\r\n#### В Москве промышленным организациям предоставляется адресная государственная поддержка в рамках программы \"Развитие промышленности и повышение ее конкурентоспособности в городе Москве\". В рамках этой программы предприятиям предоставляются субсидии на развитие производства, модернизацию оборудования, повышение квалификации персонала и другие меры поддержки.В 2021 году на реализацию программы \"Развитие промышленности и повышение ее конкурентоспособности в городе Москве\" выделено более 5 миллиардов рублей. В рамках этой программы планируется поддержать более 200 промышленных предприятий в Москве. В целом, адресная государственная поддержка промышленным организациям в Москве является одной из важных мер поддержки бизнеса и способствует развитию промышленности в городе.\r\n\r\n#### \r\n#### \r\n#### \r\n#### \r\n\r\n[Инвестиционный портала города Москвы](https://investmoscow.ru/online-services/navigator-support-measures)\\\r\n[Технополис Москва](https://technomoscow.ru/)\\\r\n[Московская техническая школа](http://moscowtechschool.ru/)\r\n\r\n#### \r\n\r\n##### ЗАКЛЮЧЕНИЕ\r\n\r\n# ЗАКЛЮЧИТЕЛЬНОЕ СЛОВО\r\n#### В заключение, инвестирование в промышленное предприятие в городе Москва является перспективным и выгодным решением для бизнеса. Благодаря высокому уровню инфраструктуры, доступности квалифицированных кадров и наличию различных программ поддержки, Москва предоставляет отличные условия для развития промышленности и бизнеса в целом. Инвестирование в промышленное предприятие в Москве может стать успешным и долгосрочным проектом, который принесет высокую доходность и укрепит позиции бизнеса на рынке.'
        )
        
        write_file(x = docx_rmd,file = "3.Data/report/report_temp.Rmd")
        rmarkdown::render("3.Data/report/report_temp.Rmd",output_dir = "3.Data/report/",output_file = paste0("04.DTT.full_report.docx"))
        
        
        doc <- read_docx("3.Data/report/04.DTT.full_report.docx")
        print(doc, target = file)
      }else{
        
        docx_rmd <- paste0('---\ntitle: \"ОТЧЕТ\nО ВОЗМОЖНЫХ ЗАТРАТАХ НА ЗАПУСК ПРОМЫШЛЕННОГО ПРОИЗВОДСТВА В ГОРОДЕ МОСКВЕ ДОСТУПЕН ТОЛЬКО ДЛЯ ЗАРЕГЕСТРИРОВАННЫХ ПОЛЬЗОВАТЕЛЕЙ\"\noutput:\n  word_document:\n    reference_docx: report_reverence.docx\n---\n\n```{r setup, include=FALSE}\nknitr::opts_chunk$set(echo = TRUE)\n```')
        
        write_file(x = docx_rmd,file = "3.Data/report/report_temp.Rmd")
        rmarkdown::render("3.Data/report/report_temp.Rmd",output_dir = "3.Data/report/",output_file = paste0("04.DTT.full_report.docx"))
        
        
        sendSweetAlert(
          session = session,
          title =  "Ошибка",
          btn_labels = "Хорошо",
          btn_colors = "#FFA500",
          text = tags$div(h6("Скачивание возможно только для зарегистрированных пользователей")),
          type = "error",
          width = "20%"
        )
      }
    }
  )
  
  
  
  
  
  # 11. Авторизация                                                         ####
  #    11.1 UI аккаутна                                                     ####
    output$user <- bs4Dash::renderUser({
      if(log_user() == TRUE){
        bs4Dash::dashboardUser(
          name = name_user(),
          image = 'https://static.tildacdn.com/tild6463-3039-4036-b837-643530393136/Screenshot_1.jpg',
          title = name_user(),
          subtitle = "Пользователь",
          fluidRow(
            column(width = 12,htmlOutput("textuser")),
            column(width = 12,actionButton("out", "Выйти",status = "danger", flat = TRUE,width = "150px", 
                                          style = button_style))
          )
        )
      }else{
        bs4Dash::dashboardUser(
          name = "Войти",
          image = 'https://static.tildacdn.com/tild6463-3039-4036-b837-643530393136/Screenshot_1.jpg',
          title = "Гость",
          subtitle = "Войдите или зарегистрируйтесь чтобы получить отчет",
          fluidPage(
          fluidRow(
            column(width = 8,actionButton("regis", "Зарегистироваться",status = "danger", flat = TRUE,
                                          style = button_style)),
            column(width = 4,actionButton("log", "Войти",status = "danger", flat = TRUE, width = "80px",
                                          style = button_style))
          ))
        )
      }

    })
  #    11.2 Форма входа в аккаунт                                           ####
    observeEvent({input$log},{
      showModal(modalDialog(
        div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px; font-family: Panton;",
            wellPanel(
              tags$h2("Вход", class = "text-center", 
                      style = "padding-top: 0;color:#0F0F0F; font-weight:600; font-family: Panton;"),
              textInput("userName", placeholder="email", label = tagList(icon("user"), "Email")),
              tags$style(type="text/css", "#userName {color : red;}"),
              passwordInput("passwd", placeholder="Пароль", label = tagList(icon("unlock-alt"), "Пароль")),
              br(),
              div(
                style = "text-align: center;",
                actionButton("login", 
                              "Войти", 
                             style = button_style),
                br()))), 
        footer = modalButton("Отмена")))
    })
  #    11.3 Проверка и вход в аккаунт                                       ####
    observeEvent(priority = 3,{input$login},{
      if(input$userName %in% users_data$email){
        if(input$passwd == users_data[email == input$userName]$password){
          log_user(TRUE)
          name_user(input$userName)
          removeModal(session = getDefaultReactiveDomain())
            sendSweetAlert(
              session = session,
              title =  "Успешно",
              btn_labels = "Хорошо",
              btn_colors = "#FFA500",
              text = tags$div(h6(paste0("Вы успешно вошли в систему"))),
              type = "success",
              width = "20%"
            )
        }else{
          sendSweetAlert(
            session = session,
            title =  "Неверно введен пароль",
            btn_labels = "Хорошо",
            btn_colors = "#FFA500",
            text = tags$div(h6("Попробуйте еще")),
            type = "error",
            width = "20%"
          )
        }
      }else{
        sendSweetAlert(
          session = session,
          title =  "Неверно введен Email",
          btn_labels = "Хорошо",
          btn_colors = "#FFA500",
          text = tags$div(h6("Попробуйте еще")),
          type = "error",
          width = "20%"
        )
      }
    })
  #    11.4 Выход из аккаунта                                               ####
    observeEvent(priority = 3,{input$out},{
      log_user(FALSE)
      sendSweetAlert(
        session = session,
        title =  "До свидания",
        btn_labels = "Хорошо",
        btn_colors = "#FFA500",
        text = tags$div(h6("Ждем вас снова")),
        type = "success",
        width = "20%"
      )
    })
  #    11.5 Форма Регистрации                                               ####
  observeEvent({input$regis},{
    showModal(modalDialog(
      div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px; font-family: Panton;",
          wellPanel(
            tags$h2("Регистрация", class = "text-center", 
                    style = "padding-top: 0;color:#0F0F0F; font-weight:600; font-family: Panton;"),
            textInput("firstname",  placeholder="Ваша фамилия",            label = tagList(icon("user"), "Фамилия")),
            textInput("secondname", placeholder="Ваше имя",                label = tagList(icon("user"), "Имя")),
            textInput("lastname",   placeholder="Ваше отчество",           label = tagList(icon("user"), "Отчество")),
            textInput("email",      placeholder="your_email@mail.ru",      label = tagList(icon("envelope"), "Email")),
            textInput("company",    placeholder="Название вашей компании", label = tagList(icon("industry"), "Компания")),
            textInput("inn",        placeholder="Ваш ИНН",                 label = tagList(icon("file"), "ИНН")),
            textInput("site",       placeholder="Ссылка на сайт компании", label = tagList(icon("link"), "Сайт")),
            selectizeInput(inputId = 'okato',
                           label = tagList(icon("briefcase"),'Отрасль ведения хозяйственной деятельности'),
                           choices = industry$industry,
                           selected = industry$industry[1],
                           options = list(create = TRUE,
                                          plugins = list('restore_on_backspace')),
                           width = '100%'),
            # textInput("okato",      placeholder="Выберите",                label = tagList(icon("briefcase"), "Сфера деятелности")),
            textInput("country",    placeholder="Россия",                  label = tagList(icon("globe"), "Страна")),
            textInput("city",       placeholder="Санкт-Петербург",         label = tagList(icon("city"), "Город")),
            textInput("job",        placeholder="Аналитик Инвестиций",     label = tagList(icon("pen"), "Должность")),
            textInput("pass",       placeholder="Придумайте пароль",       label = tagList(icon("lock"), "Пароль")),
            br(),
            div(
              style = "text-align: center;",
              actionButton("sign_up", 
                           "Зарегестрироваться", 
                           style = button_style),
              br()))), 
      footer = modalButton("Отмена")))
  })
  #    11.6 Проверка и регистрация                                          ####
  observeEvent(priority = 3,{input$sign_up},{
    if(input$secondname != ""){
      if(input$email != "" & input$email %!in% users_data$email){
        if(input$inn != ""){
          if(input$pass != ""){
            log_user(TRUE)
            name_user(input$email)
            removeModal(session = getDefaultReactiveDomain())
            browser()
            add_new <- data.table(id = last(users_data$id) + 1,
                                  first_name = ifelse(input$firstname == "", NA, input$firstname),
                                  second_name = ifelse(input$second_name == "", NA, input$second_name),
                                  last_name = ifelse(input$last_name == "", NA, input$last_name),
                                  email = ifelse(input$email == "", NA, input$email),
                                  company = ifelse(input$company == "", NA, input$company),
                                  inn = ifelse(input$inn == "", NA, input$inn),
                                  site = ifelse(input$site == "", NA, input$site),
                                  okato = ifelse(input$okato == "", NA, input$okato),
                                  country = ifelse(input$country == "", NA, input$country),
                                  city = ifelse(input$city == "", NA, input$city),
                                  job = ifelse(input$job == "", NA, input$job),
                                  password = ifelse(input$pass == "", NA, input$pass))
            users_data <- rbindlist(list(users_data,add_new))
            write.xlsx(users_data,paste0("3.Data/accounts.xlsx"))
            sendSweetAlert(
              session = session,
              title =  "Успешно",
              btn_labels = "Хорошо",
              btn_colors = "#FFA500",
              text = tags$div(h6(paste0("Вы успешно зарегистрированы. Теперь вам доступен отчет"))),
              type = "success",
              width = "20%"
            )
          }else{
          sendSweetAlert(
            session = session,
            title =  "Ошибка",
            btn_labels = "Хорошо",
            btn_colors = "#FFA500",
            text = tags$div(h6("Пароль обязательное поле для заполниения")),
            type = "error",
            width = "20%"
          )
        }
        }else{
          sendSweetAlert(
            session = session,
            title =  "Ошибка",
            btn_labels = "Хорошо",
            btn_colors = "#FFA500",
            text = tags$div(h6("ИНН обязательное поле для заполниения")),
            type = "error",
            width = "20%"
          )
        }
      }else{
        sendSweetAlert(
          session = session,
          title =  "Ошибка",
          btn_labels = "Хорошо",
          btn_colors = "#FFA500",
          text = tags$div(h6("Email обязательное поле для заполниения")),
          type = "error",
          width = "20%"
        )
      }
    }else{
      sendSweetAlert(
        session = session,
        title =  "Ошибка",
        btn_labels = "Хорошо",
        btn_colors = "#FFA500",
        text = tags$div(h6("Имя обязательное поле для заполниения")),
        type = "error",
        width = "20%"
      )
    }
  })
}