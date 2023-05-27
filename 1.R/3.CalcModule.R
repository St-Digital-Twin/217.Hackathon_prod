# source('R/1.SourceAll.R')

# try(source("R/2.libraries.r"))


InvestCalculation <- R6Class(classname = 'InvestCalculation',

                             public = list(
                               # list_data = NA,
                               initialize = function(list_data = NA){
                                 private$list_data <- list_data
                               },

                               calculateInvestments = function(industry = 'Металлообработка', # Отрасль
                                                                org_type = 'ИП', # Тип организации: ООО или ИП
                                                                number_employees = 150, # Количество работников
                                                                district_name = 'ЗАО', # Район производства
                                                                tax_type = 'УСН', # Вид налогооблажения, ОСН, УСН, Патент
                                                                land_area = 7000, # Площадь участка в м2
                                                                building_area = 4000, # Площадь объектов кап строительства в м2
                                                                machine = "Токарные станки", # Предполагаемое оборудование, можно вектора
                                                                number_machines = 15, # Количество оборудования, можно вектора
                                                                accounting = TRUE, # Бух услуги TRUE, FALSE
                                                                other_services = 'Отопление' # Иные услуги, можно вектор
                               ) {
                                 private$calculateInvestmentsPrivate(
                                   industry = industry,
                                   org_type = org_type, # Тип организации: ООО или ИП
                                   number_employees = number_employees, # Количество работников
                                   district_name = district_name, # Район производства
                                   tax_type = tax_type, # Вид налогооблажения, ОСН, УСН, Патент
                                   land_area = land_area, # Площадь участка в м2
                                   building_area = building_area, # Площадь объектов кап строительства в м2
                                   machine = machine, # Предполагаемое оборудование, можно вектора
                                   number_machines = number_machines, # Количество оборудования, можно вектора
                                   accounting = accounting, # Бух услуги TRUE, FALSE
                                   other_services = other_services # Иные услуги, можно вектор

                                 )
                               }
                             ),
                             private = list(
                               list_data = NULL,
                               calculateInvestmentsPrivate = function(industry = 'Металлообработка', # Отрасль
                                                                      org_type = 'ИП', # Тип организации: ООО или ИП
                                                                      number_employees = 150, # Количество работников
                                                                      district_name = 'ЗАО', # Район производства
                                                                      tax_type = 'УСН', # Вид налогооблажения, ОСН, УСН, Патент
                                                                      land_area = 7000, # Площадь участка в м2
                                                                      building_area = 4000, # Площадь объектов кап строительства в м2
                                                                      machine = "Токарные станки", # Предполагаемое оборудование, можно вектора
                                                                      number_machines = 15, # Количество оборудования, можно вектора
                                                                      accounting = TRUE, # Бух услуги TRUE, FALSE
                                                                      other_services = 'Отопление' # Иные услуги, можно вектор
                               ) {


                                 out <- NULL

                                 if(!is.null(industry)){

                                 if(tax_type == 'Патент'){ # Проверка на патент

                                   if(org_type == 'ООО'){ # Проверка на ООО
                                     out <- 'ООО не может иметь Патентное налогооблажение'

                                   } else if(org_type == 'ИП'){

                                     patent_industry <- private$list_data$patent %>%
                                       .[industry, on = 'industry'] %>%
                                       .[is.na(district), district := district_name] %>%
                                       .[is.na(m2_from), m2_from := building_area] %>%
                                       .[is.na(m2_to), m2_to := building_area] %>%
                                       .[!is.na(patent)]

                                     if(nrow(patent_industry) > 0){ # Если есть отрасль
                                       patent_ind <- patent_industry %>%
                                         .[district %chin% district_name] %>%
                                         .[between(building_area, m2_from, m2_to)]
                                         # .[building_area %between% c(m2_from, m2_to)]

                                       if(nrow(patent_ind) > 0){

                                         okved <- patent_ind$okved %>%
                                           unique()

                                         other_taxes <- private$list_data$imperson_data %>%
                                           .[okved, on = 'okved'] %>%
                                           .[, lapply(.SD, min, na.rm = TRUE), .SDcols = c('specific_personal_income_tax', 'specific_transport_tax', 'specific_other_taxes'), by = 'okved']

                                         df <- patent_ind %>%
                                           .[, patent := median(patent, na.rm = TRUE), by = 'okved'] %>%
                                           .[, `:=`(industry = industry,
                                                    number_employees = number_employees)] %>%
                                           # .[1] %>%
                                           .[, .(industry, okved, income_tax = patent)] %>%
                                           .[, income := income_tax / 0.06] %>%  # 6% для патента
                                           merge(other_taxes, by = 'okved', all.x = TRUE)

                                       } else {
                                          out <- 'По заданым параметрам нет такого района'
                                       }

                                     } else {
                                          out <- 'По заданым параметрам нет такой отрасли'
                                       }

                                   }
                                 } else { # УСН или ОСН
                                   df_industry <- private$list_data$imperson_data %>%
                                     .[industry, on = 'industry'] %>%
                                     .[!is.na(okved)]

                                   # tax_income <- ifelse(tax_type == 'УСН', 0.15, 0.2) # УСН - 15%, ОСН - 20%

                                   if(nrow(df_industry) > 0){ # Есть отрасль
                                     df <- df_industry %>%
                                       .[, lapply(.SD, median, na.rm = TRUE), .SDcols = c('specific_income', 'specific_personal_income_tax', 'specific_transport_tax', 'specific_other_taxes'), by = 'okved'] %>%
                                       .[, industry := industry] %>%
                                       .[, `:=`(industry = industry,
                                                number_employees = number_employees)] %>%
                                       .[, income := specific_income * number_employees] %>%
                                       .[(tax_type == 'УСН') & (income < (219.2 * 1e6)) & (number_employees < 130), tax_income := 0.15] %>%  #  Если выбран УСН, прибыль меньше 219,2 млн рублей и численность меньше 130 человек, тогда налог 15%, в протмвном случае это ОСН и 205
                                       .[is.na(tax_income), tax_income := 0.2] %>%
                                       .[, income_tax := specific_income * tax_income] %>%
                                       .[, !c('specific_income', 'tax_income')]

                                   } else { # Если нет отрасли, ищем в патентах
                                     okved <- private$list_data$patent %>%
                                       .[industry, on = 'industry'] %>%
                                       .$okved

                                     if(all(is.na(okved))){
                                       out <- 'По заданым параметрам нет такой отрасли'
                                     } else {

                                       df <- private$list_data$imperson_data %>%
                                         .[okved, on = 'okved'] %>%
                                         .[, lapply(.SD, median, na.rm = TRUE), .SDcols = c('specific_income', 'specific_personal_income_tax', 'specific_transport_tax', 'specific_other_taxes'), by = 'okved'] %>%
                                         .[, industry := industry] %>%
                                         .[, `:=`(industry = industry,
                                                    number_employees = number_employees)] %>%
                                         .[, income := specific_income * number_employees] %>%
                                         .[(tax_type == 'УСН') & (income < (219.2 * 1e6)) & (number_employees < 130), tax_income := 0.15] %>%  #  Если выбран УСН, прибыль меньше 219,2 млн рублей и численность меньше 130 человек, тогда налог 15%, в протмвном случае это ОСН и 205
                                         .[is.na(tax_income), tax_income := 0.2] %>%
                                         .[, income_tax := specific_income * tax_income] %>%
                                         .[, !c('specific_income', 'tax_income')]
                                     }
                                   }

                                 }



                                 if(is.null(out)){
                                   # Цена участка за м2
                                   land_price <- private$list_data$cadastr_price[district_name, on = 'district']$price %>%
                                     ifelse(is.na(.), 0, .)

                                   # Цена здания за м2
                                   building_price_min <- private$list_data$add_services$capital_construction_cost$min_price
                                   building_price_max <- private$list_data$add_services$capital_construction_cost$max_price

                                   # Цена оборудования
                                   machine_price <- private$list_data$machine %>%
                                     .[machine, on = 'type'] %>%
                                     .[!is.na(price)]

                                   # Затраты на оборудование
                                   if(nrow(machine_price) > 0){
                                     machine_costs <- machine_price %>%
                                       cbind(data.table(number_machines = number_machines)) %>%
                                       .[, total := number_machines * price] %>%
                                       .$total %>%
                                       sum(na.rm = TRUE)

                                   } else {
                                     machine_costs <- 0
                                   }

                                   # Цена бух услуг
                                   if(accounting){
                                     accounting_price <- private$list_data$add_services$accounting %>%
                                       .[org_type, on = 'type'] %>%
                                       .[, c('num_emp', tax_type), with = FALSE] %>%
                                       setnames(tax_type, 'price') %>%
                                       # .[num_emp > number_employees, price := last(price)] %>%
                                       .[between(number_employees, shift(num_emp), num_emp)] %>%
                                       .[1] %>%
                                       .$price
                                   } else {
                                     accounting_price <- 0
                                   }

                                   # Цена Пошлины
                                   state_duty_price <- private$list_data$add_services$state_duty %>%
                                     .[org_type, on = 'type'] %>%
                                     .$price

                                  # Другие услуги
                                   # if(!is.null(other_services)){
                                   other_services <- private$list_data$other_services %>%
                                     .[other_services, on = 'service'] %>%
                                     .[!is.na(price)]


                                   if(nrow(other_services) > 0){
                                   other_services_price <- other_services %>%
                                     .[between(building_area, shift(area), area)] %>%
                                     .[1] %>%
                                     .$price

                                   if(is.na(other_services_price)){
                                     other_services_price <- other_services$price %>% last()
                                     }
                                   } else {
                                     other_services_price <- 0
                                   }

                                   out <- df %>%
                                     copy() %>%
                                     .[, c('personal_income_tax', 'transport_tax', 'other_taxes') := lapply(.SD, function(x) x * number_employees), .SDcols = c('specific_personal_income_tax', 'specific_transport_tax', 'specific_other_taxes')] %>% # Находим НДФЛ, транспортный и другие налоги
                                     .[, !c('specific_personal_income_tax', 'specific_transport_tax', 'specific_other_taxes')] %>%
                                     .[, gross_salary := personal_income_tax / 0.13] %>%  # Зарплата до вычета за год для всех работников, брутто-зарплата = НДФЛ / ставка НДФЛ (13%)
                                     .[, `:=`(CHI_min = gross_salary * 0.001, # Обязательное медицинское страхование (ОМС) от 0.1%
                                              CHI_max = gross_salary * 0.051, # Обязательное медицинское страхование (ОМС) до 5.1%
                                              CSI_min = gross_salary * 0.018, # Обязательное социальное страхование на случай временной нетрудоспособности в связи с материнством (ОСС) от 1.8%
                                              CSI_max = gross_salary * 0.029, # Обязательное социальное страхование на случай временной нетрудоспособности в связи с материнством (ОСС) до 2.9%,
                                              CPI_min = gross_salary * 0.06, # Обязательное пенсионное страхование (ОПС) от 6%
                                              CPI_max = gross_salary * 0.22  # Обязательное пенсионное страхование (ОПС) до 22%

                                              )] %>%
                                     # Если значение больше установленного предела базы налога для ОСС
                                     .[gross_salary > 1032000 * number_employees * 12, `:=`(CSI_min = 1032000 * number_employees * 12 * 0.018, # Обязательное социальное страхование на случай временной нетрудоспособности в связи с материнством (ОСС) от 1.8%
                                                                                            CSI_max = 1032000 * number_employees * 12 * 0.029 # Обязательное социальное страхование на случай временной нетрудоспособности в связи с материнством (ОСС) до 2.9%
                                              )] %>%
                                     # Если значение больше установленного предела базы налога для ОПС
                                     .[gross_salary > 1565000 * number_employees * 12, `:=`(CPI_max = 1032000 * number_employees * 12 * 0.22 + (gross_salary - 1032000 * number_employees * 12) * 0.1 # Обязательное пенсионное страхование (ОПС) до 22%, сверх 1565000 руб на человека за месяц
                                              )] %>%
                                     .[, `:=`(total_staff_costs_min = gross_salary + CHI_min + CSI_min + CPI_min, # Итоговые затраты на персонал от, руб
                                              total_staff_costs_max = gross_salary + CHI_max + CSI_max + CPI_max  # Итоговые затраты на персонал до, руб
                                              )] %>%
                                     .[, land_costs := land_price * land_area] %>%  # Затраты на землю
                                     .[, land_tax := land_costs * 0.015] %>% # Налог на землю, 1.5%
                                     .[, `:=`(building_costs_min = building_price_min * building_area, # Затраты на объекты кап строительства от
                                              building_costs_max = building_price_max * building_area  # Затраты на объекты кап строительства до
                                              )] %>%
                                     .[, machine_costs := machine_costs] %>%  # Затраты на оборудование, станки
                                     .[, `:=`(property_costs_min = machine_costs + building_costs_min, # Затраты на имущество от
                                              property_costs_max = machine_costs + building_costs_max  # Затраты на имущество до
                                              )] %>%
                                     .[, `:=`(property_tax_min = property_costs_min * 0.015, # Налог на имущество от, 1.5%
                                              property_tax_max = property_costs_max * 0.015  # Налог на имущество до, 1.5%
                                              )] %>%
                                     .[, `:=`(taxes_min = property_tax_min + land_tax + personal_income_tax + transport_tax + other_taxes + income_tax, # Налоги от
                                              taxes_max = property_tax_max + land_tax + personal_income_tax + transport_tax + other_taxes + income_tax  # Налоги до
                                              )] %>%
                                     .[, accounting_costs := accounting_price] %>% # Затраты на бухгалтерские услуги
                                     .[, state_duty_costs := state_duty_price] %>% # Затраты на госпошлину
                                     .[, other_services_costs := other_services_price] %>% # Затраты на иные услуги
                                     .[, services_costs := accounting_costs + state_duty_costs + other_services_costs] %>% # Суммировать услуги
                                     .[, services_costs := accounting_costs + state_duty_costs + other_services_costs] %>% # Суммировать услуги
                                     .[, `:=`(capex_min = state_duty_costs + other_services_costs + property_costs_min + land_costs, # Кап затраты в основные средства от
                                              capex_max = state_duty_costs + other_services_costs + property_costs_max + land_costs, # Кап затраты в основные средства до
                                              opex_min = accounting_costs + total_staff_costs_min, # Оборотные средства. Эксплуатационные затраты. Постоянные расходы от
                                              opex_max = accounting_costs + total_staff_costs_max  # Оборотные средства. Эксплуатационные затраты. Постоянные расходы до

                                              )] %>%
                                     .[, `:=`(total_costs_min = capex_min + opex_min + taxes_min - personal_income_tax, # Итоговые затраты (Кап вложения + эксплуатационные вложения + налоги (- НДФЛ, так как он уже учитывается в эксплуатационных затратах)) от
                                              total_costs_max = capex_max + opex_max + taxes_max - personal_income_tax # Итоговые затраты (Кап вложения + эксплуатационные вложения + налоги (- НДФЛ, так как он уже учитывается в эксплуатационных затратах)) до
                                              )] %>%
                                     # .[, names(.)[-grep(x = names(.), pattern = paste0(c("okved", "industry", "number_employees"), collapse = '|'))] := lapply(.SD, function(x) round(x / 1e6, 2)), .SDcols = names(.)[-grep(x = names(.), pattern = paste0(c("okved", "industry", "number_employees"), collapse = '|'))]] # Приводим к млн рублей
                                   .[, names(.)[-grep(x = names(.), pattern = paste0(c("okved", "industry", "number_employees"), collapse = '|'))] := lapply(.SD, function(x) x / 1e6), .SDcols = names(.)[-grep(x = names(.), pattern = paste0(c("okved", "industry", "number_employees"), collapse = '|'))]] # Приводим к млн рублей

                                 }

                                 } else {
                                   out <- 'Надо заполнить отрасль'
                                 }

                                 return(out)

                               }
                             ))

#
# list_data <- list(
#   imperson_data = fread('3.Data/imperson_data.csv'), # Отрасли
#   patent = '3.Data/patent_data.xlsx' %>%  # Патент
#     read_xlsx() %>%
#     as.data.table(),
#   cadastr_price = '3.Data/cadastr_price_by_district.xlsx' %>% # Кадастровая стоимость
#     read_xlsx() %>%
#     as.data.table(),
#   add_services = '3.Data/add_services.xlsx' %>% # Доп услуги хакатона
#     excel_sheets() %>%
#     sapply(function(x) read_xlsx('3.Data/add_services.xlsx', sheet = x) %>% as.data.table, USE.NAMES = TRUE),
#   machine = '3.Data/machine.xlsx' %>% # Станки
#     read_xlsx() %>%
#     as.data.table,
#   other_services = '3.Data/other_services.xlsx' %>% # Иные услуги
#      read_xlsx() %>%
#     as.data.table
#
# )
