try(source("1.R/2.libraries.r"))
library("xlsx")

okved <- read_xlsx("3.Data/A1.DT.0999.all_spr.xlsx", sheet = "15_okved_fc") %>%
  as_tibble()

data <- read_xlsx("3.Data/Обезличенные данные.xlsm", sheet = "Лист2") %>%
  as_tibble()

industries <- data %>% select(1, 2) %>% distinct()

industries_okved <- c(10.5, 26, 10.1, 30.3, 29, 10.7, 28, 13, 19, 20.1, 28.2, 10.7,
                      30.2, 23, 25.4, 21.1, 35.0, 10.2, 21.2, 27.32, 28.1, 16, 24, 18, 10.3,
                      32, 26, 11, 28.9, 28.4, 26, 10.6, 10.8, 11, 74, 28.4, 10.9, 30.1,
                      26, 30.2, 26.4, 10.81, 26, 28.3, 28, 10.4, 25.6)


industries["okved"] <- as.character(industries_okved)
industries[17, "okved"] <- "35_0"

industries <- industries %>% left_join(okved, by = join_by(okved))

data <- data %>% mutate_if(is.numeric, as.character)


year_2020 <- rep("2020", nrow(data))
year_2021 <- rep("2021", nrow(data))
year_2022 <- rep("2022", nrow(data))
value_NA  <- rep(NA,     nrow(data))

new_columns <- unique(str_sub(colnames(data)[3:(length(colnames(data))-1)], end = -6))
new_columns <- new_columns[-1]

data_1 <- data.table(industry = rep(data[1] %>% pull(), 3),
                     sub_industry = rep(data[2] %>% pull(), 3),
                     year = c(year_2020, year_2021, year_2022))
data_1[, new_columns] <- NA

data_1 <- data_1 %>% mutate_if(is.logical, as.character)

for (i in 1:length(new_columns)){
  if (i %in% c(1, 2)){
    data_1[, new_columns[i]] <- c(data[, 2*i+1] %>% pull(), data[, 2*i+2] %>% pull(), value_NA)
  } else {
    data_1[, new_columns[i]] <- c(value_NA, data[, 2*i+1] %>% pull(), data[, 2*i+2] %>% pull())
  }
}

data_2 <- data_1 %>%
  left_join(industries[c(1, 2, 3)], by = join_by(industry == "Основная отрасль",
                                                 sub_industry == "Подотрасль промышленности"))

write.csv(data_2, "3.Data/imperson_data.csv", row.names = FALSE)

write.xlsx(data_2, "3.Data/imperson_data.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)

patent <- read_xlsx("3.Data/Патентование_потенциальный_доход,_Москва.xlsx", sheet = "Table 1") %>%
  as_tibble()

data_3 <- data.frame(patent[6], patent[2], patent[5],
                     patent[7], patent[8], patent[9])

colnames(data_3)[1:3] <- c("ОКВЭД", "Отрасль", "Патент в тыс руб")

write.xlsx(data_3, "3.Data/patent_data.xlsx", sheetName = "Sheet1",
           col.names = TRUE, row.names = FALSE, append = FALSE)




# Ошибка есть по колонка, вместо зп туда указана численность персонала
data1 <- read_xlsx("3.Data/Обезличенные данные.xlsm") %>% 
  as.data.table() %>% 
  setnames(c(1,2), c('industry', 'sub_industry')) %>% # Первые два столбца, меняем имена
  .[, names(.)[-c(1,2)] := lapply(.SD, as.numeric), .SDcols = names(.)[-c(1,2)]] %>% 
  melt(id.vars = c('industry', 'sub_industry'), variable.name = 'indicator') %>% # поварачиваем
  .[, indicator := as.character(indicator)] %>% 
  .[, value := as.numeric(value)] %>% 
  .[, year := str_extract_all(indicator, '\\d+$', simplify = TRUE) %>% as.numeric()] %>% 
  .[, indicator := fcase(indicator %ilike% 'Среднесписочная численность персонала', 'num_employees',
                         indicator %ilike% 'Средняя з.п. сотрудников', 'salary_per_month',
                         indicator %ilike% 'Налоги, уплаченные в бюджет', 'budget_taxes',
                         indicator %ilike% 'Налог на прибыль', 'income_tax',
                         indicator %ilike% 'Налог на имущество', 'property_tax',
                         indicator %ilike% 'Налог на землю', 'land_tax',
                         indicator %ilike% 'НДФЛ', 'personal_income_tax',
                         indicator %ilike% 'Транспортный налог', 'transport_tax',
                         indicator %ilike% 'Прочие налоги', 'other_taxes'
                         )] %>% 
  .[!'num_employees', value := value * 1e3, on = 'indicator'] %>%  # из тыс в руб
  merge(as.data.table(industries)[, 1:3], by.x = c('industry', 'sub_industry'), by.y = c('Основная отрасль', 'Подотрасль промышленности'), all.x = TRUE) %>% 
  na.omit() %>% 
  # .[, value := median(value, na.rm = TRUE), by = .(industry, sub_industry, year, indicator)]
  dcast(industry + sub_industry + okved + year ~ indicator, fun.aggregate = mean) %>% 
  
  
  .[income_tax >= 32880000, income_tax_rate := 0.2] %>%  # Если налог больше или равен ( 219,2 * 0,15) = 32,88 млн руб, ОСН - ставка 20%
  .[income_tax < 32880000, income_tax_rate := 0.15] %>%  # Если налог меньше ( 219,2 * 0,15) = 32,88 млн руб, УСН (доход - расход) - ставка 15%
  .[year == 2021, paste0('specific_', c('income_tax', 'property_tax', 'land_tax', 'personal_income_tax', 'transport_tax', 'other_taxes')) := lapply(.SD, function(x) x / num_employees), .SDcols = c('income_tax', 'property_tax', 'land_tax', 'personal_income_tax', 'transport_tax', 'other_taxes')] %>%  # Уделки по налогам (налог к численности персонала) - глупо конечно, но по другому никак
  .[, paste0('specific_', c('income_tax', 'property_tax', 'land_tax', 'personal_income_tax', 'transport_tax', 'other_taxes')) := lapply(.SD, median, na.rm = TRUE), .SDcols = paste0('specific_', c('income_tax', 'property_tax', 'land_tax', 'personal_income_tax', 'transport_tax', 'other_taxes')), by = .(industry, sub_industry, okved)] %>% # Распидорасиваем на все года
  .[!(sub_industry %chin% c('Сведения отсутствуют', 'Иные отрасли')), industry := sub_industry] %>% 
  .[, specific_income := specific_income_tax / income_tax_rate] %>%  # Уделка по прибыли
  .[year == 2021] %>% 
  .[, c('industry', 'okved', 'salary_per_month', grep(x = names(.), pattern = '^specific_', value = TRUE)), with = FALSE]

write.csv(data1, "3.Data/imperson_data.csv", row.names = FALSE)

write.xlsx(data1, "3.Data/imperson_data.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)

patent <- '3.Data/patent_data.xlsx' %>% 
  read_xlsx() %>% 
  as.data.table()

industry_direct <- data1[, .(industry, okved)] %>% 
  list(patent[, .(industry, okved)]) %>% 
  rbindlist(use.name = TRUE) %>% 
  unique(by = 'industry')

write.csv(industry_direct, '3.Data/industry_direct.csv')



  
