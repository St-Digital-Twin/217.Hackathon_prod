# Разработчик: Н.Бураков
# Дата релиза: Май 2023 года

source('1.R/1.SourceAll.R')

{
  users_data <- as.data.table(read.xlsx(paste0("3.Data/accounts.xlsx")))
 
  industry <- read_delim("3.Data/industry_direct.csv")
  machine  <- read.xlsx("3.Data/machine.xlsx")
  other_services  <- read.xlsx("3.Data/other_services.xlsx")
  polygons <- as.data.table(qread("3.Data/polygons_input")) %>% 
    .[,name := paste0(name, " округ")]
  
  key = "pk.eyJ1IjoibmlraXRhLWJ1cmFrb3YiLCJhIjoiY2t3MGxnMTBpYjdpbTJ1cXdjeGN2eGJvNCJ9.faoxOUVwQQ4QK46LGBhYzg"
  
  ns <- shiny::NS("login")
  
  colors <- c("#30D5C8", "#FFA500", "#700000")
  
  button_style <- 
  "color: #FFFFFF; 
  background-color:#FFA500;
  border: 1px solid #FFA500;
  border-radius: 6px;
  box-shadow: rgba(0, 0, 0, 0.1) 1px 2px 4px;
  display: inline-block;
  outline: 0;
  text-align: center;
  text-rendering: geometricprecision;
  text-transform: none;
  user-select: none;
  -webkit-user-select: none;
  touch-action: manipulation;
  vertical-align: middle;
  cursor: pointer;
  font-size: 16px; 
  font-weight: 600; 
  font-family: Panton;"
  

list_data1 <- list(
  imperson_data = fread('3.Data/imperson_data.csv'), # Отрасли
  patent = '3.Data/patent_data.xlsx' %>%  # Патент
    readxl::read_xlsx() %>%
    as.data.table(),
  cadastr_price = '3.Data/cadastr_price_by_district.xlsx' %>% # Кадастровая стоимость
    readxl::read_xlsx() %>%
    as.data.table(),
  add_services = '3.Data/add_services.xlsx' %>% # Доп услуги хакатона
    excel_sheets() %>%
    sapply(function(x) readxl::read_xlsx('3.Data/add_services.xlsx', sheet = x) %>% as.data.table, USE.NAMES = TRUE),
  machine = '3.Data/machine.xlsx' %>% # Станки
    readxl::read_xlsx() %>%
    as.data.table,
  other_services = '3.Data/other_services.xlsx' %>% # Иные услуги
    readxl::read_xlsx() %>%
    as.data.table

)
  
 
  calc <- InvestCalculation$new(list_data = list_data1)

  # calc$calculateInvestments() %>% print()
  
} # Data
{  js <- "
Shiny.addCustomMessageHandler('anim',
 function(x){

    var $box = $('#' + x.id + ' div.small-box');
    var value = x.value;

    var $s = $box.find('div.inner h4');
    var o = {value: 0};
    $.Animation( o, {
        value: value
      }, {
        duration: 1200
      }).progress(function(e) {
          $s.text((e.tweens[0].now).toFixed(1) +' - ' + x.value2 + x.units);
    });

  }
);
Shiny.addCustomMessageHandler('anim2',
 function(x){

    var $box = $('#' + x.id + ' div.small-box');
    var value = x.value;

    var $s = $box.find('div.inner h4');
    var o = {value: 0};
    $.Animation( o, {
        value: value
      }, {
        duration: 1200
      }).progress(function(e) {
          $s.text((e.tweens[0].now).toFixed(1) + ' ' + x.units);
    });

  }
);
  
  "
 

} # Function

source('ui.R',encoding = "UTF-8")
source('server.R',encoding = "UTF-8")
shinyApp(ui, server)

