# 2. UI                                 ####
ui <- bs4DashPage(
  dark = NULL,
  # 2.1. Header                         ####
  header = bs4DashNavbar(
    tags$head(
      includeCSS("www/CSS.css")
    ),
    tags$style(HTML(
      "<script>
           $( document ).ready(function() {
              $(\".tab-content [type='checkbox']\").on('click', function(){
                var is_checked = this.checked;
                if(typeof is_checked === 'boolean' && is_checked === true){
                  setTimeout(function() {
                    window.scrollTo(0,document.body.scrollHeight);
                  }, 200)
                }
              })
           })
        </script>")),
#     tags$head(HTML(
#       '<!-- Yandex.Metrika counter -->
# <script type="text/javascript" >
#    (function(m,e,t,r,i,k,a){m[i]=m[i]||function(){(m[i].a=m[i].a||[]).push(arguments)};
#    m[i].l=1*new Date();
#    for (var j = 0; j < document.scripts.length; j++) {if (document.scripts[j].src === r) { return; }}
#    k=e.createElement(t),a=e.getElementsByTagName(t)[0],k.async=1,k.src=r,a.parentNode.insertBefore(k,a)})
#    (window, document, "script", "https://mc.yandex.ru/metrika/tag.js", "ym");
# 
#    ym(93358670, "init", {
#         clickmap:true,
#         trackLinks:true,
#         accurateTrackBounce:true,
#         webvisor:true
#    });
# </script>
# <noscript><div><img src="https://mc.yandex.ru/watch/93358670" style="position:absolute; left:-9999px;" alt="" /></div></noscript>
# <!-- /Yandex.Metrika counter -->')),
    status = 'light',
    controlbarIcon = shiny::icon("info"),
    textOutput("texttitle"),
    rightUi = tagList(tags$li(class='dropdown', bs4Dash::actionButton("contact", "Обратная связь", icon = icon("comment"),status = "danger", style = paste0(button_style,""))),
                      tags$li(class='dropdown', userOutput("user")))
    ),
  # 2.2. Sidebar                        ####
  sidebar = bs4DashSidebar(
    collapsed = TRUE,
    minified = TRUE,
    expandOnHover = FALSE,
    elevation = 5,
    customArea = actionButton("help_button", "Help",status = "danger",
                              style = button_style),
    id = "sidebar",
    imageOutput("logo", height = "70px"),
    bs4SidebarMenu(
      id = "sbMenu",
      tags$style(HTML('.sidebar{font-family: Panton;
                                 color: black}
                        }
                      ')),
      bs4SidebarMenuItem("Ввод параметров", tabName = "analyst", icon = icon("chart-column")),
      # bs4SidebarMenuItem("City model & Impact", tabName = "impro", icon =icon("code-compare")),
      # bs4SidebarMenuItem("Search", tabName = "search", icon =icon("magnifying-glass")),
      # 2.2.1. Menu                     ####
      setSliderColor(c(rep("black",100)), c(1:100)),
      chooseSliderSkin("Flat")
      )
    ),
  # 2.3. Controlbar (left)              ####
    controlbar = bs4DashControlbar(
      tags$ol(
        style = "font-size:12px;",
        tags$h5 (tags$b("Ссылки"), style = "font-size: 18px;margin-top: 15px; margin-bottom: 15px;"),
        tags$li(a(href = "https://dtwin.ru/", target = "_blank", "Digital Twin's website"),style = "font-size: 14px;")
      )
  ),
  
  # 2.4. Body                           ####
  body = bs4DashBody(
    use_theme(create_theme(
      bs4dash_status(light = "#787878"),
      bs4dash_status(light = "#1c1c1c", primary = "#FFA500"),
      bs4dash_vars(
        navbar_light_color = "#787878",
        navbar_light_active_color = "white",
        navbar_light_hover_color = "white"
      ),
      bs4dash_font(
        size_base = "0.85rem"
      ),
      bs4dash_sidebar_light(
        bg = "white",
        color = '#1c1c1c',
        hover_color  = '#787878',
        submenu_active_bg  = '#787878'
          # header_color = 'black'
      ),
      bs4dash_sidebar_dark(
        bg = "white",
        color = '#1c1c1c',
        hover_color  = '#787878',
        submenu_active_bg   = '#787878'
          # header_color = 'black'
      ),
      bs4dash_layout(
        sidebar_width = "240px",
        main_bg = "#1c1c1c"
      ),
      bs4dash_color(
        gray_900 = "#FFF"
      )
    )),
    tags$head(tags$script(HTML(js))),
    tags$style(HTML('*{font-family: Panton}')),
    bs4TabItems(
      bs4TabItem(tabName = "analyst",uiOutput("ui_0")),
      bs4TabItem(tabName = "impro",uiOutput("ui_1")),
      bs4TabItem(tabName = "search",uiOutput("ui_2"))
    ),
    tags$head(tags$style(HTML('
        .dropdown-menu {
          font-weight: bold;
          font-family: Panton;
          color: #0F0F0F;
                        }'))),
    tags$head(tags$style(HTML('
        .modal-content {
          font-weight: bold;
          font-family: Panton;
          color: #0F0F0F;
                        }'))),
    # tags$head(
    #   tags$style(HTML("
    #   .selectize-input {
    #     height: 30px;
    #   }
    # 
    # "))),
    tags$head(tags$style("#texttitle{
                                 font-size: 20px;
                                 font-weight: bold;
                                 }")),
    tags$head(tags$style(HTML('
        .nav-pills .nav-link.active {
          font-weight: bold;
          font-family: Panton;
          color: #0F0F0F;
                        }')))
    )
  )

