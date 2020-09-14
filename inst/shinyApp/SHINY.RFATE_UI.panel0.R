
tabPanel(title =  HTML("<span class='panel_title'><i class='fa fa-home'></i></span>")
         , value = "panel0"
         , sidebarLayout(
           
           # Inputs
           sidebarPanel(
             width = 12,
             style = panel.style,
             withMathJax(),
             
             br(),
             fluidRow(
               column(3
                      , br()
                      , div(id = "help0_6"
                            , actionButton(inputId = "HELP.panel0"
                                           , label = "Need some help"
                                           , icon = icon("question-circle")
                                           , width = "100%"
                                           , style = button.style.help)
                      )
                      , br()
                      , div(id = "help0_7"
                            , actionButton(inputId = "web.RFate"
                                           , label = "Go to RFate website"
                                           , icon = icon("arrow-circle-right")
                                           , width = "100%"
                                           , style = button.style.help
                                           , onclick ="window.open('https://mayagueguen.github.io/RFate/', '_blank')")
                      )
                      , br()
                      , br()
                      # , downloadButton(outputId = "report"
                      #                  , label = "Generate report"
                      #                  , width = "100%"
                      #                  , style = button.style.help)
                      
               )
               , column(1, br())
               , column(8
                        , br()
                        , div(id = "help0_1"
                              , HTML("
                                     <p><code>FATE</code> is a <strong>spatially and temporally explicit 
                                     vegetation model</strong>. It uses <strong>plant functional groups 
                                     (PFG)</strong> and integrates important mechanisms driving vegetation 
                                     dynamics, structure and diversity, such as <strong>demographic cycle
                                     </strong>, obviously, but also <strong>seeds dispersal</strong>, 
                                     <strong>abiotic filtering</strong> or <strong>biotic interactions</strong> 
                                     (through the competition for resources like light availability or soil 
                                     suitability).</p>
                                     <p>If <strong>primary succession</strong> is the most obvious ecological 
                                     process that can be modelled with <code>FATE</code>, events related to 
                                     <strong>secondary succession</strong> can be represented as well using 
                                     the various <code>FATE</code> <strong>add-on modules</strong> : 
                                     disturbances (mowing, grazing, fire...), drought event, invasive species.</p>
                                     <p>As vegetation modelling can be challenging (data gathering, 
                                     parameterization, handling results...), <code>RFate</code> provides 
                                     <strong>user-friendly functions</strong> to go through the <strong>whole</strong> 
                                     <code>FATE</code> <strong>workflow</strong>.
                                     ")
                              )
                        , HTML("<hr><br/>")
                              )
                        )
             , fluidRow(
               column(1, br())
               , column(6
                      , br()
                      , div(id = "help0_2"
                            , HTML("
                                   <p><i class='fa fa-object-group' style='font-size:30px;'></i>&emsp;&emsp;
                                   <strong>STEP 1 : Creation of Plant Functional Groups</strong><br/></p>
                                   <p style='padding:0 0 0 40px;'>A plant functional group, or <strong>PFG</strong>, 
                                   is \"<em>A set of representative species is classified based on key biological 
                                   characteristics, to determine groups of species sharing ecological 
                                   strategies</em>\" 
                                   (<a href='http://j.boulangeat.free.fr/pdfs/Boulangeat2012_GCB_published.pdf' 
                                   title='Boulangeat, I., Philippe, P., Abdulhak, S., Douzet, R., Garraud, 
                                   L., Lavergne, S., Lavorel, S., Van Es J., Vittoz, P. and Thuiller, W. 
                                   Improving plant functional groups for dynamic models of biodiversity: 
                                   at the crossroad between functional and community ecology. 
                                   Global Change Biology, 18, 3464-3475.'>Boulangeat, 2012</a>). 
                                   PFGs are based on their distribution, physiological characteristics, 
                                   competition traits...</p>
                                   ")
                                   )
                                   )
               , column(5
                        , br()
                        , div(id = "help0_4"
                              , HTML("
                                     <p><i class='fa fa-cogs' style='font-size:30px;'></i>&emsp;&emsp;
                                     <strong>STEP 3 : Run a simulation</strong><br/></p>
                                     <p style='padding:0 0 0 40px;'>Give a simulation folder and a simulation parameters file, 
                                     and run your <code>FATE</code> simulation.
                                     ")
                              )
                              )
                              ) ## END fluidRow
             , fluidRow(
               column(1, br())
               , column(6
                      , br()
                      , div(id = "help0_3"
                            , HTML("
                                   <p><i class='fa fa-copy' style='font-size:30px;'></i>&emsp;&emsp;
                                   <strong>STEP 2 : Creation of simulation folder</strong><br/></p>
                                   <p style='padding:0 0 0 40px;'><code>FATE</code> requires a quite large number of 
                                   parameters, which are stored into <code>.txt</code> files, presented to and 
                                   recovered by the software. These <strong>parameters</strong> can be of 3 types :</p>
                                   <ol>
                                   <li>
                                   <strong>Filenames</strong>, to guide the application to other parameter files that should be read</li>
                                   <li>These filenames either correspond to :
                                   <ul>
                                   <li>other parameter files that contain <strong>values</strong> to be actually read and used</li>
                                   <li>
                                   <strong>raster</strong> files, with the extension <code>.tif</code> (lighter) or <code>.img</code>
                                   </li>
                                   </ul>
                                   </li>
                                   </ol>
                                   ")
                            )
                            )
               , column(5
                        , br()
                        , div(id = "help0_5"
                              , HTML("
                                     <p><i class='fa fa-chart-bar' style='font-size:30px;'></i>&emsp;&emsp;
                                     <strong>STEP 4 : Creation of simulation outputs & graphics</strong><br/></p>
                                     <p style='padding:0 0 0 40px;'>Once a <code>FATE</code> simulation is done, 
                                     some post treatment panels and functions are available :</p>
                                     <ul>
                                     <li><strong>BROWSER :</strong> to visualize existing graphics</li>
                                     <li><strong>Through time :</strong> create tables and graphics for temporal evolution of PFG abundances ...</li>
                                     <li><strong>Specific year :</strong> create tables and spatial maps of basic patterns (richness, diversities ...) 
                                     for specific years</li>
                                     </ul>
                                     ")
                              )
                              )
                              ) ## END fluidRow
                              ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 12,
             wellPanel(id = "main.panel"
                       , style = border.style
                       , fluidRow(
                         column(4, br())
                         , column(4, br())
                         # , column(4, HTML("<img src='./ARBRES.png' width='200px' />"))
                         , column(4, HTML(paste0("<img src='https://mayagueguen.github.io/pictures/logo-leca.png'"
                                                 , " width='200px' alt='LOGO LECA' style='float:right;' />")))
                       )## END fluidRow
             ) ## END wellPanel
           ) ## END mainPanel
                              ) ## END sidebarLayout
                        ) ## tabPanel
