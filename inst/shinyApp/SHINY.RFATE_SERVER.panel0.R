
####################################################################

observeEvent(input$HELP.panel0, {
  introjs(session = session
          , options = list("nextLabel" = "Next"
                           , "prevLabel" = "Prev"
                           , "skipLabel" = "Close"
                           , steps = data.frame(element = paste0("#help0_", 1:7)
                                                , intro = c("<p>This is a user-friendly application for anyone who wants to model vegetation with <code>FATE</code>!</p>
                                                            <p>This application allows to perform all the steps of the modelling process, from the preparation of data 
                                                            and parameter files, to the software execution and the analysis of results.</p>
                                                            <p>It uses both <code>FATE</code> (coded in C++) for the modelling itself, and the <code>RFate</code> package 
                                                            for all that is pre- and post-analysis.</p>"
                                                            , "<p><em>1. Build Plant Functional Groups</em></p>
                                                            <p>This first step allows one to gather all data needed (species releves and traits, and environmental data) 
                                                            and process them to identify dominant species of the studied area and group them into functional groups.</p>
                                                            <p>It can be achieved within the 1st panel.</p>"
                                                            , "<p><em>2. Build parameter files</em></p>
                                                            <p>This second step corresponds to the parametrization of all parameters needed by <code>FATE</code>, and the 
                                                            creation of the corresponding files.</p>
                                                            <p>It can be achieved within the 2nd panel.</p>"
                                                            , "<p><em>3. Run a simulation</em></p>
                                                            <p>Once PFG and parameter files are ready, <code>FATE</code> simulations can be run within the 3rd panel.</p>"
                                                            , "<p><em>4. Analyse results</em></p>
                                                            <p>Finally, the 4th panel allows some post-treatment :</p>
                                                            <ul>
                                                            <li>creation of summary tables</li>
                                                            <li>transformation of abundance maps into O/1</li>
                                                            <li>evaluation of predictions</li>
                                                            <li>creation and visualization of basic graphics</li>
                                                            </ul"
                                                            , "These buttons can be found all over the applications providing some informations about :
                                                            <ul>
                                                            <li>the different steps to be followed in each part</li>
                                                            <li>the different parameters needed by the available functions, and redirection to more complete documentation</li>
                                                            </ul>"
                                                            , "More detailed informations about <code>FATE</code> software or <code>RFate</code> package can be found on their website.
                                                            "))
          )
  )
})

