
path.reference = "https://mayagueguen.github.io/RFate/reference/"
path.articles = "https://mayagueguen.github.io/RFate/articles/"

###################################################################################################################################

theme.color = "#3a7da8"
theme.font = "'Londrina Solid', cursive"
navbar.color = "#e0dbd9"
navbar.color.text = "#8c8582"
button.color = "rgba(96, 129, 150, 0.5)"
help.color = "#dee2e8"
help.color = "#cdcfd1"

baliseHTML.font = "@import url('https://fonts.googleapis.com/css?family=Londrina+Solid:200,300|Medula+One|Slabo+27px|Francois+One');"
baliseHTML.theme = paste0("
      background-color: ", theme.color, ";
      color: #FFFFFF;
      border-radius: 0px;")
baliseHTML.title = paste0("
      font-family: ", theme.font, ";
      font-size: 20px;
      font-weight: 200;")
baliseHTML.margin0 = paste0("
      padding: 0px;
      margin-top: 0px;")


button.style = paste0("background-color: ", button.color, "; border-width: 0px;")
button.style.action = paste0(button.style, " margin-bottom: 3px;")
button.style.help = paste0("color:#FFFFFF; font-family: 'Londrina Solid', cursive; "
                           , "font-size: 18px; background-color: rgba(10, 58, 135, 0.8); border-width:0px;")

border.style = "background-color:#FFFFFF; border-solid:solid; border-width:1px;"

panel.style = paste0("border-width:0px; background-color:", help.color, "; margin-top:18px;")
# , "; margin-left:15px; margin-top:18px;")
panel.style.scrollX = paste0(panel.style, " overflow-x:scroll;")
panel.style.scrollY = paste0(panel.style, " overflow-y:scroll; max-height:300px;")



param.style = function(param.text)
{
  return(HTML(paste0("<span style = 'font-style: italic; font-weight: normal;'>"
                     , param.text
                     , "</span>")))
}


###################################################################################################################################

help.web = function(web.address)
{
  return(paste0("<a href='"
                , web.address
                , "' target='_blank'>See more details on <span style='font-family:Monospace;'>RFate</span> package website.</a>"))
}

help.param.name = function(param.text)
{
  return(paste0("<tr><td style='width:30%;font-weight:bold;font-family:Monospace;vertical-align:top;'>"
                , param.text
                , "</td>"))
}
help.param.desc = function(param.text)
{
  return(paste0("<td style='width:70%;'>"
                , param.text
                , "</td></tr>"))
}
help.full = function(param.web = NULL, param.name.vec, param.desc.vec)
{
  TEXT.full = foreach(i = 1:length(param.name.vec), .combine = "c") %do%
  {
    text.name = help.param.name(param.name.vec[i])
    text.desc = help.param.desc(param.desc.vec[i])
    return(paste0(text.name, text.desc))
  }
  TEXT.full = paste0(TEXT.full, collapse = "")

  TEXT = paste0("<table style='width:100%;'>"
                , TEXT.full
                , "</table>")
  if (!is.null(param.web))
  {
    TEXT = paste0(help.web(param.web), TEXT)
  }
  return(HTML(TEXT))
}

###################################################################################################################################

help.HTML = function(html.file, target.anchor = 'class="hasAnchor"', target.class = '#arguments', web.address = NULL)
{
  TEXT = readLines(html.file)
  TEXT.keep = help.web(web.address = paste0(path.reference, basename(html.file)))
  if (!is.null(web.address))
  {
    TEXT.keep = help.web(web.address = web.address)
  }

  ind.anchor = grep(target.anchor, TEXT)
  # ind.anchor = c(ind.anchor, length(TEXT))
  for (targ in target.class)
  {
    ind.class = grep(targ, TEXT[ind.anchor])
    TEXT.keep = c(TEXT.keep
                  , "<hr>"
                  , TEXT[(ind.anchor[ind.class] + 1):(ind.anchor[min(ind.class + 1, length(ind.anchor))] - 1)]
    )
    no_div_beg = grep("<div", TEXT.keep)
    no_div_end = grep("</div", TEXT.keep)
    while (length(no_div_end) > length(no_div_beg))
    {
      TEXT.keep = TEXT.keep[-no_div_end[length(no_div_end)]]
      no_div_end = no_div_end[-length(no_div_end)]
    }
  }
  
  TEXT.keep = paste0(TEXT.keep, collapse = "\n")
  TEXT.keep = gsub("\"", "\'", TEXT.keep)
  return(TEXT.keep)
}


###################################################################################################################################

factory <- function(fun) {
  # function(...) {
  mess = capture.output(
    assign("res"
           , {
             warn <- err <- NULL
             res <- withCallingHandlers(
               tryCatch(
                 # fun(...),
                 fun
                 , error = function(e) {
                   err <<- conditionMessage(e)
                   NULL
                 }
               )
               , warning = function(w) {
                 warn <<- append(warn, conditionMessage(w))
                 invokeRestart("muffleWarning")
               }
             )
             list(res = res, warn = warn, err = err)
           })
    , type = "message")
  mess = paste0(mess, collapse = "\n")
  return(list(res = res$res
              , mess = mess[which(nchar(mess) > 0)]
              , warn = res$warn
              , err = res$err))
}

print_messages = function(fun, cut_pattern = "STUPID")
{
  out_fun = factory(fun)
  if (length(out_fun$err) > 0)
  {
    sapply(out_fun$err, function(xx) shinyalert(type = "error"
                                                , closeOnClickOutside = TRUE
                                                , text = xx))
    return(0)
  } else
  {
    if (length(out_fun$warn) > 0)
    {
      sapply(out_fun$warn, function(xx) showNotification(xx, type = "warning"))
    }
    if (length(out_fun$mess) > 0)
    {
      sapply(out_fun$mess, function(xx) shinyalert(type = "success"
                                                   , closeOnClickOutside = TRUE
                                                   , text = gsub(cut_pattern
                                                                , paste0(cut_pattern, " ")
                                                                , xx)))
    }
    return(out_fun$res)
  }
}


###################################################################################################################################

get_files.names = function(path_folder, opt.sub_folder = FALSE)
{
  if (dir.exists(path_folder))
  {
    tab_names = list.files(path = path_folder
                           , include.dirs = FALSE
                           , full.names = TRUE
                           , recursive = opt.sub_folder)
    return(tab_names)
  }
}

get_files = function(path_folder, skip.no = 2, opt.sub_folder = FALSE)
{
  tab_names = get_files.names(path_folder = path_folder
                              , opt.sub_folder = opt.sub_folder)
  if (!is.null(tab_names) && length(tab_names) > 0)
  {
    tab = foreach(tab_name = tab_names) %do%
    {
      fread(file = tab_name, header = FALSE, skip = skip.no, sep = "\t")
    }
    if (length(tab) > 1)
    {
      nrows = sapply(tab, nrow)
      nrow_max = max(nrows)
      if (length(which(nrows < nrow_max)) > 0)
      {
        for (i in which(nrows < nrow_max))
        {
          tab[[i]] = rbind(tab[[i]], data.frame(V1 = rep("", nrow_max - nrows[i])))
        }
      }
      tab = do.call(cbind, tab)
    } else
    {
      tab = tab[[1]]
    }
    tab_names = sub("//", "/", tab_names)
    tab_names = sub(path_folder, "", tab_names)
    tab_names = gsub("/", "__", tab_names)
    colnames(tab) = tab_names
    return(tab)
  }
}

###################################################################################################################################

.getParam = function(params.lines
                     , flag
                     , flag.split
                     , is.num = TRUE
){
  

  param.name = params.lines
  params.lines = readLines(params.lines)

  if(flag.split == " "){
    value.line = grep(flag, params.lines, value = TRUE) #params.lines[ind.flag]
    value.line = unlist(strsplit(value.line, split = flag.split))[-1]
  } else {
    ind.flag.split = grep(flag.split, params.lines)
    ind.flag = grep(paste0("--", flag, "--"), params.lines)
    if (length(ind.flag) == 0)
    {
      stop(paste0("Wrong type of data!\n `flag` (", flag, ") is not found within `params.lines` (", param.name, ")"))
    }
    ind.start = which(ind.flag.split == ind.flag)
    if (ind.flag.split[ind.start + 1] == ind.start + 1)
    {
      stop(paste0("Wrong type of data!\n `flag` (", flag, ") does not contain any value"))
    }
    
    ind1 = (ind.flag.split[ind.start] + 1)
    ind2 = ifelse(length(ind.flag.split) == 1
                  , max(length(params.lines), ind1)
                  , ind.flag.split[ind.start + 1] - 1)
    value.line = params.lines[ind1:ind2]
    value.line = as.character(value.line)
  }
  if(is.num){
    value.line = as.numeric(value.line)
  }
  return(value.line)
}

###################################################################################################################################

.getGraphics_theme = function()
{
  return(theme_fivethirtyeight() +
           theme(panel.background = element_rect(fill = "transparent", colour = NA)
                 , plot.background = element_rect(fill = "transparent", colour = NA)
                 , legend.background = element_rect(fill = "transparent", colour = NA)
                 , legend.box.background = element_rect(fill = "transparent", colour = NA)
                 , legend.key = element_rect(fill = "transparent", colour = NA)))
}

###################################################################################################################################

# Create a function that wraps a Shiny input function in code that adds information about the tag type
updateableInput <- function(inputType) {
  function(...) {
    shinyFuncName <- as.character(as.list(match.call()[1]))
    shinyFunc <- get(shinyFuncName, envir = as.environment("package:shiny"))
    shiny::tagAppendAttributes(shinyFunc(...), `data-input-type` = inputType)
  }
}

# define what Shiny inputs you want to support
# (the following three common input types are tested; the code here probably will
# not work as-is for ALL inputs but you should be able to modify it slightly for other inputs)
textInput <- updateableInput("Text")
numericInput <- updateableInput("Numeric")
selectInput <- updateableInput("Select")
checkboxInput  <- updateableInput("Checkbox")

# Update a single Shiny input without specifying its type
updateShinyInput <- function(session, id, value) {
  shinyUpdateInputId <- paste0("shiny-update-input-", id)
  # print(shinyUpdateInputId)
  js$getInputType(id, shinyUpdateInputId)
  # print("yo")
  shiny::observeEvent(session$input[[shinyUpdateInputId]], {
    inputType <- session$input[[shinyUpdateInputId]]
    # print(inputType)
    updateFunc <- sprintf("update%sInput", inputType)
    funcParams <- list(session = session, inputId = id)    
    if (inputType == "Select") {
      funcParams[['selected']] <- value
    } else {
      funcParams[['value']] <- value
    }    
    do.call(updateFunc, funcParams)
  })
}

# Update multiple Shiny inputs simultaneously
updateShinyInputs <- function(session, updates) {
  lapply(names(updates), function(id) {
    updateShinyInput(session, id, updates[[id]])
  })
}

###################################################################################################################################


util.ellipse <- function(mx, my, vx, cxy, vy, coeff) {
  lig <- 100
  epsi <- 1e-10
  x <- 0
  y <- 0
  if (vx < 0) { vx <- 0 }
  if (vy < 0) { vy <- 0 }
  if (vx == 0 && vy == 0) { return(NULL) }
  delta <- (vx - vy) * (vx - vy) + 4 * cxy * cxy
  delta <- sqrt(delta)
  l1 <- (vx + vy + delta)/2
  l2 <- vx + vy - l1
  if (l1 < 0) { l1 <- 0 }
  if (l2 < 0) { l2 <- 0 }
  l1 <- sqrt(l1)
  l2 <- sqrt(l2)
  test <- 0
  if (vx == 0) {
    a0 <- 0
    b0 <- 1
    test <- 1
  }
  if ((vy == 0) && (test == 0)) {
    a0 <- 1
    b0 <- 0
    test <- 1
  }
  if (((abs(cxy)) < epsi) && (test == 0)) {
    if (vx > vy) {
      a0 <- 1
      b0 <- 0
    } else {
      a0 <- 0
      b0 <- 1
    }
    test <- 1
  }
  if (test == 0) {
    a0 <- 1
    b0 <- (l1 * l1 - vx)/cxy
    norm <- sqrt(a0 * a0 + b0 * b0)
    a0 <- a0/norm
    b0 <- b0/norm
  }
  a1 <- 2 * pi/lig
  c11 <- coeff * a0 * l1
  c12 <- (-coeff) * b0 * l2
  c21 <- coeff * b0 * l1
  c22 <- coeff * a0 * l2
  angle <- 0
  for (i in 1:lig) {
    cosinus <- cos(angle)
    sinus <- sin(angle)
    x[i] <- mx + c11 * cosinus + c12 * sinus
    y[i] <- my + c21 * cosinus + c22 * sinus
    angle <- angle + a1
  }
  return(list(x = x, y = y,
              seg1 = c(mx + c11, my + c21, 
                       mx - c11, my - c21),
              seg2 = c(mx + c12, my + c22, 
                       mx - c12, my - c22)))
}


util.ELLIPSE = function(x, y, z){
  z <- z/sum(z)
  m1 <- sum(x * z)
  m2 <- sum(y * z)
  v1 <- sum((x - m1) * (x - m1) * z)
  v2 <- sum((y - m2) * (y - m2) * z)
  cxy <- sum((x - m1) * (y - m2) * z)
  ell <- util.ellipse(m1, m2, v1, cxy, v2, 1)
  return(ell)
}

.getELLIPSE = function(xy, fac) {
  fac = factor(fac)
  dfdistri = as.data.frame(model.matrix( ~ fac - 1))
  dfdistri = t(t(dfdistri) / as.vector(table(fac)))
  
  coox = as.matrix(t(dfdistri)) %*% xy[, 1] # label
  cooy = as.matrix(t(dfdistri)) %*% xy[, 2] # label
  
  pfg = NULL
  DAT = foreach(pfg = colnames(dfdistri), .combine = "rbind") %do% {
    ell = util.ELLIPSE(xy[, 1], xy[, 2], dfdistri[, pfg])
    if(length(ell$x) > 0){
      dat = data.frame(x = ell$x
                       , y = ell$y
                       , xlabel = as.vector(coox[pfg, 1])
                       , ylabel = as.vector(cooy[pfg, 1])
                       , PFG = sub("fac", "", pfg))
    } else {
      dat = data.frame(x = as.vector(coox[pfg, 1])
                       , y = as.vector(cooy[pfg, 1])
                       , xlabel = as.vector(coox[pfg, 1])
                       , ylabel = as.vector(cooy[pfg, 1])
                       , PFG = sub("fac", "", pfg))
    }
    return(dat)
  }
  return(DAT)
}

