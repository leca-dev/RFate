
###############################################################################
.testParam_notDef = function(param)
{
  if (missing(param) ||
      (length(param) == 1 && is.na(param)) ||
      (!is.factor(param) && (length(param) == 1 && nchar(param) == 0)) ||
      is.null(param) ||
      length(param) == 0)
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}

###############################################################################
.testParam_notChar = function(param)
{
  if (.testParam_notDef(param) ||
      !is.character(param) ||
      is.factor(param) ||
      sum(nchar(param) == 0, na.rm = TRUE) > 0)
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}
.testParam_notChar.m = function(param.n, param)
{
  if (.testParam_notChar(param))
  {
    .stopMessage_beChar(param.n)
  }
}

## ----------------------------------------------------------------------------
.testParam_existFile = function(param)
{
  if (.testParam_notChar(param) ||
      sum(!file.exists(param), na.rm = TRUE) > 0)
  {
    .stopMessage_existFile(param)
  }
}

## ----------------------------------------------------------------------------
.testParam_existFolder = function(param1, param2)
{
  if (.testParam_notChar(param1) ||
      !dir.exists(paste0(param1, "/", param2)))
  {
    .stopMessage_existFolders(deparse(substitute(param1)), param2)
  }
}

###############################################################################
.testParam_NAvalues = function(param)
{
  if (length(which(is.na(param))) > 0)
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}
.testParam_NAvalues.m = function(param.n, param)
{
  if (.testParam_NAvalues(param))
  {
    .stopMessage_NAvalues(param.n)
  }
}

## ----------------------------------------------------------------------------
.testParam_samevalues = function(param)
{
  if (.testParam_NAvalues(param) ||
      length(unique(param)) < length(param))
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}
.testParam_samevalues.m = function(param.n, param)
{
  if (.testParam_samevalues(param))
  {
    .stopMessage_samevalues(param.n)
  }
}

###############################################################################
.testParam_notDf = function(param)
{
  if (missing(param) ||
      !is.data.frame(param))
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}

###############################################################################
.testParam_notNum = function(param)
{
  if (.testParam_notDef(param) ||
      !is.numeric(param))
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}
.testParam_notNum.m = function(param.n, param)
{
  if (.testParam_notNum(param))
  {
    .stopMessage_beNumeric(param.n)
  }
}

## ----------------------------------------------------------------------------
.testParam_notBetween = function(param, value1, value2)
{
  if (.testParam_notNum(param) ||
      sum(param < value1) > 0 || sum(param > value2) > 0)
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}
.testParam_notBetween.m = function(param.n, param, value1, value2)
{
  if (.testParam_notBetween(param, value1, value2))
  {
    .stopMessage_between(param.n, value1, value2)
  }
}

## ----------------------------------------------------------------------------
.testParam_notInteger = function(param)
{
  if (.testParam_notBetween(param, 0, 1e15))
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}
.testParam_notInteger.m = function(param.n, param)
{
  if (.testParam_notInteger(param))
  {
    .stopMessage_beInteger(param.n)
  }
}

## ----------------------------------------------------------------------------
.testParam_notRound.m = function(param.n, param)
{
  if (round(param) != param)
  {
    .warnMessage_beRounded(param.n)
  }
}


###############################################################################
.testParam_notInValues = function(param, inList)
{
  if (.testParam_notDef(param) ||
      sum(!(param %in% inList), na.rm = TRUE) > 0)
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}
.testParam_notInValues.m = function(param.n, param, inList)
{
  if (.testParam_notInValues(param, inList))
  {
    .stopMessage_content(param.n, inList)
  }
}

## ----------------------------------------------------------------------------
.testParam_notColnames = function(param, inList)
{
  .testParam_notInValues(colnames(param), inList)
}

###############################################################################
.testParam_notInClass = function(param, inList)
{
  if (missing(param) ||
      is.null(param) ||
      length(param) == 0 ||
      sum(!(class(param) %in% inList), na.rm = TRUE) > 0)
  {
    return(TRUE)
  } else
  {
    return(FALSE)
  }
}

###############################################################################
.getParam_opt.folder.name = function(param, folder.path, create.dir = TRUE)
{
  if (is.null(param)){
    opt.folder.name = ""
  } else if (!is.null(param) && !is.character(param)){
    warning("As `opt.folder.name` does not contain character value, it will be ignored")
    opt.folder.name = ""
  } else if (nchar(param) > 0){
    opt.folder.name = paste0(param, "/")
    if (create.dir){
      dir.create(paste0(folder.path, opt.folder.name))
    }
  } else {
    opt.folder.name = ""
  }
  return(opt.folder.name)
}

###############################################################################
.getParam_abs.simulParams = function(param, name.simul)
{
  if (.testParam_notDef(param) || nchar(param) == 0)
  {
    abs.simulParams = list.files(paste0(name.simul, "/PARAM_SIMUL/"))
    if (length(abs.simulParams) == 0)
    {
      stop(paste0("Missing data!\n The folder ", name.simul
                  , "/PARAM_SIMUL/ does not contain adequate files"))
    }
  } else
  {
    abs.simulParams = basename(param)
  }
  abs.simulParams = paste0(name.simul, "/PARAM_SIMUL/", abs.simulParams)
  .testParam_existFile(abs.simulParams)
  return(abs.simulParams)
}


