
.quiet = function(x)
{
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 

.stopMessage_notDef = function(param)
{
  stop(paste0("No data given!\n (missing `", param, "` information)"))
}

.stopMessage_existFile = function(param)
{
  stop(paste0("Wrong name file given!\n `", param, "` does not exist"))
}

.stopMessage_existFolders = function(param1, param2)
{
  stop(paste0("Wrong name folder given!\n `", param1, "` does not exist or does not contain a ", param2, " folder"))
}

.stopMessage_beDataframe = function(param)
{
  stop(paste0("Wrong type of data!\n `", param, "` must be a data.frame"))
}

.stopMessage_beNumeric = function(param)
{
  stop(paste0("Wrong type of data!\n `", param, "` must contain numeric values"))
}

.stopMessage_beInteger = function(param)
{
  stop(paste0("Wrong type of data!\n `", param, "` must be an integer > 0"))
}

.warnMessage_beRounded = function(param)
{
  warning(paste0("`", param, "` is a double. It will be converted (rounded) to an integer"))
}

.stopMessage_between = function(param, value1, value2)
{
  stop(paste0("Wrong type of data!\n `", param, "` must contain values between `", value1, "` and `", value2, "`"))
}

.stopMessage_beChar = function(param)
{
  stop(paste0("Wrong type of data!\n `", param, "` must contain a character value of length > 0"))
}

.stopMessage_NAvalues = function(param)
{
  stop(paste0("Wrong type of data!\n `", param, "` must not contain NA values"))
}

.stopMessage_samevalues = function(param)
{
  stop(paste0("Wrong type of data!\n `", param, "` must contain different values"))
}


#################################################################################################

.stopMessage_numRowCol = function(param1, param2)
{
  stop(paste0("Wrong dimension(s) of data!\n `", param1
              , "` does not have the appropriate number of rows (>0) or columns ("
              , paste0(param2, collapse = ", "), ")"))
}


.stopMessage_columnNames = function(param1, param2)
{
  if (length(param2) == 1)
  {
    end_message = param2
  } else
  {
    end_message = paste0(paste0(param2[-length(param2)], collapse = "`, `")
                         , "` and `", param2[length(param2)])
  }
  stop(paste0("Wrong type of data!\n Column names of `", param1
              , "` must be `", end_message, "`"))
}


#################################################################################################

.stopMessage_content = function(param1, param2)
{
  if (length(param2) == 1)
  {
    end_message = param2
  } else
  {
    end_message = paste0(paste0(param2[-length(param2)], collapse = "`, `")
                         , "` or `", param2[length(param2)])
  }
  stop(paste0("Wrong type of data!\n `", param1, "` must be either `", end_message, "`"))
}

