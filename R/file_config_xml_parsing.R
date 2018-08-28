#' file_config_xml_parsing
#' 
#' @description This function read file_config.xml file and organize inforamtions in a list
#' 
#' @usage file_config_xml_parsing(inpt_file)
#' 
#' @param inpt_file a list, the file readed with function xmlParse()
#' 
#' @return a list of variables assignemet 
#'  
#' @examples See an example in inst/examples_profile_analysis.R
 

file_config_xml_parsing = function(inpt_file){
  list_inpt = xmlToList(inpt_file)
  if(any(names(list_inpt) == "comment")){
    list_inpt = list_inpt[-which(names(list_inpt) == "comment")]
  }
  
  for(i in 1:length(list_inpt)){
    if(any(names(list_inpt[[i]]) == "comment")){
      list_inpt[[i]] = list_inpt[[i]][-which(names(list_inpt[[i]]) == "comment")]
    }
    # list_inpt[[i]] = list_inpt[[i]][-which(names(list_inpt[[i]]) == "comment")]
  }
  
  for(i in 1:length(list_inpt)){
    for(j in 1:length(list_inpt[[i]])){
      list_inpt[[i]][[j]] = gsub(pattern =  "\"",replacement = "",x = list_inpt[[i]][[j]])
    }
  }
  return(list_inpt)
}