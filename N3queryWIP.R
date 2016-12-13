folder = "./SampleData/NEMSIS v334 Sample Data Set/NEMSIS v334 Sample Data Set/"
files = list.files(folder)[1:100]

folder = "./SampleData/COMPASS/AGENCY1/"
files = list.files(folder)[1:100]

files = "EMSDataset.xml"
folder = "./SampleData/N3SampleData/EMS/xml/"

require(xml2)
require(dplyr)

pcrs = N3toXML_Read(folder,files)
COMPASS_STROKE01(pcrs)
COMPASS_TRAUMA02(pcrs)

## Read XML files containging one or more PCRs
## Could be optimized if you assumed 1 PCR per XML file and
## declared outputs beforehand, or did this with lapply or something
## - The weird list/XMLNode structure I think causes some problems

N3toXML_Read = function(folder,files){
  require(xml2)
  out = vector('list')
  for (i in seq_along(files)){
    temp = read_xml(paste0(folder,files[i]))
      xml_ns_strip(temp)
      temp = xml_find_all(temp, "//PatientCareReport")
      out[length(out):(length(out)+length(temp))] = temp[1:length(temp)] ## kinda slow, but better than c()
    }
  return(out)
}  

## Save some time later on
findXMLtext = function(data, name){
  xml_text(xml_find_all(data, paste0(".//",name)))
}

checkXMLattr = function(data, name, attr){
  xml_has_attr(xml_find_all(data, paste0(".//",name)),attr)
}

## Return vector of dummy variables for each record -
## These could be generalized to a single function accepting
## arguments defining numerator/denominator and dummy variable values

COMPASS_STROKE01 = function(data){
  require(xml2)
  out = rep(NA,length(data)) ## Declare for loop output
  for(i in seq_along(data)) {
  out[i] = ifelse(
    any(c(grepl("I63",findXMLtext(data[[i]],"eSituation.11")),
          grepl("G45",findXMLtext(data[[i]],"eSituation.11")),
          grepl("I63",findXMLtext(data[[i]],"eSituation.12")),
          grepl("G45",findXMLtext(data[[i]],"eSituation.12")))) &
      findXMLtext(data[[i]],"eResponse.05") == "2205001",
      ifelse(
        (length(findXMLtext(data[[i]],"eVitals.29")) > 0 & !all(checkXMLattr(data[[i]],"eVitals.29","nil"))) |
        (length(findXMLtext(data[[i]],"eVitals.30")) > 0 & !all(checkXMLattr(data[[i]],"eVitals.30","nil"))),
        1,0),NA)
  }
  return(out)
}

COMPASS_TRAUMA02 = function(data){
  require(xml2)
  out = rep(NA,length(data)) ## Declare for loop output
  for(i in seq_along(data)) {
    out[i] = ifelse(
      findXMLtext(data[[i]],"eSituation.02") == "	9922005" &
      findXMLtext(data[[i]],"eResponse.05") == "2205001" &
      any(findXMLtext(data[[i]],"eVitals.27") > 0),
      ifelse(
        (length(findXMLtext(data[[i]],"eVitals.27")) > 0 & !all(checkXMLattr(data[[i]],"eVitals.27","nil"))),
        0,1),NA)
  }
  return(out)
}
