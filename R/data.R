#' Country code conversion tables
#'
#' Codes for territories, institutions, and country groups, including official names
#'
#' @docType data
#'
#' @usage data(dictccode)
#'
#' @format An object of class \code{"data.frame"}. See \code{\link{countrycode_dict}} for explanations
#'
#' @keywords datasets
#'
#' @source \href{https://sdw.ecb.europa.eu/datastructure.do?conceptMnemonic=REF_AREA&datasetinstanceid=143}{ECB codelist},
#' \href{https://unctadstat.unctad.org/en/classifications.html}{UNCTAD codelist},
#' \href{http://www.fao.org/countryprofiles/iso3list/en/}{FAO codelist},
#' \href{https://github.com/xmarquez/WorldValuesSurvey/blob/master/Country codes for WVS wave 6.csv}{WVS codelist},
#' \href{https://wits.worldbank.org/wits/wits/witshelp/content/codes/country_codes.htm}{Worldbank codelist}
#'
#'
#' @examples
#' data(dictccode)
"dictccode"

load('data/dictccode.rda')
