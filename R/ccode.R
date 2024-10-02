#setwd('F:/countrycode')
#dictccode = read.csv('countrycode_dictionary.csv',
#         header=TRUE,row.names = NULL,stringsAsFactors=FALSE,
#         blank.lines.skip = TRUE, na.strings = c('',':'))

#check https://unctadstat.unctad.org/en/classifications.html for UN codes


#' Get a vector of country group (e.g. euro area) members
#'
#' @param sgroup the code of the group in Eurostat or ECB terms. E.g. 'EU', 'OECD' or 'EA19' (See Details). Note: sgroup is a singleton, thus only the first element of any vector is taken.
#' @param time group members at that specific  year (as integer, like 2012) or date (as date or string like '2017-05-23'). If left empty, then current composition is returned. Note: time needs to be a singleton (only its first element is taken).
#' @param ccodeas The format in which to return the members (such as \code{'iso2m'}, \code{'iso3c'} or \code{'ec'}). If left empty, then uses \code{\link{defaultcountrycode}}, resp. 'iso2m'
#' @param fail if TRUE, requesting a non-existing sgroup generates an error. If FALSE, this would return a zero-length character instead
#' @return a character vector with the country codes of group members currently (if \code{time} is unspecified), respectively with members as of the end of the period specfied in \code{time}
#' @details See \code{\link{ccode}} for permissible values for ccode.
#' cgrp currently has variants for the following groups: euro area (EA, alias U2), European Union (EU, alias D0), OECD (A8), and EFTA (A5);
#' The following shortcuts can also be used: EA11, EA12, EA17, EA18, EA19, EU12, EU15, EU25, EU27, EU28 respectively their ECB equivalents (D2, V3, V1, D3, EU12, I1, I2, I6, I7, I8)
#' @seealso \code{\link{ccode}} for converting between country code conventions, \code{\link{countrycode_dict}} to set and change your own country groups
#' @examples
#' cgrp("EA") #euro area members
#' cgrp(EA)   #also works without quotes
#' cgrp("EA", 2002) #euro area Members in 2012
#' cgrp('U2', 2002) #same, using ECB instead of Estat code for euro area
#' cgrp('EA12') #shortcut: euro area with 12 Members
#' cgrp('EU','2004-01-31') #EU Members at the start of 2004 (excludes 'new' member states)
#'
#' cgrp('EFTA', 1993)
#' cgrp('EFTA', 1995) #three countries had left EFTA as of 1995
#'
#'
#' cgrp('EFTA', 1994, "name")
#'
#' cgrp('OECD', 2000, ccodeas="iso3c") #OECD countries in 2000
#' @export
cgrp=function (sgroup = c("EA", "EU", "OECD", "EFTA"),
                time = NULL, ccodeas = defaultcountrycode(), fail = TRUE)
{
  if (missing(sgroup)) {
    if (fail)
      stop("sgroup needs to be specified. The following are available: ", paste(countrycode_dict$groupget(),collapse=", "))
    else return(character(0))
  }
  tempx=try(as.character(sgroup)[[1]],silent=TRUE)
  if (grepl('err',class(tempx))) {
    sgroup=gsub("\\s", "", deparse(substitute(sgroup)))
  } else {
    sgroup=toupper(as.character(sgroup[[1]]))
  }
  if (match(sgroup,countrycode_dict$groupget(),nomatch = 0)<1) {
    sgroup=ccode(sgroup,'iso2m','iso2m')
  }

  if (is.null(ccodeas)) {
    ccodeas = "iso2m"
  }
  if (missing(time)) {
    timep = Sys.Date()
  }
  else {
    timep = time[[1]]
  }
  if (is.numeric(timep)|(is.character(timep) & nchar(timep)==4L)) {
    timep = (paste0(timep, "-01-01"))
  }
  timep = try(as.Date(timep), silent = TRUE)
  if (grepl("error", class(timep)))
    stop("you specified time=", time, ". Yet time needs to be a year number like 1997, date string like 2013-04-07, or date")
  membvec = countrycode_dict$groupget(sgroup)
  if (is.null(membvec)) {
    if (fail)
      stop("Country group \"", sgroup, "\" not available. The following are available: ", paste(countrycode_dict$groupget(),collapse=", "))
    else return(character(0))
  }
  nmembvec = names(membvec)
  membvec = as.Date(membvec)
  names(membvec) = nmembvec
  membvec = names(membvec[!(membvec > timep)])
  mleavers = gsub("!", "", membvec[grep("^!",
                                        membvec)])
  if (length(mleavers)) {
    membvec = membvec[!(membvec %in% mleavers)]
    membvec = membvec[-grep("!", membvec)]
  }
  return(ccode(membvec, "iso2c", ccodeas))
}


#' Converting country codes
#'
#' Converts country codes (e.g. 3-letter to 2-letter), as well as from and to country names.
#' Strongly inspired by the \code{\link[countrycode]{countrycode}} package
#'
#' @param sourcevar vector which contains the codes or country names to be converted (character or numeric)
#' @param origin Coding scheme of origin (case-insensitive string enclosed in quotes ""):  'iso2m', 'iso2c','iso3c', 'ecb', 'ec', 'name'(=name.en), 'name.fr', 'name.de', 'iso3n', 'un', 'wb', 'imf', 'fao', 'fips105', 'icao', 'ioc', 'cowc', 'ameco', 'oecd', 'wits', 'ccy'
#'     If left unspecified, then the function will guess whether the source convention is one of 'iso2m', 'iso3c', 'name' or 'iso3n'
#' @param destination Coding scheme of destination (case-insensitive string enclosed in quotes ""): all of the above, plus 'name.en_estat',  'name.en_ecb', 'continent', 'region', 'sovereign'
#' @param warn Warns if elements from sourcevar could not be matched
#' @param custom_dict A data frame which supplies custom country codes.
#'   Variables correspond to country codes, observations must refer to unique countries. See \code{countrycode_dict()$get()} for an example
#' @param custom_match retained for compatibility with countrycode package. Not operational at the moment.
#' @param origin_regex retained for compatibility with countrycode package. Not operational at the moment.
#' @param leaveifNA if FALSE then matches that cannot be converted are signalled as \code{NA}.
#'   If TRUE, then  matches that cannot be converted are left as provided in \code{sourcevar}
#' @param strict Boolean. If FALSE, then 'old' codes are converted to new ones in a fuzzy manner. \code{strict=TRUE} applies strict matching to entities, and ignores 'unoffical' codes. See Disclaimer for details.
#' @param verbose Boolean. If TRUE, then the function posts messages ans warnings about things that look odd
#' @return a string vector with converted country codes/names
#' @details There are four types of entities: existing countries and territories, former political entities (such as Yugoslavia), inter- and supra-national institutions and organisations (such as the IMF), and country groups (such as the euro area).
#'
#'  The following code tables are available (for \code{origin} and \code{destination})
#' \itemize{
#'   \item iso2m (to and from): ISO 2-letter country codes (\code{FR} for France). Complemented with Estat memnonics for country groups (such as \code{EU} for the EU) and institutions (such as \code{EIB} for the EIB)
#'   \item iso2c (to and from): ISO 2-letter country codes (\code{FR} for France). Complemented with ECB codes for country groups (format LETTER-DIGIT) and institutions (format DIGIT-LETTER)
#'   \item iso3c (to and from): ISO 3-letter country codes (\code{FRA} for France). Complemented with ECB codes for country groups (format LETTER-DIGIT such as D0 for the EU) and institutions (format DIGIT-LETTER such as 4C for the EIB)
#'   \item ecb (to and from): ECB convention very similar to \code{iso2c} (such as \code{FR} for France). Some peculiarities for entities that have ceased to exist (e.g. Yugoslavia). See \href{https://sdw.ecb.europa.eu/datastructure.do?conceptMnemonic=REF_AREA&datasetinstanceid=143}{ECB codelist}.
#'   \item ec (to and from, alias eurostat): European Commission convention very similar to \code{iso2m}, i.e. ISO 2-letter country codes (\code{FR} for France) with an exception for Greece ('EL' instead of 'GR') and the UK ('UK' instead of 'GB'). See \href{https://webgate.ec.europa.eu/sdmxregistry/}{SDMX codelist registry}
#'   \item sovereign (to): The existing sovereign state to which a territory belongs, as recognized by the majority of UN Member States (i.e, Britain, Jersey, and Bermuda all yield 'UK' as sovereign). For former entities, sovereign denotes the successor with the largest population (e.g. Russia in the case of the USSR)
#'   \item name.en (to and from, alias 'name' and 'country.name'): short English language name. Uses regular expression to convert from names (e.g. Belgium to BE)
#'   \item name.fr (to, alias country.name.fr): French-language name, as defined by Eurostat
#'   \item name.de (to and from,, alias country.name.de): German-language name, as defined by Eurostat. also uses regular taken from the countrycode package
#'   \item name.en_estat (to): English language name as defined by Eurostat
#'   \item name.en_ecb (to): English language name as defined by ECB
#'   \item continent (to): Continent of a territory. Reads 'Organisation' for entities like 'IMF' and 'CountryGroup' for entities like 'EU'
#'   \item region (to): continental sub-region of a territory, as defined by the Worldbank. Type of organization or group (International or European) for institutions and country groups
#'   \item iso3n (to and from): ISO 3-digit numerical code
#'   \item un (to and from): UN numerical code (similar to iso3n, but with agggregates). See \href{https://unctadstat.unctad.org/en/classifications.html}{UNCTAD codelist}.
#'   \item wb (to and from): Worldbank 3-letter code (variant of iso3c). See \href{https://wits.worldbank.org/wits/wits/witshelp/content/codes/country_codes.htm}{Worldbank codelist}.
#'   \item wvs (to and from): World Values Survey numeric code (similar to iso3n). See \href{https://github.com/xmarquez/WorldValuesSurvey/blob/master/Country codes for WVS wave 6.csv}{WVS codelist}.
#'   \item fao (to and from): FAO numeric code. See \href{http://www.fao.org/countryprofiles/iso3list/en/}{FAO codelist}
#'   \item fips105 (to and from): FIPS 10-4 (Federal Information Processing Standard), similar to iso2c
#'   \item icao (to and from): ICAO (int'l civil air organization) code (when to: only first one relevant for a country is shown, when from: matches to largest population country within the ICAO area)
#'   \item imf (to and from): IMF numeric code
#'   \item ioc (to and from): Int'l Olympic Comittee 3-letter code
#'   \item cowc (to and from): Correlates of War character code
#'   \item cown (to and from): Correlates of War numeric code
#'   \item ameco (to and from): AMECO database 3-letter code (variant of iso3c)
#'   \item oecd (to and from): OECD 3-letter code (variant of iso3c)
#'   \item ilo (to and from): Int'l Labor Organization 3-letter code (variant of iso3c)
#'   \item wits (to and from): 3-letter codes from Worldbank's World Integrated Trade Solution (variant of iso3c)
#'   \item ccy (to and from): 3-letter currency codes according to ISO 4217, such as \code{JPY} for Japan. Note that old currency codes such as FRF for French Franc are also matched.
#' }
#'
#' @section Disclaimer:
#'
#' Note that there is absolutely no guarantee that the codes contained herein  conform to official code lists by the mentioned institutions. The code lists may not necessarily represent the views of
#' any institution regarding the status of territories included in, or excluded from, this package.
#' The code tables aim to represent the sovereignty status of entities as recognized by the majority of UN Member States, but accuracy is not guaranteed.
#' For creating your own deviations, adjustments, and additions to the correspondence table, see \code{\link{countrycode_dict}}.
#'
#' The parameter \code{strict} tries to emulate official correspondence tables (accuracy not guaranteed): If strict=TRUE, certain entities are not matched under correspondence tables (e.g. Kosovo for \code{un}).
#' In addition, with strict=TRUE, old codes are mostly matched to old codes: \code{iso3n} code \code{278} for Western Germany is matched to Western Germany, rather than to modern Germany (which has code \code{280}).
#' In contrast \code{strict=FALSE} fills in unofficial codelist extensions for disputed territories, and matches old to new codes where the continuation is clear (e.g. codes for pre-1991 Ethiopia (incl. Eritrea) are matched to modern Ethiopia.)
#'
#' @section Relationship to countrycode package:
#'
#' This function is inspired by Vincent Arel-Bundock's \code{countrycode}  package.
#' The arguments of \code{ccode} are made to be consistent with those of the packages \code{countrycode}  function.
#' This function can be considered a fork of the countrycode packages, putting more emphasis on macroeconomic aspects, in particular:
#' \itemize{
#' \item Capturing small territories, also by regex
#' \item Adding institutions and country group codes
#' \item Adding alternative code matching
#' \item Reflecting the latest name and code changes etc. (e.g. "North Macedonia")
#' }
#'
#'
#' @seealso \code{\link{cgrp}} for getting country group members,
#'  \code{\link{defaultcountrycode}} for setting a default destination,
#'  \code{\link{countrycode_dict}} for altering the conversion table,
#'  \code{\link[countrycode]{countrycode}} for the original inspiration
#'
#' @examples
#' ccode('Belgium','name','iso2m')
#'
#' x1= c("France","Guadeloupe","Latvia","Guinea-Bissau","Euro area 19","Greece")
#' x2=ccode(x1,'name','iso2c')
#' print(x2)
#' x3=ccode(x2,'iso2c','ec') #note the code change for euro area and Greece
#' print(x3)
#'
#'
#' ccode(x2,'iso2c','sovereign', warn=FALSE) #Note the code change for Guadeloupe. There is no sovereign country to which the euro area belongs.
#' ccode(x2,'iso2c','sov', warn=FALSE, leaveifNA=TRUE) #leave the euro area code intact
#'
#' ccode(x3,'iso2c','iso2c') #even though Greece was named 'EL' rather than 'GR', ccode can deal with that
#'
#'
#'ccode('CA','iso2m','ccy') #Canada has Canadian dollars
#'
#' @export
ccode = function (sourcevar, origin=NULL, destination=defaultcountrycode(), warn = TRUE, custom_dict = NULL,
          custom_match = NULL, origin_regex = FALSE,leaveifNA = FALSE, strict=FALSE, verbose=TRUE)
{
  if (!missing(custom_match)) warning('custom_match currently notimplemented. Please use package countrycode for that.')
  if (!missing(origin_regex)) warning('origin_regex currently notimplemented. Please use package countrycode for that.')
  if (is.null(custom_dict)) { custom_dict = countrycode_dict$get() }
  if (is.null(destination)) {
    if (missing(destination)) stop("you need either to provide argument 'destination' or set a default country code like defaultcountrycode('iso2c')")
    stop('argument destiantion cannot be NULL')
  }

  sourcevarorig=sourcevar; destinorig=destination; origorig=origin
  if (missing(custom_dict)) { ddict = countrycode_dict$get() } else { ddict = as.data.frame(custom_dict,stringsAsFactors=FALSE)}
  colnames(ddict) = tolower(colnames(ddict))
  destination=destination[[1]]
  permissibleFROM = !sapply(ddict,function(x) anyDuplicated(na.omit(grep('%',x,perl=TRUE,invert = TRUE,value = TRUE))))
  permissibleFROM[grep('^name',colnames(ddict))]=FALSE
  permissibleFROM[grep('^regex',colnames(ddict))]=TRUE

  destination = gsub('^regex','name',gsub("^country\\.name","name",trimws(tolower(destination))))
  if (destination=='name') { destination='name.en'}


  if (!length(origin)) {
    tryit=""
    vecnchar=nchar(utils::head(sourcevarorig,100))
    if (is.numeric(sourcevarorig)) tryit ='iso3n' else
    if (!anyNA(suppressWarnings(as.numeric(utils::head(sourcevarorig,100))))) tryit='iso3n' else
    if (mean(vecnchar==2)>.8) tryit = 'iso2m' else
    if (mean(vecnchar==3)>.8) tryit = 'iso3c' else
    if (mean(vecnchar>5)>.8) tryit = 'country.name'
    if (tryit=="") {stop('origin not specfied. Could not guess what it relates to. See help(ccode)')}
    if (verbose) message('origin not specficed. Trying ',tryit)
    origin=tryit
  }
  origin = gsub('^name','regex',gsub("^country\\.name","name",trimws(tolower(origin))))
  origin =origin[[1]];
  if (origin=='regex') { origin='regex.en'}
  if (origin %in% c('eurostat','estat')) {origin='ec'}
  if (destination %in% c('eurostat','estat')) {destination='ec'}
  if (is.na(match(origin,colnames(ddict)))) { origin=gsub('^regex','name',origin) }
  destination=colnames(ddict)[pmatch(destination,colnames(ddict))]
  origin     =colnames(ddict)[pmatch(origin,colnames(ddict))]
  if (is.na(origin)) { stop('origin code ', origorig, ' not available') }
  if (is.na(destination)) { stop('destination code ', destinorig, ' not available', ifelse(missing(destination),'Please check your defaultcountrycode option','')) }

  if (!origin %in% colnames(ddict)) { stop('origin code "', origin, '" cannot be found in dictionary')}
  if (!(origin %in% colnames(ddict)[permissibleFROM])) { stop('Code ', origin, ' only permissible as destination, not as origin')}

  if (missing(sourcevar)) {
    stop("sourcevar is NULL (does not exist).")
  }
  if (!mode(sourcevar) %in% c("character", "numeric")) {
    stop("sourcevar must be a character or numeric vector. This error often\n arises when users pass a tibble (e.g., from dplyr) instead of a\ncolumn vector from a data.frame (i.e., my_tbl[, 2] vs. my_df[, 2]\nvs. my_tbl[[2]])")
  }
  #if (mode(sourcevar) == 'character') sourcevar = tolower(sourcevar)

  if (!destination %in% colnames(ddict)) { stop('origin code "', destination, '" cannot be found in dictionary')}

  vdest=gsub('%','',ddict[[destination]])
  if (is.character(vdest))  vdest = sapply(strsplit(vdest, split = "\\|"), "[", 1L)
  vout=rep(NA_character_,length(sourcevar))


  sourceisnumber=FALSE; if (is.numeric(sourcevar)) {sourceisnumber=TRUE} #to adjust for leading zeros
  sourcevar = tolower(sourcevar)
  if (!grepl('^regex',origin)) {
    temp=strsplit(tolower(ddict[[origin]]),split="\\|")


    for (i in seq_len(max(sapply(temp,length),na.rm=TRUE))) {
      vtemp=sapply(temp,'[',i)
      if (sourceisnumber) if (suppressWarnings(sum(is.na(as.integer(vtemp[!grepl('%',vtemp)])))==sum(is.na(vtemp[!grepl('%',vtemp)])))) {
        vtemp0=suppressWarnings(as.character(as.integer(vtemp)))
        vtemp0[is.na(vtemp0)] = vtemp[is.na(vtemp0)]; vtemp=vtemp0; rm(vtemp0)
      }  #to adjust for leading zeros
      if (strict) { vtemp=gsub('%','',vtemp); vtemp[vtemp %in% vtemp[duplicated(vtemp)]]=NA }

      vconv=vdest[!is.na(vtemp)]; names(vconv) = na.omit(vtemp)
      vout[is.na(vout)] = vconv[sourcevar][is.na(vout)]
      if (strict) {break}
    }
  } else {
    sourcevar=trimws(tolower(sourcevar),whitespace = '\\s')
    for (i in 1:nrow(ddict)) {
      matches <- grep(ddict[[origin]][i], sourcevar, perl = TRUE,
                      ignore.case = TRUE, value = FALSE)
      vout[matches] <- vdest[i]
    }
  }

  if (anyNA(vout)) {
    if (warn) { warning(sum(is.na(vout)), ' codes could not be matched.')}
    if (leaveifNA) { vout[is.na(vout)] = sourcevarorig[is.na(vout)] }
  }

  return(vout)
}




#' Set the default country code to be converted to
#'
#' @param x the default 'destination' argument to function \code{ccode}.
#' @return if x is not specified, then the function returns the current default country code. If x is specfied, the function sets the countrycode to x (and returns it invisibly)
#' @details See \code{\link{ccode}} for permissible values for x. Commonly used values are:
#' \itemize{
#' \item \code{"iso2c"} for 2-letter country codes ('FR' for France), with letter-digit combinations for country groups  (\code{U2} for Euro area)
#' \item \code{"iso2m"} for 2-letter country codes ('FR' for France), with mnemnonics for country groups  (\code{EA19} for Euro area 19)
#' \item \code{"ec"} for Eurostat/EC-style two-letter codes (EL instead of GR, UK instead of GB, otherwise like \code{iso2c}) and mnemonics for country groups (\code{EA} for Euro area)
#' \item \code{"iso3c"} for 3-letter country codes ('FRA' for France), with letter-digit combinations for country groups  (\code{U2} for Euro area)
#' }
#' @seealso \code{\link{ccode}}, \code{\link{cgrp}}
#' @examples
#' defaultcountrycode("iso2m") #set default country code to ISO 2-letters
#'
#' ccode("Sudan","name") # here 'destination' does not have to be specfied in ccode
#' ccode("FRA","iso3c") # similar
#'
#' defaultcountrycode() #shows current default country code
#'
#' defaultcountrycode(NULL) # unsets default countrycode
#'
#' @export
defaultcountrycode =function(x)  {
  if (missing(x)) { return(getOption('defaultcountrycode'))}
  if (!length(x)) { options("defaultcountrycode"=NULL); return(invisible(NULL))}

  x=x[[1]];
  if (!tolower(x) %in% tolower(colnames(countrycode_dict$get()))) stop('argument x needs to be ',paste(colnames(countrycode_dict$get())[1:5],collapse=", "), " or the like. \nSee help(ccode) for permissible values.")
  options(defaultcountrycode=x)
  return(invisible(x))
}




.countrycode_dict = function(myurl='https://s-ecfin-web/directorates/db/u1/R/routines/countrycode_dictionary.csv') {
  intdict = NULL
  loadDict <- function(withload=FALSE, from=myurl) {
    #intdict <<- .readLinesSpecial(myurl,timeout=0.5)$value
    #browser()
    if (is.null(intdict)) {
      if (exists('dictccode')) {
        intdict <<- get('dictccode')
      } else if (!withload) {
        intdict <<- MDcountrycode::dictccode
      }
    }
    if (withload) {{
      if (is.null(from)) {from=myurl}
      if (is.data.frame(from)) {
        intdict <<- suppressWarnings(try(from,silent=TRUE))
      } else {
        intdict <<- suppressWarnings(try(read.csv(from,
                                                header=TRUE,row.names = NULL,stringsAsFactors=FALSE,
                                                blank.lines.skip = TRUE, na.strings = c('',':')),silent=TRUE))
      }
      if (grepl('error',class(intdict))) {
        message("Updating countrycode dictionary to latest version did not succeed.")
        intdict <<- get('dictccode')
      } else {
        message("Successfully updated countrycode dictionary to latest version.")
        for (i in intersect(c('iso3n','un'),colnames(intdict))) {
          temp=suppressWarnings(as.integer(intdict[[i]]))
          temp[!is.na(temp)] = sprintf('%03d',temp[!is.na(temp)])
          temp[is.na(temp)] = intdict[[i]][is.na(temp)]
          intdict[[i]] <<- temp
        }; rm(temp,i)
      }
    }}
    intdict <<-intdict[!is.na(intdict[,1]),]
    rownames(intdict) <<- intdict[,'ecb']
  }

  .cgrpdict = list(
    EA=c(BE ='1999-01-01',DE ='1999-01-01',EE='2011-01-01',IE ='1999-01-01',GR='2001-01-01',ES ='1999-01-01',
         FR ='1999-01-01',HR='2023-01-01',IT ='1999-01-01',CY='2008-01-01',LV='2014-01-01',LT='2015-01-01',LU ='1999-01-01',
         MT='2008-01-01',NL ='1999-01-01',AT ='1999-01-01',PT ='1999-01-01',SI='2007-01-01',SK='2009-01-01',
         FI ='1999-01-01'),

    EU=c(BE='1958-01-01', BG='2007-01-01', CZ='2004-05-01', DK='1973-01-01', DE='1958-01-01', EE='2004-05-01',
         IE='1973-01-01', GR='1981-01-01', ES='1986-01-01', FR='1958-01-01', HR='2013-07-01', IT='1958-01-01',
         CY='2004-05-01', LV='2004-05-01', LT='2004-05-01', LU='1958-01-01', HU='2004-05-01', MT='2004-05-01',
         NL='1958-01-01', AT='1995-01-01', PL='2004-05-01', PT='1986-01-01', RO='2007-01-01', SI='2004-05-01',
         SK='2004-05-01', FI='1995-01-01', SE='1995-01-01', GB='1973-01-01',"!GB"='2020-01-31'),

    OECD=c(CA='1961-04-10',US='1961-04-12',GB='1961-05-02',DK='1961-05-30',IS='1961-06-05',NO='1961-07-04',
           TR='1961-08-02',ES='1961-08-03',PT='1961-08-04',FR='1961-08-07',IE='1961-08-17',BE='1961-09-13',
           DE='1961-09-27',GR='1961-09-27',SE='1961-09-28',CH='1961-09-28',AT='1961-09-29',NL='1961-11-13',
           LU='1961-12-07',IT='1962-03-29',JP='1964-04-28',FI='1969-01-28',AU='1971-06-07',NZ='1973-05-29',
           MX='1994-05-18',CZ='1995-12-21',HU='1996-05-07',PL='1996-11-22',KR='1996-12-12',SK='2000-12-14',
           CL='2010-05-07',SI='2010-07-21',IL='2010-09-07',EE='2010-12-09',LV='2016-07-01',LT='2018-07-05',
           CO='2020-04-28',CR='2021-05-25'),

    EFTA=c(AT='1960-05-03', DK='1960-05-03', FI='1960-05-03', NO='1960-05-03', PT='1960-05-03', SE='1960-05-03',
           CH='1960-05-03', GB='1960-05-03', IS='1970-01-01', LI='1991-01-01', "!DK"='1973-01-01', "!GB"='1973-01-01',
           "!PT"='1986-01-01',"!AT"='1995-01-01',"!FI"='1995-01-01',"!SE"='1995-01-01'),

    ASEAN=c(BN='1984-01-07', KH='1999-04-30', ID='1967-08-08', LA='1997-07-23', MY='1967-08-08',
            MM='1997-07-23', PH='1967-08-08', SG='1967-08-08', TH='1967-08-08', VN='1995-07-28'),

    G20 = c(AR='1999-09-26', AU='1999-09-26', BR='1999-09-26', CA='1999-09-26', CN='1999-09-26', FR='1999-09-26',
            DE='1999-09-26', IN='1999-09-26', ID='1999-09-26', IT='1999-09-26', JP='1999-09-26', KR='1999-09-26',
            MX='1999-09-26', RU='1999-09-26', SA='1999-09-26', ZA='1999-09-26', TR='1999-09-26', GB='1999-09-26',
            US='1999-09-26', D0='1999-09-26'),

    OPEC = c(DZ='1969-01-01', AO='2007-01-01', CG='2018-01-01', EC='1973-01-01', '!EC'='1992-01-01','EC'='2007-01-01',
             '!EC'='2020-01-01', GQ='2017-01-01', GA='1975-01-01', '!GA'='1995-01-01', GA='2016-01-01', IR='1960-01-01',
             IQ='1960-01-01', KW='1960-01-01', LY='1962-01-01', NG='1971-01-01', SA='1960-01-01', AE='1967-01-01',
             VE='1960-01-01', ID='1962-01-01', '!ID'='2008-01-01', QA='1961-01-01', '!QA'='2019-01-01')
  )

  grpsubset=function(mycode,refyear) {
    temp=.cgrpdict[[mycode]][substr(.cgrpdict[[mycode]],0,4)<=as.character(refyear)]
    temp[]='1901-01-01'
    temp
  }

  .cgrpdict[['EU28']]= grpsubset('EU',2018)
  .cgrpdict[['EU27']]= grpsubset('EU',2021)
  .cgrpdict[['EU25']]= grpsubset('EU',2006)
  .cgrpdict[['EU15']]= grpsubset('EU',1996)
  .cgrpdict[['EU12']]= grpsubset('EU',1992)
  .cgrpdict[['EA11']]= grpsubset('EA',1999)
  .cgrpdict[['EA12']]= grpsubset('EA',2003)
  .cgrpdict[['EA17']]= grpsubset('EA',2014)
  .cgrpdict[['EA18']]= grpsubset('EA',2015)
  .cgrpdict[['EA19']]= grpsubset('EA',2022)
  .cgrpdict[['EA20']]= grpsubset('EA',2023)
  .cgrpdictaliases=names(.cgrpdict);
  #names(.cgrpdictaliases) = ccode(.cgrpdictaliases,"ec","ecb",warn = FALSE,
  #                                leaveifNA = TRUE,custom_dict = dictccode)
  names(.cgrpdictaliases) =.cgrpdictaliases

  provccodes=list(ECB=list(dims="_AREA$",src="iso2c"), EUROSTAT = list(dims="geo", src="iso2c"), AMECO=list(dims="country",src="ameco"), OECD=list(dims="cou$|location$",src="oecd"))


  return(list(
    set=function(x) {if (length(x)==1L) {loadDict(TRUE,x)} else {intdict<<-x}},
    get=function() { if (is.null(intdict)) loadDict(); return(intdict) },
    update = function(srccsv=NULL) {loadDict(TRUE,from = srccsv); return(invisible(intdict))},
    groupget = function(x) {
      if (missing(x)) {return(names(.cgrpdict))}
      x=trimws(as.character(toupper(x[[1]])),whitespace = '\\s')
      if (!(x %in% names(.cgrpdict))) x=.cgrpdictaliases[x];
      if (is.na(x)) return(NULL)
      .cgrpdict[[x]]
    },
    groupset = function(x,value) {
      if (!is.character(value)) stop('value needs to be a character vector')
      if (!is.null(names(value)) && !anyDuplicated(names(value))) {
        .cgrpdict[[x]] <<- value;
        names(.cgrpdict[[x]]) <<- toupper(names(.cgrpdict[[x]]));
      } else {
        .cgrpdict[[x]] <<- rep('1901-01-01',length(value));
        names(.cgrpdict[[x]]) <<- toupper(value);
      }

      .cgrpdictaliases<<-names(.cgrpdict);
      names(.cgrpdictaliases) <<- ccode(names(.cgrpdict), 'iso2m','ecb',leaveifNA = TRUE,warn=FALSE)
    },
    ccodes=function(x) {
      if (missing(x)) return(provccodes)
      provccodes <<- x
    }
  )
  )
}

#' conversion table for country codes, editable
#'
#' @details the countrycode_dict object holds the conversion table for countrycodes, and the country group lists
#' The object has the following subfunctions:
#' \itemize{
#'   \item \code{countrycode_dict$get()} data.frame with country code conversion table
#'   \item \code{countrycode_dict$set(x)} set the data.frame with country code conversion table equal to x
#'   \item \code{countrycode_dict$update(srccsv=NULL)} update \code{countrycode_dict} to the latest country code conversion table from internet. Either this is a character path/URL to a CSV, or a data.frame
#'   \item \code{countrycode_dict$groupget(x)} get the character vector of members for country group x (e.g. x='EA')
#'   \item \code{countrycode_dict$groupset(x, value)} set the character vector of members for country group x (e.g. x='EA') equal to value
#' }
#'
#'
#' @section How country codes are stored and can be adjusted:
#'
#' The data.frame returned by \code{countrycode_dict$get()} holds column names that conform to the code groups detailed in \code{\link{ccode}}.
#' converting from one column to the other (e.g. from ECB to iso3c) looks up which line corresponds to the wanted code in the origin column, and then
#' returns the code from the same line in the destination column. I.e. \code{ccode('FR','ecb','iso3c')} finds 'FR' in column ECB and consequently finds
#' 'FRA' in the same line, column \code{iso3c}.
#'
#'
#' If there are multiple (mostly deprecated) unique codes for an origin, the data.frame stores these separated by \code{|}, such as 'ROU|ROM' for Romania.
#' Only the first one of these is used for finding destination codes.
#' This way \code{ccode('ROU','iso3c','name')} returns 'Romania' just like \code{ccode('ROM','iso3c','name')}. Yet \code{ccode('Romania','name','iso3c')} always returns 'ROU'.
#'
#'
#' If there are multiple (mostly deprecated) codes for a target, then all but one of these are appended by %, such as EUR% for Montenegro's currency.
#' This way \code{ccode('Montenegro','name','ccy')} returns EUR, but \code{ccode('EUR','ccy','name')} returns 'Euro area'.
#'
#'
#' Names are a special type of column: any column name like \code{name.XX} is paired with a column name like \code{regex.XX}. XX typically
#' corresponds to a language code such as 'en'.
#' If \code{name.XX} is the destination column (As in \code{ccode('FR','ecb','name.en')}), things are as before.
#' But if \code{name.XX}  is the origin (as in \code{ccode('France','name','ecb')}) than instead \code{regex.XX} is applied to find the right line.
#' Thus \code{ccode(c('China incl. Hong Kong','Taiwan, province of China'),'name','iso2c')} returns \code{CN} and \code{TW}.
#'
#'
#'
#' @section How country group members are stored and can be adjusted:
#'
#'  Each country group used in \code{\link{cgrp}} is a named character vector, such as \code{c('CA'='1995-01-01','US'='1995-01-01','MX'='1995-01-01')}.
#'  The name of each element is a 2-letter country code (ECB/iso2c format).
#'  The value of each element is the entry data of the country into the country group (as a character like '1999-04-26').
#'  The leaving a country group (like Brexit) is denoted by prefixing the country code with an exclamation mark, as in \code{'!GB'='2020-01-31'}.
#'
#'  You can get the named vector for a group like this: \code{countrycode_dict$groupget('ASEAN')}
#'  You can set the named vector for a (new) group like this: \code{'NAFTA',c('CA'='1995-01-01','US'='1995-01-01','MX'='1995-01-01'))}
#'  Note that you can use shorthand as well: \code{countrycode_dict$groupset('NAFTA',c('CA','US','MX'))} assumes all NAFTA members joined in 1901
#'
#' @seealso \code{\link{ccode}}, \code{\link{cgrp}}
#'
#'
#' @examples
#' mydict= countrycode_dict$get()
#' mydict["AT","name.en"] = "Republic of Austria" #change country name for Austria
#' countrycode_dict$set(mydict) #update the conversion table to a custom-made version
#' ccode("Austria","name","name")
#'
#' #countrycode_dict$update() #update the dictionary files to the latest version
#'
#' #how to set a country group:
#' countrycode_dict$groupset('NAFTA',c('CA','MX','US'))
#' #now use the group
#' cgrp('NAFTA')
#'
#'
#' @export
countrycode_dict=.countrycode_dict()



.codestandardguess = function(sourcevar,permitted=c("iso2c","iso3c","name.en")) {
  sourcevar=utils::head(sourcevar,200)
  if ("name.en" %in% permitted) if (mean(nchar(sourcevar),na.rm=TRUE)>5L) {
    tempn=ccode(sourcevar,origin = "name.en","iso2c",warn = FALSE, leaveifNA = FALSE)
    if (sum(!is.na(tempn)) > length(tempn)*.5) {return("name.en")}
  }
  for (ff in c("iso2c","iso3c","imf","iso3n")) {
    if (ff %in% permitted) {
      tempn=ccode(sourcevar,origin = ff,"iso2c",warn = FALSE, leaveifNA = FALSE)
      if (sum(!is.na(tempn)) > length(tempn)*.51) {return(ff)}
    }
  }
  return(character(0))
}



.fixcountrycode = function(d1d, provider=NULL, tocode=getOption("defaultcountrycode"), cols2fix=NULL,
                           permsrc=c("iso2c", "iso3c", "name.en", "iso3n", "imf")) {
  #this tries to guess where countrycodes are and what standard they are, and fixes them to the desired standard
  #d1d: a data.frame or list with one or several columns/elems to fix; or a vector
  #provider:  helps to identify the origin country code
  if (is.null(tocode)) return(d1d)
  if (!is.list(d1d) & is.null(dim(d1d))) { d1d=as.data.frame(d1d,stringsAsFactors = FALSE) }

  if (is.null(cols2fix) & length(provider)) {
    if (toupper(provider) %in% toupper(names(countrycode_dict$ccodes()))) {
      cols2fix = grep(countrycode_dict$ccodes()[[toupper(provider)]]$dims,names(d1d),ignore.case = TRUE)
      permsrc = countrycode_dict$ccodes()[[toupper(provider)]]$src
    }
  }
  if (!length(cols2fix)) { cols2fix=names(d1d)[grep("^[A-z]",names(d1d))] }
  for (mycol in cols2fix) {
    if (length(permsrc)!=1L) convfrom = .codestandardguess(sample(d1d[[mycol]],200L,replace = TRUE),permsrc) else convfrom=permsrc
    if (length(convfrom)) d1d[[mycol]] =  ccode(d1d[[mycol]],convfrom,tocode,leaveifNA = TRUE,warn=FALSE)
  }
  #try(rownames(d1d) <- d1d[[1]],silent=TRUE)
  return(d1d)

}
