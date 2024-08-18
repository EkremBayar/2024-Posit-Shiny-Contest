#' TM Transfer Type
#'
#' TM Transfer Type
#'
#' @param x Transfer Type Column
#'
#' @return a vector or value
#' @importFrom dplyr case_when
#'
.standardize_fee <- function(x) {
  dplyr::case_when(
    grepl("free", x) ~ "Free transfer",
    grepl("end of", x) ~ "End of loan",
    grepl("fee", x) ~ "Paid loan",
    grepl("loan", x) ~ "Loan",
    grepl("m|th.|k", x) ~ "Transfer",
    grepl("draft|Draft", x) ~ "Draft",
    TRUE ~ NA_character_
  )
}




#' TM Empty NA
#'
#' TM Empty NA
#'
#' @param val NA
#'
#' @return a vector or value
#'
.replace_empty_na <- function(val) {
  if(length(val) == 0) {
    val <- NA_character_
  } else {
    val <- val
  }
  return(val)
}


#' TM Convert Market Value to Numeric
#'
#' Convert formatted valuations to numeric Returns a numeric data type for player valuations
#'
#' @param euro_value Market Value Column
#'
#' @return a vector or value
#'
#' @importFrom dplyr %>%
#'
.convert_value_to_numeric <- function(euro_value) {
  clean_val <- gsub("[^\x20-\x7E]", "", euro_value) %>% tolower()
  if(grepl("free", clean_val)) {
    clean_val <- 0
  } else if(grepl("m", clean_val)) {
    clean_val <- suppressWarnings(gsub("m", "", clean_val) %>% as.numeric() * 1000000)
  } else if(grepl("th.", clean_val)) {
    clean_val <- suppressWarnings(gsub("th.", "", clean_val) %>% as.numeric() * 1000)
  }else if(grepl("k", clean_val)){
    clean_val <- suppressWarnings(gsub("k", "", clean_val) %>% as.numeric() * 1000)
  }else {
    clean_val <- suppressWarnings(as.numeric(clean_val) * 1)
  }
  return(clean_val)
}


#' TM Tidy Dates
#'
#' Clean date fields Returns a date format in YYYY-MM-DD from 'mmm d, yyyy'
#'
#' @param dirty_dates Date Column
#'
#' @return a vector or value
#' @importFrom dplyr %>%
#' @importFrom lubridate ymd
#'

.tm_fix_dates <- function(dirty_dates) {

  fix_date <- function(dirty_date) {
    if(is.na(dirty_date)) {
      clean_date <- NA_character_
    } else {
      split_string <- strsplit(dirty_date, split = " ") %>% unlist() %>% gsub(",", "", .)
      if(length(split_string) != 3) {
        clean_date <- NA_character_
      } else {
        tryCatch({clean_date <- lubridate::ymd(paste(split_string[3], split_string[1], split_string[2], sep = "-")) %>%
          as.character()}, error = function(e) {country_name <- NA_character_})
      }
    }

    return(clean_date)
  }
  clean_dates <- dirty_dates %>% purrr::map_chr(fix_date)

  return(clean_dates)
}





#' Player Name Abbreviate
#'
#' Abreviate Long Player Names
#'
#' @param player_name Date Column
#' @param type Type of proccess
#'
#' @return a vector or value
#' @importFrom stringr str_split
#' @importFrom stringr str_sub
#' @importFrom stringr str_split_i
#' @examples
#' \dontrun{
#' player_list <- c("İrfan Can Kahveci", "Fred", "Mert Müldür")
#' .player_name_abbreviate(player_list, type="full_name")
#' .player_name_abbreviate(player_list, type="only_last_name")
#' }
.player_name_abbreviate <- function(player_name, type=c("full_name", "only_last_name")){

  if(missing(player_name) | missing(type)){stop("Please, check arguments! `player_name` and `type`")}
  if(sum(type %in% c("full_name", "only_last_name")) == 0){stop("`type` argument must be `full_name` or `only_last_name`")}
  if(length(type) > 1){stop("`type` argument must be a character not a vector!")}

  if(type == "full_name"){
    player_abb <- stringr::str_split(player_name, " ")
    player_abb <- sapply(player_abb, function(i){
      if(length(i) == 1){
        i
      }else if(length(i) > 1){
        paste0(paste0(stringr::str_sub(i[-length(i)], 1, 1), ".", collapse = ""), i[length(i)])
      }
    })
  }

  if(type == "only_last_name"){
    player_abb <- stringr::str_split_i(player_name, " ", -1)
  }

  return(player_abb)


}


#' Uniform Text
#'
#' Uniform Text
#'
#' @param text a character
#' @param upper logical
#'
#' @return a character
#'
#' @importFrom stringr str_squish
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_to_upper
#' @importFrom stringi stri_trans_general
#'
.uniform_text <- function(text, upper = TRUE){
  res <- stringr::str_replace_all(text, "-", " ")
  res <- gsub('[[:punct:]]+','', res)
  res <- stringr::str_squish(res)
  res <- stringi::stri_trans_general(res, 'latin-ascii')
  if(upper == TRUE){res <- stringr::str_to_upper(res)}
  # Arabian, Chinesei Korean alphabeths etc.
  res <- ifelse(stringr::str_detect(res, "[\\p{Letter}&&\\p{script=latin}]") == FALSE, NA_character_, res)
  return(res)
}

#' Helper Distinct
#'
#' Distinct Data After Joining Process
#'
#' @param data datafrmae
#' @param tidcol1 character
#' @param tidcol2 character
#'
#' @return a dataframe
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang sym
.helper_distinct_join <- function(data, tidcol1, tidcol2, apply = TRUE){
  if(apply){
    data %>%
      dplyr::group_by(!!rlang::sym(tidcol2)) %>%
      dplyr::mutate(n = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!rlang::sym(tidcol1)) %>%
      dplyr::mutate(n2 = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n == 1, n2 == 1) %>%
      dplyr::select(-n, -n2)
  }else{
    data
  }
}


#' Helper Join
#'
#' Helper Join
#'
#' @param Type join type
#' @param by matching columns
#' @param joincol1 matching first column
#' @param joincol2 matching second column
#' @param data1 data1
#' @param data2 data2
#' @param fuzzy_method fuzyy method
#' @param similarity similarity
#' @param regextype regextype
#'
#' @return a dataframe
#'
#' @import dplyr fuzzyjoin
#' @importFrom rlang sym
.helper_join <- function(Type = c("Exact", "Regex", "Fuzzy"), by, joincol1, joincol2, data1, data2, fuzzy_method = NULL, similarity, regextype){

  if(Type == "Exact"){

    res <- dplyr::inner_join(
      data1,
      data2,
      by = by
    ) %>%
      dplyr::mutate(!!rlang::sym(joincol2) := !!rlang::sym(joincol1), Distance = -1, Type = "Exact")

  }else if(Type == "Regex"){

    if(regextype == 1){
      res <- fuzzyjoin::regex_inner_join(
        data1,
        data2,
        by = by
      ) %>%
        dplyr::mutate(Distance = 5, Type = "Regex")
    }else if(regextype == 2){
      res <- fuzzyjoin::regex_inner_join(
        data2,
        data1,
        by = by
      ) %>%
        dplyr::mutate(Distance = 5, Type = "Regex")

    }



  }else if(Type == "Fuzzy"){

    # if(length(intersect(c("cosine", "jw", "jaccard", "soundex"), fuzzy_method) == 0)){stop("fuzzy_method must be one of them: 'cosine', 'jw', 'jaccard', 'soundex'")}

    res <- fuzzyjoin::stringdist_inner_join(
      data1,
      data2,
      by = by,
      max_dist = similarity,
      method = fuzzy_method,
      distance_col = "Distance"
    ) %>%
      dplyr::mutate(Type = "Fuzzy")


  }else{
    stop("Wrong type! The types are 'Exact', 'Regex' and 'Fuzzy'")
  }

  # Rename
  res <- res %>% dplyr::rename(!!rlang::sym(stringr::str_sub(joincol1, 1, -2)) := !!rlang::sym(joincol1), !!rlang::sym(stringr::str_sub(joincol2, 1, -2)) := !!rlang::sym(joincol2))

  return(res)

}

#' Select Specific Columns
#'
#' Select Specific Columns
#'
#' @param data dataframe
#' @param colvec column vector
#'
#' @return dataframe
#'
#' @importFrom dplyr select
#' @importFrom tidyr any_of
.helper_select <- function(data, colvec){data %>% dplyr::select(tidyr::any_of(colvec))}

#' Removing matched data from the main data
#'
#' Removing matched data from the main data
#'
#' @param temp_data dataframe
#' @param res_data dataframe
#' @param idcol id column
#'
#' @return a dataframe
#'
#' @importFrom rlang sym
#' @importFrom dplyr filter
#' @importFrom dplyr pull
.helper_filter <- function(temp_data, res_data, idcol){
  temp_data %>% dplyr::filter(!(!!rlang::sym(idcol)) %in% (res_data %>% dplyr::pull(!!rlang::sym(idcol))))
}

#' Match DOB info to improve joining accuracy
#'
#' Match DOB info to improve joining accuracy
#'
#' @param data dataframe
#' @param pdobcol1 DOB column
#' @param pdobcol2 DOB column
#' @param apply logical
#'
#' @return dataframe
#'
#' @importFrom rlang sym
#' @importFrom dplyr filter
.helper_filter_dob <- function(data, pdobcol1, pdobcol2, apply = TRUE){
  if(apply & !is.null(pdobcol1) & !is.null(pdobcol2)){
    data %>% dplyr::filter(!is.na(!!rlang::sym(pdobcol1)), !is.na(!!rlang::sym(pdobcol2)), as.character(!!rlang::sym(pdobcol1)) == as.character(!!rlang::sym(pdobcol2)))
  }else{
    data
  }
}

#' Match Wrong DOB info to improve joining accuracy
#'
#' Match Wrong DOB info to improve joining accuracy
#'
#' @param data dataframe
#' @param pdobcol1 DOB column
#' @param pdobcol2 DOB column
#' @param apply logical
#'
#' @return dataframe
#'
#' @importFrom rlang sym
#' @importFrom dplyr filter
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate day
.helper_filter_wrong_dob <- function(data, pdobcol1, pdobcol2, apply = FALSE){
  if(apply & !is.null(pdobcol1) & !is.null(pdobcol2)){
    data %>%
      # Remove NA
      dplyr::filter(!is.na(!!rlang::sym(pdobcol1)), !is.na(!!rlang::sym(pdobcol2))) %>%
      dplyr::filter(
        # Year & Month same, Day wrong
        (lubridate::year(!!rlang::sym(pdobcol1)) == lubridate::year(!!rlang::sym(pdobcol2)) & lubridate::month(!!rlang::sym(pdobcol1)) == lubridate::month(!!rlang::sym(pdobcol2)) & lubridate::day(!!rlang::sym(pdobcol1)) != lubridate::day(!!rlang::sym(pdobcol2))) |
          # Month & Day same, Year wrong +-1
          (abs(lubridate::year(!!rlang::sym(pdobcol1)) - lubridate::year(!!rlang::sym(pdobcol2))) == 1 & lubridate::month(!!rlang::sym(pdobcol1)) == lubridate::month(!!rlang::sym(pdobcol2)) & lubridate::day(!!rlang::sym(pdobcol1)) == lubridate::day(!!rlang::sym(pdobcol2))) |
          # Year & Day same, Month wrong
          (lubridate::year(!!rlang::sym(pdobcol1)) == lubridate::year(!!rlang::sym(pdobcol2)) & lubridate::month(!!rlang::sym(pdobcol1)) != lubridate::month(!!rlang::sym(pdobcol2)) & lubridate::day(!!rlang::sym(pdobcol1)) == lubridate::day(!!rlang::sym(pdobcol2))) |
          # Only year same
          (lubridate::year(!!rlang::sym(pdobcol1)) == lubridate::year(!!rlang::sym(pdobcol2)) & !!rlang::sym(pdobcol1) != !!rlang::sym(pdobcol2))
      )
  }else{
    data
  }
}



#' Execute Join
#'
#' Execute Join
#'
#' @param result empty tibble
#' @param data1 dataframe1
#' @param data2 dataframe2
#' @param combination_cols all possible name columns
#' @param pidcol1 id column
#' @param pdobcol1 dob column
#' @param pposcol1 position column
#' @param pnatcol1 nationality column
#' @param pidcol2 id column
#' @param pdobcol2 dob column
#' @param pposcol2 position column
#' @param pnatcol2 nationality column
#' @param joinbycheck check join by
#' @param regextype define regex type
#' @param jointype define join type
#' @param apply_helper_filter_dob logical TRUE/FALSE
#' @param apply_helper_filter_wrong_dob logical TRUE/FALSE
#' @param fuzzy_method fuzzy method
#' @param similarity numeric 0-1
#'
#' @return a dataframe
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @importFrom stringr str_sub
#'
.helper_exec <- function(
    # Dataframes
  result, data1, data2,
  # Combinations Cols
  combination_cols,
  # Other Columns
  pidcol1, pdobcol1, pposcol1, pnatcol1, pidcol2, pdobcol2, pposcol2, pnatcol2,
  # Join Check
  joinbycheck,
  regextype,
  # Join Type Exact, Regex, Fuzzy
  jointype,
  # Apply Filter
  apply_helper_filter_dob = FALSE,
  apply_helper_filter_wrong_dob = FALSE,
  # If fuzzy
  fuzzy_method = NULL, similarity = NULL
){

  for(combind in 1:nrow(combination_cols)){

    if(nrow(data1) > 0 & nrow(data2) > 0){
      # Select Columns
      joincol1 <- as.character(combination_cols[combind, 1])
      joincol2 <- as.character(combination_cols[combind, 2])

      # Join Columns
      if(joinbycheck == 1){
        joinbychecks <- setNames(joincol2, joincol1)
      }else if(joinbycheck == 2){
        joinbychecks <- setNames(joincol1, joincol2)
      }

      # Select Columns
      temp_data11 <- data1 %>% .helper_select(colvec = c(pidcol1, joincol1, pdobcol1, pposcol1, pnatcol1))
      temp_data22 <- data2 %>% .helper_select(colvec = c(pidcol2, joincol2, pdobcol2, pposcol2, pnatcol2))

      # Inner Join
      result <- dplyr::bind_rows(
        result,
        .helper_join(jointype, by = joinbychecks, joincol1, joincol2, temp_data11, temp_data22, fuzzy_method, similarity, regextype)
      ) %>%
        .helper_distinct_join(pidcol1, pidcol2, apply = TRUE) %>%
        .helper_filter_dob(pdobcol1, pdobcol2, apply = apply_helper_filter_dob) %>%
        .helper_filter_wrong_dob(pdobcol1, pdobcol2, apply = apply_helper_filter_wrong_dob) %>%
        dplyr::filter(!is.na(!!rlang::sym(stringr::str_sub(joincol1, 1, -2))), !is.na(!!rlang::sym(stringr::str_sub(joincol2, 1, -2)))) %>%
        suppressWarnings()

      # Remove Matched Players
      data1 <- .helper_filter(data1, result, pidcol1)
      data2 <- .helper_filter(data2, result, pidcol2)

    }else{
      break
    }

  }

  return(list("result" = result, "data1" = data1, "data2" = data2))

}


#' .helper_final_match_data
#'
#' .helper_final_match_data
#'
#' @param data dataframe
#'
#' @return dataframe
#'
#' @importFrom dplyr setequal
.helper_final_match_data <- function(data){

  collist <- c("SBPlayerId", "TMPlayerId", "SBPlayer", "TMPlayer", "SBDOB", "TMDOB", "Distance", "Type")

  if(!dplyr::setequal(intersect(names(data), names(collist)), names(collist))){
    data$SBDOB <- NA_Date_
    data$TMDOB <- NA_Date_
  }

  return(data[, collist])

}


#' .html_nodes_regex
#'
#' .html_nodes_regex
#'
#' @param html html
#' @param node_name character
#' @param attr character
#' @param regex_type character
#'
#' @return html
#'
#' @importFrom rvest html_nodes
#'
#' @examples
#' \dontrun{
#' # Reading the HTML page of the Premier League
#' url <- "https://fbref.com/en/comps/9/Premier-League-Stats"
#' page <- rvest::read_html(url)
#' # Starts with
#' page %>% .html_nodes_regex(node_name = "all_stats_squads_", attr = "id", regex_type = "startswith")
#' # Contains
#' page %>% .html_nodes_regex(node_name = "squads_standar", attr = "id", regex_type = "contains")
#' # Ends with
#' page %>% .html_nodes_regex(node_name = "_for", attr = "id", regex_type = "endswith")
#' }
.html_nodes_regex <- function(html, node_name, attr, regex_type = c("equal","startswith", "contains", "endswith")){

  #https://developer.mozilla.org/en-US/docs/Web/CSS/Pseudo-classes
  #https://medium.com/yonder-techblog/css-regex-attribute-selectors-98075b7f4726

  # Checks
  if(missing(node_name)){stop("`node_name` cannot be missing!")}
  if(missing(attr)){stop("`attr` cannot be missing!")}
  if(missing(regex_type)){stop("`regex_type` cannot be missing!")}
  if(!is.character(node_name)){stop("The class of `node_name` has to be character!")}
  if(!is.character(attr)){stop("The class of `node_name` has to be character!")}
  if(!is.character(regex_type)){stop("The class of `node_name` has to be character!")}
  if(length(regex_type %in% c("equal","startswith", "contains", "endswith")) != 1){
    stop("`regex_type` has to be one of them: `equal`, `startswith`, `contains` or `endswith`!")
  }

  # Regex Type
  regex_type_check <- switch(regex_type,
                             equal = "",
                             startswith = "^",
                             contains = "*",
                             endswith = "$",
                             stop("Unknown `regext_type!` Type must be `equal`, `startswith`, `contains` or `endswith`", call. = FALSE)
  )

  query <- paste0("[", attr, regex_type_check, "=", node_name, "]")


  html %>% rvest::html_nodes(query)

}




#' .fbref_names_corrected
#'
#' .fbref_names_corrected
#'
#' @param df dataframe
#'
#' @return colnames

#'
.fbref_names_corrected <- function(df){
  res <- unlist(ifelse(names(df) != "", paste0(names(df), "_", df[1,]), df[1,]))
  return(res)
}


#' .fbref_stats_categories
#'
#' .fbref_stats_categories
#'
#' @return vector

.fbref_stats_categories <- function(){
  res <- c(
    "standard", "shooting", "passing", "passing_types", "gca", "defense",
    "possession", "playing_time", "misc", "keeper", "keeper_adv"
    )
  return(res)
}


