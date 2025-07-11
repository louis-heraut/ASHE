# Copyright 2021-2023 Louis Héraut (louis.heraut@inrae.fr)*1,
#                     Éric Sauquet (eric.sauquet@inrae.fr)*1
#
# *1   INRAE, France
#
# This file is part of ASHE R package.
#
# ASHE R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# ASHE R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ASHE R package.
# If not, see <https://www.gnu.org/licenses/>.



#' @title clean_path
#' @description Sanitizes a character string for safe use as a path or filename.
#' @export
clean_path = function(text) {
    text = iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT")
    text = gsub("'", "_", text)
    text = gsub("[[:space:]]+", "_", text)
    text = gsub("[^a-zA-Z0-9._-]", "", text)
    text = tolower(text)
    text = gsub("^_+|_+$", "", text)
    return(text)
}



## 1. FORMATTING OF DATA _____________________________________________
### 1.1. Joining selection ___________________________________________
# Joins tibbles of different selection of station as a unique one
#' @title Join selection
#' @export
join_selection = function (list_data, list_meta, list_from) {

    nb_selec = length(list_data)
    Blank = TRUE

    Code = c()
    for (i in 1:nb_selec) {
        datatmp = list_data[[i]]
        df_metatmp = list_meta[[i]]
        from = list_from[[i]]
        
        if (!is.null(datatmp)) {
            
            datatmp = datatmp[!(datatmp$code %in% Code),]
            df_metatmp = df_metatmp[!(df_metatmp$code %in% Code),]
            Code = c(Code,
                     df_metatmp$code[!(df_metatmp$code %in% Code)])
            
            df_metatmp$source = from
            
            if (Blank) {
                data = datatmp
                df_meta = df_metatmp
                Blank = FALSE
            } else {
                # Joins tibble
                data = full_join(data, datatmp)
                df_meta = full_join(df_meta, df_metatmp)
            }
        }
    }
    # If there is no data
    if (Blank) {
        stop('No data')
    }
    return (list(data=data, meta=df_meta))
}


## 3. FOLLOWING OF DATA MODIFICATIONS ________________________________
#' @title Add modification info
#' @export
add_mod = function (df_mod, Code, type, fun_name, comment, df_meta=NULL) {
    
    if (Code == 'all' & is.null(df_meta)) {
        Code = NA # erreur
    } else if (Code == 'all' & !is.null(df_meta)) {
        # Get all different stations code
        Code = rle(data$Code)$value
    }
    
    for (code in Code) {
        df_modtmp = tibble(code=code, type=type,
                           fun_name=fun_name,
                           comment=comment)
        df_mod = bind_rows(df_mod, df_modtmp)
    }
    return (df_mod)
}


## 4. CRITICISM OF DATA ______________________________________________
#' @title Add criticism
#' @export
add_critique = function (df_critique, Code, author, level, start_date, variable, type, comment='', end_date=NULL, df_meta=NULL, resdir=NULL) {
    if (Code == 'all' & is.null(df_meta)) {
        Code = NA # erreur
    } else if (Code == 'all' & !is.null(df_meta)) {
        # Get all different stations code
        Code = rle(data$code)$value
    }

    if (is.null(end_date)) {
        end_date = start_date
    }
    
    df_tmp = tibble(code=Code, author=author, level=level,
                    start_date=start_date, end_date=end_date,
                    variable=variable, type=type,
                    comment=comment)
    df_critique = bind_rows(df_critique, df_tmp)

    nc = nrow(df_critique)
    print('Criticism registered')
    print(df_critique[(nc-2):nc,])

    if (!is.null(resdir)) {   
        write_critique(df_critique, resdir)
    }
    
    return (df_critique)
}

# df_critique = add_critique(df_critique, resdir=resdir, Code='', author='louis', level=, start_date=, end_date=NA, variable='', type='', comment='')
