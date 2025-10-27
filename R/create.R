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


# Regroups functions to generation of generical station selection
# according to pre-existing directory of data or specific other
# selection format as '.docx' file from Agence de l'eau Adour-Garonne.
# Also useful to create the data and the metadata present in the
# Banque Hydro files from a selection of station. Manages also
# shapefiles loading.



### 1. GENERAL METADATA ON STATION ___________________________________
# Status of the station
#' @title Status info
#' @export
iStatut = function () {
    c('0'='inconnu', 
      '1'='station avec signification hydrologique', 
      '2'='station sans signification hydrologique', 
      '3'="station d'essai")
}

# Goal
#' @title Goal of measure info
#' @export
iFinalite = function () {
    c('0'='inconnue', 
      '1'="hydrométrie générale", 
      '2'='alerte de crue', 
      '3'="hydrométrie générale et alerte de crue",
      '4'="gestion d'ouvrage", 
      '5'='police des eaux', 
      '6'="suivi d'étiage", 
      '7'='bassin expérimental', 
      '8'='drainage')
}

# Type of measure
#' @title Type of measure info
#' @export
iType = function () {
    c('0'='inconnu',
      '1'='une échelle',
      '2'='deux échelles, station mère',
      '3'='deux échelles, station fille',
      '4'='débits mesurés',
      '5'='virtuelle')
}

# Influence of the flow
#' @title Influence info
#' @export
iInfluence = function () {
    c('0'='inconnue',
      '1'='nulle ou faible',
      '2'='en étiage seulement',
      '3'='forte en toute saison')
}

# Type of flow
#' @title Débit info
#' @export
iDebit = function () {
    c('0'='reconstitué',
      '1'=paste("réel (prise en compte de l'eau rajoutée ",
                "ou retirée du bassin selon aménagements)",
                sep=''),
      '2'='naturel')
}

# Quality of low water flow
#' @title Qualité basse eau info
#' @export
iQBE = function () {
    c('0'='qualité basses eaux inconnue',
      '1'='qualité basses eaux bonne',
      '2'='qualité basses eaux douteuse')
}

# Quality of mean water flow
#' @title Qualité moyenne eau info
#' @export
iQME = function () {
    c('0'='qualité moyennes eaux inconnue',
      '1'='qualité moyennes eaux bonne',
      '2'='qualité moyennes eaux douteuse')
}

# Quality of high water flow
#' @title Qualité haute eau info
#' @export
iQHE = function () {
    c('0'='qualité hautes eaux inconnue',
      '1'='qualité hautes eaux bonne',
      '2'='qualité hautes eaux douteuse')
}

# Hydrological region
#' @title Region info
#' @export
iRegHydro = function () {
    c('D'='Affluents du Rhin',
      'E'="Fleuves côtiers de l'Artois-Picardie",
      'A'='Rhin',
      'B'='Meuse',
      'F'='Seine aval (Marne incluse)',
      'G'='Fleuves côtiers haut normands',
      'H'='Seine amont',
      'I'='Fleuves côtiers bas normands',
      'J'='Bretagne',
      'K'='Loire',
      'L'='Loire',
      'M'='Loire',
      'N'='Fleuves côtiers au sud de la Loire',
      'O'='Garonne',
      'O0'='Garonne',
      'O1'='Garonne',
      'O2'='Garonne',
      'O3'='Tarn-Aveyron',
      'O4'='Tarn-Aveyron',
      'O5'='Tarn-Aveyron',
      'O6'='Tarn-Aveyron',
      'O7'='Lot',
      'O8'='Lot',
      'O9'='Lot',
      'P'='Dordogne',
      'Q'='Adour',
      'R'='Charente',
      'S'="Fleuves côtiers de l'Adour-Garonne",
      'U'='Saône',
      'V'='Rhône',
      'W'='Isère',
      'X'='Durance',
      'Y'='Fleuves côtiers du Rhône-Méditérannée et Corse',
      'Z'='Îles',
      '1'='Guadeloupe',
      '2'='Martinique',
      '5'='Guyane',
      '6'='Guyane',
      '7'='Guyane',
      '8'='Guyane',
      '9'='Guyane',
      '4'='Réunion')
}


## 2. SELECTION ______________________________________________________
### 2.0. Copy a selection of station _________________________________
#' @title Copy a selection of station
#' @export
copy_selection = function (select_dir, select_file, from_dir, to_dir,
                           codeCol='code', sep=',', quote='"',
                           optname='_HYDRO_QJM',
                           codeSwipe=NULL,
                           codeRm=NULL) {

    # Creates it if it does not exist
    if (!(file.exists(to_dir))) {
        dir.create(to_dir, recursive=TRUE)
        # If it already exists it deletes the pre-existent directory
        # and recreates one
    } else {
        unlink(to_dir, recursive=TRUE)
        dir.create(to_dir)
    }
    
    select_path = file.path(select_dir, select_file)
    # Creates the data as a data frame
    Code = read.table(select_path,
                      header=TRUE,
                      encoding='UTF-8',
                      sep=sep,
                      quote=quote)[codeCol]
    # Stores it in the tibble of selection
    Code = as.character(Code[[1]])

    if (!is.null(codeSwipe)) {
        nSwipe = length(codeSwipe)
        for (swipe in codeSwipe) {
            Code[Code == swipe[1]] = swipe[2]
        }
    }

    if (!is.null(codeRm)) {
        Code = Code[!(Code %in% codeRm)]
    }
    
    file_list = paste0(Code, optname, '.txt')
    path_list = file.path(from_dir, file_list)
    file.copy(path_list, to_dir)
}

# copy_selection(
#     computer_data_path, 'RRSE_METROPOLE.csv',
# '/home/louis/Documents/bouleau/INRAE/CDD_stationnarite/data/BanqueHydro_Export2021/',
#     file.path(computer_data_path, 'RRSE'),
#     codeCol='Num',
#     codeSwipe=list(c("H78335XX", "H7833520"),
#                    c("H21220XX", "H2122010")),
#     codeRm=c("H2342020", "J8433010"))

# missing : K0010010, H78335XX, K7414010, H21220XX
# H78335XX becomes H7833520
# H21220XX becomes H2122010
# H2342020 removed (too short period)
# J8433010 removed (too short period)


### 2.1. Creation of a selection _____________________________________
#' @title Create a '.txt' selection of station
#' @description Create a txt file that resume all the station data
#' files present in a filedir
#' @param computer_data_path Path to the data.
#' @param filedir Directory of Banque HYDRO data you want to use in
#' ASHE\\computer_data_path\\ to get station codes. If "" is
#' use, data will be search in ASHE\\computer_data_path\\.
#' @param outname Name of the created selection file.
#' @param optname Optional name that will be added to the selected
#' station codes to match names of the Banque HYDRO files
#' (default: '_HYDRO_QJM').
#' @return Writes a selection '.txt' file of data filename of
#' stations.
#' @export
create_selection = function (computer_data_path, filedir, outname,
                             optname='_HYDRO_QJM') {

    # Out file for store results
    outfile = file.path(computer_data_path, outname)
    
    # Path to find the directory of desired codes 
    dir_path = file.path(computer_data_path, filedir)
    # Create a filelist of all the filename in the above directory
    filelist_tmp = list.files(dir_path)
    # Create a filelist to store all station codes
    codelist = c()

    # For all the filename in the file list
    for (f in filelist_tmp) {
        # If the filename is a 'txt' file
        if (tools::file_ext(f) == 'txt') {
            # Creates the station code
            code = gsub("[^[:alnum:] ].*$", '', f)
            # Then the station code is stored
            codelist = c(codelist, code)
        }
    }  
    # Create a tibble to store the data to write
    filename = paste(codelist, optname, '.txt', sep='')
    # Write the data in a txt file
    write.table(filename, outfile, sep=";", row.names=FALSE, col.names=FALSE, quote=FALSE)
    # Returns that it is done with the path
    print('Done')
    print(paste('path : ', outfile, sep=''))
    print('example of file : ')
    print(filename)
}
# Example
# create_selection(
#     "/home/louis/Documents/bouleau/INRAE/CDD_stationnarite/ASHE/data",
#     "AEAG_selection",
#     "selection.txt")

### 2.2. TXT selection _____________________________________________
#' @title Get selection of a '.txt' file
#' @description Gets the selection of station from the selection txt
#' file generated by the 'create_selection' function
#' @param computer_data_path Path to the data.
#' @param listdir Directory of the selection '.txt' file in
#' ASHE\\computer_data_path\\.
#' @param listname Name of the selection '.txt' file.
#' @return A vector of string containing file names of stations.
#' @export
get_selection_TXT = function (computer_data_path, listdir, listname) {
    
    # Gets the file path to the data
    list_path = file.path(computer_data_path, listdir, listname)
    # Creates the data as a data frame
    filename = read.table(list_path,
                          header=FALSE,
                          encoding='UTF-8',
                          sep=';')
    # Stores it in the tibble of selection
    filename = as.character(filename[[1]])
    return (filename)
}
# Example
# df_selec_TXT = get_selection_TXT(
# "/home/louis/Documents/bouleau/INRAE/CDD_stationnarite/ASHE/data",
# "",
# "selection.txt")

### 2.3. Agence de l'eau Adour-Garonne selection _____________________
#' @title Get selection of a '.docx' file
#' @description Gets the selection of station from a formated '.docx'
#' file.
#' @param computer_data_path Path to the data.
#' @param listdir Directory of the selection '.docx' file in
#' ASHE\\computer_data_path\\.
#' @param listname Name of the selection '.docx' file.
#' @param code_nameCol Name of the column of the file containing code
#' stations
#' @param choice_nameCol Name of the column of the file containing the
#' choice if the associated station is selected or not. If set to
#' 'NULL', all stations in the 'code_nameCol' are selected.
#' @param choice_Val Vector of string meaning that the station is
#' selected.
#' @param optname Optional name that will be added to the selected
#' station codes to match names of the Banque HYDRO files
#' (default: '_HYDRO_QJM').
#' @return A vector of string containing file names of stations.
#' @export
get_selection_DOCX = function (computer_data_path, listdir, listname,
                               code_nameCol='code',
                               choice_nameCol='Choix',
                               choice_Val=c('A garder', 'Ajout'), 
                               optname='_HYDRO_QJM') {
    
    # Gets the file path to the data
    list_path = file.path(computer_data_path, listdir, listname)

    # Reads and formats the docx file
    sample_data = read_docx(list_path)
    content = docx_summary(sample_data)
    table_cells = content %>% filter(content_type == "table cell")
    table_data = table_cells %>% filter(!is_header) %>% select(row_id,
                                                               cell_id,
                                                               text)
    # Splits data into individual columns
    splits = split(table_data, table_data$cell_id)
    splits = lapply(splits, function(x) x$text)
    # Combines columns back together in wide format
    df_selec = bind_cols(splits)

    # Stores the first line as column names
    cname = df_selec[1,]
    # Removes the first line
    df_selec = df_selec[-1,]
    # Names the columns
    names(df_selec) = cname

    # If there is a column for choosing if the code is taken or not
    if (!is.null(choice_nameCol)) {
        # Performs the selection according to the column of choice and
        # values representing a positive choice
        selec = (df_selec[[choice_nameCol]] %in% choice_Val)
        df_selec = df_selec[selec,]
    }

    # Creates the filename list
    filename = paste(df_selec[[code_nameCol]], optname, '.txt',
                     sep='')
    return (filename)
}
# Example
# df_selec_DOCX = get_selection_DOCX(
# "/home/louis/Documents/bouleau/INRAE/CDD_stationnarite/ASHE/data",
# "",
# "Liste-station_RRSE.docx")


## 3. CREATEION _____________________________________________________
#' @title Convert regexp in selection
#' @export
convert_regexp = function (computer_data_path, filedir,
                           filename, obs_format) {
    # Get all the filename in the data directory selected
    filelist_dir = list.files(file.path(computer_data_path,
                                        filedir))

    filelist = c()
    for (f in filename) {
        # Get the file path to the data
        file_path = file.path(computer_data_path, filedir, f)
        if (file.exists(file_path)) {
            filelist = c(filelist, f) 
        } else {
            filelist_tmp = filelist_dir[grepl(f, filelist_dir)]
            if (length(filelist_tmp) == 0) {
                filelist_tmp = f
            }
            filelist = c(filelist,
                         filelist_tmp)
        }
    }
    Code = gsub(paste0(gsub("[_]", "[_]", obs_format), ".*$"),
                "", filelist)
    # Code = filelist
    return (Code)
}

### 3.1. Creation of metadata ______________________________________
create_data_HYDRO3_hide = function (i, paths,
                                    variable_to_load,
                                    value_to_keep,
                                    verbose) {
    path = paths[i]
    if (verbose) {
        print(paste0(i, " -> ", round(i/length(paths)*100, 1), "%"))
    }
    
    code = gsub("[_].*", "", basename(path))
    data = read.table(path,
                      header=TRUE,
                      na.strings=c("     -99", " -99.000"),
                      sep=";",
                      skip=55)
    data = dplyr::tibble(data)

    if (!is.null(value_to_keep)) {
        data = dplyr::mutate(data,
                             code=code,
                             date=as.Date(as.character(data$Date),
                                          format="%Y%m%d"),
                             Qls=as.numeric(Qls),
                             Qmmj=as.numeric(Qmmj),
                             Qm3s=as.numeric(Qls)*1E-3,
                             !!names(value_to_keep):=
                                 get(names(value_to_keep)),
                             .keep="used")
        Ok = data[[names(value_to_keep)]] != value_to_keep
        data$Qls[Ok] = NA
        data$Qmmj[Ok] = NA
        data$Qm3s[Ok] = NA
        
    } else {
        data = dplyr::mutate(data,
                             code=code,
                             date=as.Date(as.character(data$Date),
                                          format="%Y%m%d"),
                             Qls=as.numeric(Qls),
                             Qmmj=as.numeric(Qmmj),
                             Qm3s=as.numeric(Qls)*1E-3,
                             .keep="used")
    }

    data = dplyr::select(data, dplyr::all_of(c("date", "code",
                                               variable_to_load)))
    all_na =
        rowSums(is.na(
            dplyr::select(data,
                          dplyr::all_of(variable_to_load)))) ==
        length(variable_to_load)
    
    if (!all(all_na)) {
        first_non_na = which(!all_na)[1]
        last_non_na = which(!all_na)[length(which(!all_na))]
        data = data[first_non_na:last_non_na, ]
        
        if (length(variable_to_load) == 1) {
            data = dplyr::rename(data, Q=dplyr::all_of(variable_to_load))
        }
    } else {
        data = dplyr::tibble()
    }
    
    return (data)
}

create_meta_HYDRO3_hide = function (i, paths,
                                    hydrological_region_level,
                                    verbose) {

    path = paths[i]
    if (verbose) {
        print(paste0(i, " -> ", round(i/length(paths)*100, 1), "%"))
    }
    
    metatxt = c(readLines(path, n=55, encoding="UTF-8"))

    meta =
        dplyr::tibble(
                   code=trimws(substr(metatxt[20], 39,
                                      nchar(metatxt[20]))),
                   code_site=trimws(substr(metatxt[12], 39,
                                           nchar(metatxt[12]))),
                   code_hydro2=trimws(substr(metatxt[24], 39,
                                             nchar(metatxt[24]))),
                   name=trimws(substr(metatxt[21], 39,
                                      nchar(metatxt[21]))),
                   territory=trimws(substr(metatxt[23], 39,
                                           nchar(metatxt[23]))),
                   producer=trimws(substr(metatxt[8], 39,
                                          nchar(metatxt[8]))),

                   XL93_m=as.numeric(substr(metatxt[27], 39, 47)),
                   YL93_m=as.numeric(substr(metatxt[27], 51, 59)),
                   XL2_m=as.numeric(substr(metatxt[28], 39, 47)),
                   YL2_m=as.numeric(substr(metatxt[28], 51, 59)),
                   lon_deg=as.numeric(substr(metatxt[29], 39, 47)),
                   lat_deg=as.numeric(substr(metatxt[29], 51, 59)),
                   surface_km2=as.numeric(substr(metatxt[30], 39, 47)),
                   elevation_m=as.numeric(substr(metatxt[31], 39, 47)),

                   XL93_INRAE_m=as.numeric(substr(metatxt[27], 63, 72)),
                   YL93_INRAE_m=as.numeric(substr(metatxt[27], 76, 84)),
                   XL2_INRAE_m=as.numeric(substr(metatxt[28], 63, 72)),
                   YL2_INRAE_m=as.numeric(substr(metatxt[28], 76, 84)),
                   lon_INRAE_deg=as.numeric(substr(metatxt[29], 63, 72)),
                   lat_INRAE_deg=as.numeric(substr(metatxt[29], 76, 84)),
                   surface_INRAE_km2=as.numeric(substr(metatxt[30], 51, 59)),
                   elevation_INRAE_m=as.numeric(substr(metatxt[31], 51, 59)),

                   start=as.Date(substr(metatxt[41], 39, 48)),
                   end=as.Date(substr(metatxt[41], 52, 61)),

                   gap_pct=as.numeric(substr(metatxt[42], 39, 45)),
                   
                   creation_date=Sys.Date(),
                   creation_date_origin=as.Date(trimws(substr(metatxt[2], 39,
                                                              nchar(metatxt[2])))),
                   path=path)
    
    meta$surface_km2[(meta$surface_km2) <= 0] = NA
    # meta$elevation_m[(meta$elevation_m) < 0] = NA

    Ltmp = names(iRegHydro())[nchar(names(iRegHydro())) == 2]
    Ltmp = substr(Ltmp, 1, 1)
    infoSecteur = rle(sort(Ltmp))$values
    
    oneL = substr(meta$code, 1, 1)
    twoL = substr(meta$code, 1, 2)
    RH = c()
    for (i in 1:length(oneL)) {
        if (oneL[i] %in% infoSecteur & hydrological_region_level == 2) {
            RHtmp = iRegHydro()[twoL[i]]
        } else {
            RHtmp = iRegHydro()[oneL[i]]
        }
        RH = c(RH, RHtmp)
    }
    
    meta$hydrological_region = RH
    meta = dplyr::relocate(meta, hydrological_region, .before=territory)

    return (meta)
}

#' @title create_data_HYDRO3
#' @export
create_data_HYDRO3 = function (paths,
                               variable_to_load=c("Qm3s", "Qls", "Qmmj"),
                               value_to_keep=NULL,
                               verbose=FALSE) {
    data = lapply(1:length(paths),
                  create_data_HYDRO3_hide,
                  paths=paths,
                  variable_to_load=variable_to_load,
                  value_to_keep=value_to_keep,
                  verbose=verbose)
    data = purrr::reduce(.x=data,
                         .f=dplyr::bind_rows)
    return (data)
}

#' @title create_meta_HYDRO3
#' @export
create_meta_HYDRO3 = function (paths,
                               hydrological_region_level=1,
                               verbose=FALSE) {
    meta = lapply(1:length(paths),
                  create_meta_HYDRO3_hide,
                  paths=paths,
                  hydrological_region_level=hydrological_region_level,
                  verbose=verbose)
    meta = purrr::reduce(.x=meta,
                         .f=dplyr::bind_rows)
    return (meta)
}

#' @title create_HYDRO3
#' @export
create_HYDRO3 = function (paths,
                          variable_to_load=c("Qm3s", "Qls", "Qmmj"),
                          value_to_keep=NULL,
                          hydrological_region_level=1,
                          verbose=FALSE) {

    if (verbose) print("--- CREATE DATA ---")
    data = create_data_HYDRO3(paths,
                              variable_to_load=variable_to_load,
                              value_to_keep=value_to_keep,
                              verbose=verbose)
    if (verbose) print("--- CREATE META ---")
    meta = create_meta_HYDRO3(paths,
                              hydrological_region_level=hydrological_region_level,
                              verbose=verbose)

    if (length(variable_to_load) == 1) {
        variable_to_load = "Q"
    }

    stat = dplyr::summarise(dplyr::group_by(data, code),
                            start_adjust=min(date),
                            end_adjust=max(date),
                            dplyr::across(dplyr::all_of(variable_to_load),
                                          ~sum(is.na(.x))/dplyr::n()*100,
                                          .names="gap_{.col}_adjust_pct"))

    meta = dplyr::full_join(meta, stat, by="code")
    meta = dplyr::relocate(meta, start_adjust, .after=start)
    meta = dplyr::relocate(meta, end_adjust, .after=end)
    meta = dplyr::relocate(meta, dplyr::ends_with("_adjust_pct"),
                           .after=gap_pct)
    
    res = list(data=data, meta=meta)
    return (res)
}

# dirpath = "/home/lheraut/Documents/INRAE/data/HYDRO/2024-09-XX/RRSE"
# Paths = list.files(dirpath, pattern=".txt", full.names=TRUE)
# value_to_keep = NULL #c(Val_I=1)
# variable_to_load = "Qm3s" # c("Qm3s", "Qls", "Qmmj")
# hydrological_region_level = 1
# path = Paths[grepl("J421451001", Paths)]
# data = create_data_HYDRO3(path, variable_to_load, value_to_keep)
# meta = create_meta_HYDRO3(path, hydrological_region_level)
# res = create_HYDRO3(path, variable_to_load, value_to_keep, hydrological_region_level)


#' @title Create metadata
#' @description Creation of metadata of stations.
#' @param computer_data_path Path to the data.
#' @param filedir Directory of Banque HYDRO data you want to use in
#' ASHE\\computer_data_path\\ to get station codes. If "" is
#' use, data will be search in ASHE\\computer_data_path\\.
#' @param filename String or vector of string of all filenames from
#' which metadata will be created. If set to 'all', all the file in
#' 'fildir' will be use.
#' @param verbose Boolean to indicate if more processing info are
#' printed (default : TRUE).
#' @return A tibble containing metadata about selected stations.
#' @export
create_meta_HYDRO2 = function (computer_data_path, filedir, filename,
                               hydrological_region_level=1,
                               verbose=TRUE) {
    
    # Convert the filename in vector
    filename = c(filename)

    # Print metadata if asked
    if (verbose) {
        print("Get metadata from file :")
        print(paste0(filename, collapse=", "))
    }

    # If the filename is 'all' or regroup more than one filename
    if (all(filename == 'all') | length(filename) > 1 ) {

        # If the filename is 'all'
        if (all(filename == 'all')) {
            # Create a filelist to store all the filename
            filelist = c()
            # Get all the filename in the data directory selected
            filelist_tmp = list.files(file.path(computer_data_path,
                                                filedir))
            
            # For all the filename in the directory selected
            for (f in filelist_tmp) {
                # If the filename extention is 'txt'
                if (tools::file_ext(f) == 'txt') {
                    # Store the filename in the filelist
                    filelist = c(filelist, f) 
                }
            }
            # If the filename regroup more than one filename
        } else if (length(filename > 1)) {
            # The filelist correspond to the filename
            filelist = filename
        }
        
        # Create a blank data frame
        meta = data.frame()
        
        # For all the file in the filelist
        for (f in filelist) {
            # Concatenate by raw data frames created by this function
            # when filename correspond to only one filename
            meta = rbind(meta,
                         create_meta_HYDRO2(computer_data_path, 
                                            filedir, 
                                            f,
                                            verbose=FALSE))
        }
        # Set the rownames by default (to avoid strange numbering)
        rownames(meta) = NULL
        return (meta)
    }

    # Get the filename from the vector
    filename = filename[1]

    # Get the file path to the data
    file_path = file.path(computer_data_path, filedir, filename)

    if (file.exists(file_path) & substr(file_path, nchar(file_path),
                                        nchar(file_path)) != '/') {
        # Create all the header
        metatxt = c(readLines(file_path, n=41, encoding="UTF-8"))

        # print(metatxt[11])
        # print(metatxt[12])
        # print(metatxt[13])
        # print("_________________")
        
        # Create a tibble with all the metadata needed
        # (IN for INRAE data and BH for BH data)
        meta =
            dplyr::tibble(
                       # Station code
                       code=trimws(substr(metatxt[11], 38,
                                          nchar(metatxt[11]))),
                       # Station name
                       name=trimws(substr(metatxt[12], 39,
                                          nchar(metatxt[12]))),
                       # Territory
                       territoire=trimws(substr(metatxt[13], 39,
                                                nchar(metatxt[13]))),
                       # Administrator
                       gestionnaire=trimws(substr(metatxt[7], 60,
                                                  nchar(metatxt[7]))),
                       # Lambert 93 localisation
                       # L93X_m_IN=as.numeric(substr(metatxt[16], 65, 77)),
                       XL93_m=as.numeric(substr(metatxt[16], 38, 50)),
                       # L93Y_m_IN=as.numeric(substr(metatxt[16], 79, 90)),
                       YL93_m=as.numeric(substr(metatxt[16], 52, 63)),

                       # Surface
                       # surface_km2_IN=as.numeric(substr(metatxt[19], 52, 63)),
                       surface_km2=as.numeric(substr(metatxt[19], 38, 50)),

                       # Elevation
                       # altitude_m_IN=as.numeric(substr(metatxt[20], 52, 63)),
                       altitude_m=as.numeric(substr(metatxt[20], 38, 50)),

                       # Start and end of the data
                       debut=substr(metatxt[25], 38, 50),
                       fin=substr(metatxt[25], 52, 63),

                       # Different other info about the flow quality and type
                       statut=iStatut()[trimws(substr(metatxt[26], 38, 50))],
                       finalite=iFinalite()[trimws(substr(metatxt[26], 52, 56))],
                       type=iType()[trimws(substr(metatxt[26], 58, 58))],
                       influence=iInfluence()[trimws(substr(metatxt[26], 60, 60))],
                       debit=iDebit()[trimws(substr(metatxt[26], 62, 62))],
                       QBE=iQBE()[trimws(substr(metatxt[26], 72, 72))],
                       QME=iQME()[trimws(substr(metatxt[26], 74, 74))],
                       QHE=iQHE()[trimws(substr(metatxt[26], 76, 76))],

                       # The path to the data file of BH
                       file_path=file_path)
        
        meta$surface_km2[(meta$surface_km2) <= 0] = NA
        # meta$surface_km2_IN[(meta$surface_km2_BH) <= 0] = NA
        meta$altitude_m[(meta$altitude_m) < 0] = NA
        # meta$altitude_m_IN[(meta$altitude_m_BH) < 0] = NA

        Ltmp = names(iRegHydro())[nchar(names(iRegHydro())) == 2]
        Ltmp = substr(Ltmp, 1, 1)
        infoSecteur = rle(sort(Ltmp))$values
        
        oneL = substr(meta$code, 1, 1)
        twoL = substr(meta$code, 1, 2)
        RH = c()
        for (i in 1:length(oneL)) {
            if (oneL[i] %in% infoSecteur & hydrological_region_level == 2) {
                RHtmp = iRegHydro()[twoL[i]]
            } else {
                RHtmp = iRegHydro()[oneL[i]]
            }
            RH = c(RH, RHtmp)
        }
        
        # Adding of the hydrological region
        meta$hydrological_region = RH
        return (meta)

    } else {
        warning (paste('filepath', file_path, 'do not exist'))
        return (dplyr::tibble())
    }
}
# Example
# meta = create_meta_HYDRO2(
#     "/home/louis/Documents/bouleau/INRAE/CDD_stationnarite/data",
#     "BanqueHydro_Export2021",
#     c('H5920011_HYDRO_QJM.txt', 'K4470010_HYDRO_QJM.txt'))

### 3.2. Creation of data __________________________________________
#' @title Create data
#' @description Creation of data of stations.
#' @param computer_data_path Path to the data.
#' @param filedir Directory of Banque HYDRO data you want to use in
#' ASHE\\computer_data_path\\ to get station codes. If "" is
#' use, data will be search in ASHE\\computer_data_path\\.
#' @param filename String or vector of string of all filenames from
#' which data will be created. If set to 'all', all the file in
#' 'fildir' will be use.
#' @param verbose Boolean to indicate if more processing info are
#' printed (default : TRUE).
#' @return A tibble containing data about selected stations.
#' @export
create_data_HYDRO2 = function (computer_data_path,
                               filedir="",
                               filename="all",
                               variable_to_load=c("Qm3s", "Qls", "Qmmj"),
                               value_to_keep=NULL,
                               format="HYDRO2",
                               verbose=TRUE) {
    
    # Convert the filename in vector
    filename = c(filename)

    if (format == "HYDRO2") {
        skip = 41
    } else if (format == "HYDRO") {
        skip = 55
    }

    # Print metadata if asked
    if (verbose) {
        print("Get observed data from file :")
        print(paste0(filename, collapse=", "))
    }

    # If the filename is 'all' or regroup more than one filename
    if (all(filename == 'all') | length(filename) > 1) {
        # If the filename is 'all'
        if (all(filename == 'all')) {
            # Create a filelist to store all the filename
            filelist = c()
            # Get all the filename in the data directory selected
            filelist_tmp = list.files(file.path(computer_data_path,
                                                filedir))

            # For all the filename in the directory selected
            for (f in filelist_tmp) {
                # If the filename extention is 'txt'
                if (tools::file_ext(f) == 'txt') {
                    # Store the filename in the filelist
                    filelist = c(filelist, f) 
                }
            }
            # If the filename regroup more than one filename
        } else if (length(filename > 1)) {
            # The filelist correspond to the filename
            filelist = filename
        } 

        # Create a blank data frame
        data = data.frame()

        # For all the file in the filelist
        for (f in filelist) {
            # Concatenate by raw data frames created by this function
            # when filename correspond to only one filename
            data = rbind(data,
                         create_data_HYDRO2(computer_data_path=computer_data_path, 
                                            filedir=filedir, 
                                            filename=f,
                                            variable_to_load=variable_to_load,
                                            value_to_keep=value_to_keep,
                                            format=format,
                                            verbose=FALSE))
        }
        # Set the rownames by default (to avoid strange numbering)
        rownames(data) = NULL
        return (data)
    }
    
    # Get the filename from the vector
    filename = filename[1]
    
    # Print metadata if asked
    if (verbose) {
        print(paste("Creation of BH data for file :", filename))
    }

    # Get the file path to the data
    file_path = file.path(computer_data_path, filedir, filename)
    
    if (file.exists(file_path) & substr(file_path, nchar(file_path),
                                        nchar(file_path)) != '/') {
        # Create the data as a data frame
        data = read.table(file_path,
                          header=TRUE,
                          na.strings=c('     -99', ' -99.000'),
                          sep=';',
                          skip=skip)

        # Create all the metadata for the station
        # meta = create_meta_HYDRO(computer_data_path, filedir, filename,
        # verbose=FALSE)
        # Get the code of the station
        # code = meta$code

        metatxt = c(readLines(file_path, n=skip, encoding="UTF-8"))
        if (format == "HYDRO2") {
            code = trimws(substr(metatxt[11], 38, nchar(metatxt[11])))
        } else if (format == "HYDRO") {
            code = trimws(substr(metatxt[20], 38, nchar(metatxt[20])))
        }

        # Create a tibble with the date as Date class and the code
        # of the station
        data = dplyr::tibble(data)
        for (j in 1:ncol(data)) {
            if (is.factor(data[[j]])) {
                data[j] = as.numeric(as.character(data[[j]]))
            }
        }

        if (!is.null(value_to_keep)) {
            data = dplyr::mutate(data,
                                 code=code,
                                 date=as.Date(as.character(data$Date),
                                              format="%Y%m%d"),
                                 Qls=as.numeric(Qls),
                                 Qmmj=as.numeric(Qmmj),
                                 Qm3s=as.numeric(Qls)*1E-3,
                                 !!names(value_to_keep):=
                                     get(names(value_to_keep)),
                                 .keep="used")
            data = dplyr::select(data, dplyr::all_of(c("date",
                                                       "code",
                                                       variable_to_load,
                                                       names(value_to_keep))))
            
            isNA = data[[names(value_to_keep)]] != value_to_keep | is.na(data$Q)
            isNArle = rle(isNA)
            isNArle = isNArle$lengths*isNArle$values
            N = nrow(data)
            data = data[(isNArle[1]+1):(N-isNArle[length(isNArle)]),]
            data$Q[data[[names(value_to_keep)]] != value_to_keep] = NA
            data = dplyr::select(data, -names(value_to_keep))
            
        } else {
            data = dplyr::mutate(data,
                                 code=code,
                                 date=as.Date(as.character(data$Date),
                                              format="%Y%m%d"),
                                 Qls=as.numeric(Qls),
                                 Qmmj=as.numeric(Qmmj),
                                 Qm3s=as.numeric(Qls)*1E-3,
                                 .keep="used")
            data = dplyr::select(data, dplyr::all_of(c("date",
                                                       "code",
                                                       variable_to_load)))
        }

        if (length(variable_to_load) == 1) {
            data = dplyr::rename(data, Q=dplyr::all_of(variable_to_load))
        }
        return (data)

    } else {
        print(paste('filepath', file_path, 'do not exist'))
        return (NULL)
    }
}
# Example
# data = create_data_HYDRO2(
#     "/home/louis/Documents/bouleau/INRAE/CDD_stationnarite/data",
#     '',
#     c('H5920011_HYDRO_QJM.txt', 'K4470010_HYDRO_QJM.txt'))

### 3.3. Creation of climate data and metadata _____________________
#' @title Create climate data
#' @description Creation of climate data and metadata
#' @param computer_data_path Path to the data.
#' @param filedir Directory of data you want to use in
#' ASHE\\computer_data_path\\ to get climate data. If "" is
#' use, data will be search in ASHE\\computer_data_path\\.
#' @param colNames String or vector of string of column names of the
#' tibble that will be created.
#' @param verbose Boolean to indicate if more processing info are
#' printed (default : TRUE).
#' @return A tibble containing data about selected stations.
#' @export
create_climate_data = function (computer_data_path, filedir,
                                colNames=c('Date', 'PRCP_mm',
                                           'PET_mm', 'T_degC'),
                                verbose=TRUE) {

    dirpath = file.path(computer_data_path, filedir)
    filelist = list.files(dirpath)

    print(dirpath)
    print(filelist)
    
    basin = gsub("[^[:alnum:] ].*$", '', filelist)
    
    meta = tibble(code=basin)
    data = tibble()
    nfile = length(filelist)
    
    for (i in 1:nfile) {
        file_path = file.path(dirpath, filelist[i])
        # Create the data as a data frame
        data_basin = read.table(file_path,
                                header=FALSE,
                                sep=' ',
                                skip=1)

        data_basin$code = basin[i]

        data = bind_rows(data, data_basin)
    }
    colnames(data) = c(colNames, 'code')
    data$Date = as.Date(data$Date) 
    res = list(data=data, meta=meta)
    return (res)
}
# Example
# res = create_climate_data(
# "/home/louis/Documents/bouleau/INRAE/CDD_stationnarite/data",
# 'climate')
