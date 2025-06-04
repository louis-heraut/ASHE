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


## 1. WRITING ________________________________________________________
#' @title write_tibble
#' @description Writes a [dplyr::tibble()] or a list of [dplyr::tibble()].
#' @param tbl [dplyr::tibble()] to write.
#' @param path *tibble, default="data.csv"* Path were the file will be writted.
#' @details This set of [read_tibble()] and [write_tibble()] functions support `.Rdata`, `.txt` and `.fst` format.
#' @examples
#' # load data
#' data("iris")
#' # convert data
#' iris = dplyr::tibble(iris)
#' iris$Species = as.character(iris$Species)
#' 
#' # writes a tibble
#' write_tibble(iris, path="iris.csv")
#' # or a list of tibble
#' iris_setosa = iris[iris$Species == "setosa",]
#' iris_virginica = iris[iris$Species == "virginica",]
#' iris_selection = list(setosa=iris_setosa, virginica=iris_virginica)
#' write_tibble(iris_selection, path="iris_selection.csv")
#' @md
#' @export
write_tibble = function (tbl, path="data.csv", quote=TRUE, sep=",") {

    filename = basename(path)
    filedir = dirname(path)
    
    if (!(file.exists(filedir))) {
        dir.create(filedir, recursive=TRUE)
    }

    name = gsub("[.].*$", "", filename)
    format = gsub("^.*[.]", "", filename)
    filepath = file.path(filedir, filename)

    if (any(class(tbl) == "list")) {
        N = length(tbl)
        for (i in 1:N) {
            write_tibble(tbl[[i]],
                         path=file.path(filedir, name,
                                        paste0(names(tbl)[i],
                                               ".", format)))
        }

    } else {
        if (any(sapply(tbl, is.list))) {
            tbl = flatten_tibble(tbl)
        }
        
        if (format == "fst") {
            fst::write_fst(tbl, filepath, compress=100)

        } else if (format == "Rdata") {
            save(tbl, file=filepath)
            
        } else if (format %in% c("csv", "txt")) {
            # write.table(tbl,
            #             file=filepath,
            #             sep=sep,
            #             quote=quote,
            #             row.names=FALSE)
            write.csv(tbl, file=filepath,
                      row.names=FALSE)
        }
    }
}

# tbl = dplyr::tibble(A=letters[1:3], B=1:3)
# write_tibble(tbl, "data.csv")
# tbl_list = list(A=dplyr::tibble(A=letters[1:3]),
                # B=dplyr::tibble(B=1:3))
# write_tibble(tbl_list, "data_list.csv")


flatten_tibble = function (tbl, delimiter = "; ") {
    tbl = dplyr::mutate(tbl,
                        dplyr::across(dplyr::everything(), ~ {
                            if (is.list(.x)) {
                                sapply(.x, function(y) {
                                    paste(unlist(y), collapse=delimiter)
                                })
                            } else {
                                .x
                            }
                        }))
    return (tbl)
}



### 1.1. List of dataframe ___________________________________________
#' @title Write list of dataframe
#' @export
write_analyse = function (Ldf, resdir, filedir) {
    
    outdir = file.path(resdir, filedir)
    if (!(file.exists(outdir))) {
        dir.create(outdir, recursive=TRUE)
    }

    print(paste('Writing of list of dataframe in : ', outdir, sep=''))
    
    Lname = names(Ldf)
    Nname = length(Lname)
    for (i in 1:Nname) {
        outfile = paste(Lname[i], '.txt', sep='')
        
        write.table(Ldf[[i]],
                    file=file.path(outdir, outfile),
                    sep=";",
                    quote=TRUE,
                    row.names=FALSE)
    }
}

### 1.2. Dataframe of modified data __________________________________
#' @title Write dataframe
#' @export
write_data = function (data, df_mod, resdir, filedir) {

    Code = rle(sort(df_mod$code))$values
    
    outdir = file.path(resdir, filedir)
    if (!(file.exists(outdir))) {
        dir.create(outdir, recursive=TRUE)
    }

    print(paste('Writing of modified data in : ', outdir, sep=''))

    for (code in Code) {
        data_code = data[data$code == code,]
        df_mod_code = df_mod[df_mod$code == code,]

        outfile1 = paste(code, '.txt', sep='')
        write.table(data_code,
                    file=file.path(outdir, outfile1),
                    sep=";",
                    quote=TRUE,
                    row.names=FALSE)

        outfile2 = paste(code, '_modification', '.txt', sep='')
        write.table(df_mod_code,
                    file=file.path(outdir, outfile2),
                    sep=";",
                    quote=TRUE,
                    row.names=FALSE)
    }
}

### 1.3. Dataframe of criticism ______________________________________
#' @title Write criticism
#' @export
write_critique = function (df_critique, resdir, filename='critique') {

    outdir = file.path(resdir)
    if (!(file.exists(outdir))) {
        dir.create(outdir, recursive=TRUE)
    }

    print(paste('Writing criticism in : ', outdir, sep=''))

    outfile = paste(filename, '.txt', sep='')
    write.table(df_critique,
                file=file.path(outdir, outfile),
                sep=";",
                quote=FALSE,
                row.names=FALSE)   
}
# write_critique(df_critique, resdir)


### 1.4. Fast for R __________________________________________________
#' @title Write data dataframe fast
#' @export
write_dataFST = function (data, resdir, filedir='fst',
                          filename='data.fst') {
    
    outdir = file.path(resdir, filedir)
    if (!(file.exists(outdir))) {
        dir.create(outdir, recursive=TRUE)
    }
    outfile = file.path(outdir, filename)
    fst::write_fst(data, outfile, compress=100)
}


## 2. READING ________________________________________________________
#' @title read_tibble
#' @description Reads a file previously writed with [write_tibble()] and return a [dplyr::tibble()] or a list of [dplyr::tibble()].
#' @param path *character, default="data.csv"*   Path to the file to read.
#' @details This set of [read_tibble()] and [write_tibble()] functions support `.Rdata`, `.txt` and `.fst` format.
#' @examples
#' # load data
#' data("iris")
#' # convert data
#' iris = dplyr::tibble(iris)
#' iris$Species = as.character(iris$Species)
#' 
#' # writes a tibble
#' write_tibble(iris, path="iris.csv")
#' # or a list of tibble
#' iris_setosa = iris[iris$Species == "setosa",]
#' iris_virginica = iris[iris$Species == "virginica",]
#' iris_selection = list(setosa=iris_setosa, virginica=iris_virginica)
#' write_tibble(iris_selection, path="iris_selection.csv")
#' 
#' # read with a path
#' read_tibble(path="iris.csv")
#' @md
#' @export
read_tibble = function (path="data.csv", ...) {
    
    path_name = gsub("[.].*$", "", basename(path))
    path_format = gsub("^.*[.]", "", basename(path))
    path_dir = file.path(dirname(path),
                         path_name)

    if (dir.exists(path_dir)) {
        Paths = list.files(path_dir,
                           full.names=TRUE)
        Tbl = list()
        for (f in Paths) {
            tbl = read_tibble(path=f)
            Tbl = append(Tbl, list(tbl))
            names(Tbl)[length(Tbl)] = gsub("[.].*$", "",
                                           basename(f))
        }
        return (Tbl)
        
    } else {
        format = gsub("^.*[.]", "", basename(path))
        
        if (format == "fst") {
            tbl = dplyr::tibble(fst::read_fst(path))

        } else if (format == "rds") {
            tbl = readRDS(path)

        } else if (format == "Rdata") {
            tmp = load(path)
            tbl = get(tmp)
            tbl = as_tibble(tbl)
            rm (tmp)

        } else if (format %in% c("csv", "txt")) {
            # tbl = dplyr::as_tibble(read.table(file=path,
            #                                   header=TRUE,
            #                                   sep=sep,
            #                                   quote='"',
            #                                   ...))
            tbl = dplyr::as_tibble(read.csv(file=path, ...))
            
            for (j in 1:ncol(tbl)) {
                if (is.factor(tbl[[j]])) {
                    d = try(as.Date(tbl[[1, j]],
                                    tryFormats=c("%Y-%m-%d",
                                                 "%Y/%m/%d",
                                                 "%Y%m%d")),
                            silent=TRUE)
                    test = nchar(as.character(tbl[[1, j]])) > 10
                    if("try-error" %in% class(d) || is.na(d) | test) {
                        tbl[j] = as.character(tbl[[j]])
                    } else {
                        tbl[j] = as.Date(tbl[[j]])
                    }
                }
            }
        }
        return (tbl)
    }
}

# read_tibble("data.csv")
# read_tibble("data_list.csv")


### 2.1. List of dataframe ___________________________________________
#' @title Read list of dataframe
#' @export
read_analyse = function (resdir, filedir) {

    outdir = file.path(resdir, filedir)
    files = list.files(outdir)
    Nfile = length(files)

    print(paste('Reading of list of dataframe in : ', outdir, sep=''))

    Ldf = list()
    for (i in 1:Nfile) {
        name = splitext(files[i])$name
        
        df =  as_tibble(read.table(file=file.path(outdir, files[i]),
                                   header=TRUE,
                                   sep=";",
                                   quote='"'))

        for (j in 1:ncol(df)) {
            if (is.factor(df[[j]])) {
                d = try(as.Date(df[[1, j]], format="%Y-%m-%d"))
                test = nchar(as.character(df[[1, j]])) > 10
                if("try-error" %in% class(d) || is.na(d) | test) {
                    df[j] = as.character(df[[j]])
                } else {
                    df[j] = as.Date(df[[j]])
                }
            }
        }
        
        Ldf = append(Ldf, list(df))
        names(Ldf)[length(Ldf)] = name
    }
    return (Ldf)
}

### 2.2. Dataframe of modified data __________________________________
#' @title Read dataframe
#' @export
read_data = function (resdir, filedir, filename, verbose=TRUE) {

    # Convert the filename in vector
    filename = c(filename)

    # If the filename is 'all' or regroup more than one filename
    if (all(filename == 'all') | length(filename) > 1) {
        # If the filename is 'all'
        if (all(filename == 'all')) {
            # Create a filelist to store all the filename
            filelist = c()
            # Get all the filename in the data directory selected
            filelist_tmp = list.files(file.path(resdir,
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
                         read_data(resdir, 
                                   filedir, 
                                   f))
        }
        # Set the rownames by default (to avoid strange numbering)
        rownames(data) = NULL
        return (data)
    }
    
    # Get the filename from the vector
    filename = filename[1]

    # Print metadata if asked
    if (verbose) {
        print(paste("reading of data for file :", filename))
    }

    # Get the file path to the data
    filepath = file.path(resdir, filedir, filename)
    
    if (file.exists(filepath) & substr(filepath, nchar(filepath),
                                       nchar(filepath)) != '/') {
        # Extract the data as a data frame
        data = as_tibble(read.table(filepath,
                                    header=TRUE,
                                    na.strings=c('NA'),
                                    sep=';',
                                    quote='"',
                                    skip=0))

        for (j in 1:ncol(data)) {
            if (is.factor(data[[j]])) {
                d = try(as.Date(data[[1, j]], format="%Y-%m-%d"))
                if("try-error" %in% class(d) || is.na(d)) {
                    data[j] = as.character(data[[j]])
                } else {
                    data[j] = as.Date(data[[j]])
                }
            }
        }
        
        return (data)

    } else {
        print(paste('filename', filepath, 'do not exist'))
        return (NULL)
    }
}

### 2.3. Dataframe of criticism ______________________________________
#' @title Read criticism
#' @export
read_critique = function (resdir, filename='critique') {

    outdir = file.path(resdir)
    outfile = paste(filename, '.txt', sep='')
    filepath = file.path(outdir, outfile)
    
    print(paste('Reading criticism in : ', filepath, sep=''))
    
    df =  as_tibble(read.table(file=filepath,
                               header=TRUE,
                               sep=";"))
    
    for (j in 1:ncol(df)) {
        if (is.factor(df[[j]])) {
            d = try(as.Date(df[[1, j]], format="%Y-%m-%d"))
            if("try-error" %in% class(d) || is.na(d)) {
                df[j] = as.character(df[[j]])
            } else {
                df[j] = as.Date(df[[j]])
            }
        }
    }

    return (df)
}
# df_critique = read_critique(resdir)

### 2.4. Fast for R __________________________________________________
#' @title Read dataframe fast
#' @export
read_tibbleFST = function (filedir, filename) {
    filepath = file.path(filedir, filename)
    df = tibble(fst::read_fst(filepath))
    return (df)
}

read_shp = function (path) {
    shp = st_read(path)
    shp = st_transform(shp, 2154) 
    return (shp)
}

#' @title clean_path
#' @export
clean_path = function (text) {
    text = iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT")
    text = gsub("[()]", "", text)
    return (text)
}
