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


    
get_Nspace = function (data_code, unit, lim_pct, NspaceMax=NULL) {
    
    # If variable unit is date 
    if (unit == "jour de l'année") {
        # The number of digit is 6 because months are display
        # with 3 characters
        Nspace = 6
        if (!is.null(NspaceMax)) {
            accuracy = NULL
        }
        
    # If it is a flow variable
    } else if (unit == 'hm^{3}' | unit == 'm^{3}.s^{-1}' | unit == 'm^{3/2}.s^{-1/2}' | unit == 'jour' | unit == 'jour.an^{-1}') {
        # Gets the max number of digit on the label
        maxtmp = max(data_code$X, na.rm=TRUE)

        # If the max is greater than 10
        if (get_power(maxtmp) >= 4) {
            Nspace = 12
            if (!is.null(NspaceMax)) {
                accuracy = NULL
            }
            
        } else if (maxtmp >= 10) {
            # The number of digit is the magnitude plus
            # the first number times 2
            Nspace = (get_power(maxtmp) + 1)*2
            # Plus spaces between thousands hence every 8 digits
            Nspace = Nspace + as.integer(Nspace/8)
            if (!is.null(NspaceMax)) {
                # The accuracy is 1
                accuracy = 1
            }

        # If the max is less than 10 and greater than 1
        } else if (maxtmp < 10 & maxtmp >= 1) {
            # The number of digit is the magnitude plus
            # the first number times 2 plus 1 for the dot
            # and 2 for the first decimal
            Nspace = (get_power(maxtmp) + 1)*2 + 3
            if (!is.null(NspaceMax)) {
                # The accuracy is 0.1
                accuracy = 0.1
            }
            
        # If the max is less than 1 (and obviously more than 0)
        } else if (maxtmp < 1) {
            # Fixes the number of significant decimals to 3
            maxtmp = signif(maxtmp, 3)
            # The number of digit is the number of character
            # of the max times 2 minus 1 for the dots that
            # count just 1 space
            Nspace = nchar(as.character(maxtmp))*2 - 3
            if (!is.null(NspaceMax)) {
                # Computes the accuracy
                accuracy = 10^(-nchar(as.character(maxtmp))+3)
            }
        }
        if (unit == 'm^{3/2}.s^{-1/2}') {
            Nspace = Nspace + 1
        }
    }
    
    if (!is.null(NspaceMax)) {
        # Gets the associated number of white space
        prefix = strrep(' ', times=NspaceMax - Nspace)
        res = list(Nspace=Nspace, prefix=prefix, accuracy=accuracy)
        return (res)
        
    } else {
        return (Nspace)
    }
}


#' @title post
#' @export
post = function(x, ...) {
    if (verbose) {
        if (MPI != "") {
            print(paste0(formatC(as.character(rank),
                                 width=3, flag=" "),
                         "/", size-1, " > ", x), ...)
        } else {
            print(x, ...)
        }
    }
}





check_leapYear_hide = function (year) {
    if ((year %% 4) == 0) {
        if ((year %% 100) == 0) {
            if ((year %% 400) == 0) {
                return (TRUE)
            } else {
                return (FALSE)
            }
        } else {
            return (TRUE)
        }
    } else {
        return (FALSE)
    }  
}


check_leapYear = function (year) {
    sapply(year, check_leapYear_hide)
}
