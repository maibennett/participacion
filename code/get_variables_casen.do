/**********************************************************************************************
* Title: Getting CASEN variables
* Created by: M. Bennett
* Created on: 10/26/2020
* Purpose: This dofile cleans CASEN dta, keeps useful variables, create new ones, and exports
			a csv file.
* Last Modified on: 10/26/2020
* Last Modified by: MB
* Edits:
*	[11/07/19]: Created dofile
**********************************************************************************************/



clear all
set more off

if "`c(username)'"=="mc72574"{
	global main_dir "C:\Users\mc72574\Dropbox\covid\participacion"
}

else{
	global main_dir /*Insert your path here (where you have the data)*/
}


cd "${main_dir}"

use "${main_dir}\data_original\Casen 2017.dta", clear

gen cod_comuna = int(comuna)

* Keep only variables we need
keep cod_comuna yautcorh dau hacinamiento expc

* Generate new variables
ta dau, gen(decil)

gen hacinamiento_2=hacinamiento!=1
replace hacinamiento_2 = . if hacinamiento==9

* Collapse data set
collapse (mean) yautcorh decil* hacinamiento_2 [fw=expc], by(cod_comuna)

export delimited using "${main_dir}\repositorio\participacion\data\casen2017_clean.csv", replace
