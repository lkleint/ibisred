;new data reduction for IBIS
;only works for data taken with new camera, i.e. after 2010.
;mod 13.4.2017: if find_al_dir() finds parent directory, delete it from list

;find directories
;**************************************************************************************
if *info.polpath eq '' then *info.polpath = *info.path ;have polpath defined
;set paths for ibis reduction
*info.spath = *info.path 
*info.spath1 = *info.path1 + 'temporary/'
*info.cpath = *info.path1 + 'calibration/'
*info.speckle_dir = *info.path1 + 'speckle/'


;; data base files
*info.wldbfile  = *info.cpath + 'wl_sid.sav'
*info.nbdbfile  = *info.cpath + 'nb_sid.sav'
*info.tpdbfile  = *info.cpath + 'tel_params.sav'
*info.tppdbfile = *info.cpath + 'tel_params_polcal.sav'
*info.sdbfile   = *info.cpath + 'speckle_sid.sav'
*info.snbdbfile = *info.cpath + 'nb_speckle_files.sav'


;; define dark and flat for whitelight channel
subdirs = find_all_dir(*info.wpath) ;all wl subdirectories
tmp = strpos(subdirs,'DarkCalibration') ;dark subdirectories
wdfile2 = subdirs[where(tmp ne -1)] ;shorten var
*info.wdfile = file_search(wdfile2+'/*.fits')
if (size(*info.wdfile))[0] eq 0 then message,'no wl dark found'
;if (size(wdfile))[1] ne 1 then print,'more than one wl dark found, using first one'
;if (size(wdfile))[1] ne 1 then print,' (if not ok, specify other in ibis_reduce.pro)'
;if (size(wdfile))[1] ne 1 then wdfile = wdfile[0]
 

tmp = strpos(subdirs,'FlatFieldCalibration') ;flat subdirectories
wffile2 = subdirs[where(tmp ne -1)] ;shorten var
*info.wffile = file_search(wffile2+'/*.fits')
if (size(*info.wffile))[0] eq 0 then message,'no wl flat found'
;if (size(wffile))[1] ne 1 then print,'more than one wl flat found, using first one'
;if (size(wffile))[1] ne 1 then print,' (if not ok, specify other in ibis_reduce.pro)'
;if (size(wffile))[1] ne 1 then wffile = wffile[0]  ;modify here if first flat not ok


;afile = cpath + lambda + '_align_params.sav' ;defined later in template
;ffile = cpath + lambda + '_flat.sav'
;dfile = cpath + lambda + '_dark.sav'
;bfile = cpath + lambda + '_blueshift.sav'
;gfile = cpath + lambda + '_gain.sav'
*info.wlffile = *info.cpath + 'flat_' +*info.date+ '.sav'
*info.wldfile = *info.cpath + 'dark_' +*info.date+ '.sav'

;; keywords for identifying the IBIS subdirectory structure.
dark = 'DarkCalibration'
flat = 'Flat'
data = 'Science'
grid = 'Grid'
target = 'Target'
pol = 'Polarization'

;find all subdirectories
test = find_all_dir(*info.spath)
corrdate = where(strpos(test,*info.date) ne -1)

tmp = where(strpos(test[corrdate],dark) ne -1)
if tmp[0] eq -1 then message,'No darks found!'
ind = where(strlen(test[corrdate[tmp]]) eq max(strlen(test[corrdate[tmp]])))
*info.dcdir = test[corrdate[tmp[ind]]]

tmp = where(strpos(test[corrdate],flat) ne -1)
if tmp[0] eq -1 then message,'No flats found!'
ind = where(strlen(test[corrdate[tmp]]) eq max(strlen(test[corrdate[tmp]])))
*info.ffdir = test[corrdate[tmp[ind]]]

tmp = where(strpos(test[corrdate],data) ne -1)
if tmp[0] eq -1 then message,'No science data found!'
ind = where(strlen(test[corrdate[tmp]]) eq max(strlen(test[corrdate[tmp]])))
datadir = test[corrdate[tmp[ind]]]
*info.datadir = datadir[sort(datadir)]

tmp = where(strpos(test[corrdate],grid) ne -1)
if tmp[0] eq -1 then message,'No grid data found!'
ind = where(strlen(test[corrdate[tmp]]) eq max(strlen(test[corrdate[tmp]])))
*info.griddir = test[corrdate[tmp[ind]]]

tmp = where(strpos(test[corrdate],target) ne -1)
if tmp[0] eq -1 then print,'No target data found!'  ;these are not really needed
ind = where(strlen(test[corrdate[tmp]]) eq max(strlen(test[corrdate[tmp]])))
*info.tardir = test[corrdate[tmp[ind]]]


test = find_all_dir(*info.polpath)
tmp = where(strpos(test,pol) ne -1)
if tmp[0] eq -1 then begin
  print,'No polcal data found!'  ;these may not be needed for spectroscopy
  (*info.poldir)[0] = ''
endif else begin
poldir = test[tmp]
;removed keyword date for poldir -> might be taken on different date
;anyway. Just make sure it finds only one day of polcals (by
;specifying polpath in 'variables').
*info.poldir = poldir[sort(poldir)] ;see log failure of several positions...

;first poldir may just be the parent directory -> remove
if strlen((*info.poldir)[0]) lt strlen((*info.poldir)[1]) then *info.poldir = (*info.poldir)[1:*]

if (*info.poldir)[0] eq '' then begin
pol = 'Automatic'
tmp = where(strpos(test,pol) ne -1)
poldir = test[tmp]
*info.poldir = poldir[sort(poldir)] 
endif
endelse

;datadir's first 3 entries are empty spaces - strange
;remove them...
;probably not going to happen again, changed get_ibis_www...
a=where(*info.datadir ne '')
*info.datadir = (*info.datadir)[a]
a=where(*info.poldir ne '')
*info.poldir = (*info.poldir)[a]

;; create subdirectories needed for intermediate steps
;; in the data reduction

if file_test(*info.spath1) ne 1 then spawn,'mkdir '+*info.spath1
if file_test(*info.cpath) ne 1 then spawn,'mkdir '+*info.cpath

;get timestamps of all obs
;### assuming yyyymmdd_hhmmss format!
*info.ts = strarr(n_elements(*info.datadir))
for i=0,n_elements(*info.datadir)-1 do (*info.ts)[i] = strmid((*info.datadir)[i],14,15,/rev)

print,'Creating directories...'
for i=0,n_elements(*info.datadir)-1 do begin
   if file_test(*info.spath1+(*info.ts)[i]) ne 1 then spawn,'mkdir '+*info.spath1+(*info.ts)[i]
   if file_test(*info.path1+(*info.ts)[i]) ne 1 then  spawn,'mkdir '+*info.path1+(*info.ts)[i]
endfor

;; load the sub-directories for later .....

*info.upath = file_search(*info.spath1 + *info.ts)  ;temporary directories
*info.rpath = file_search(*info.path1 + *info.ts)   ;directories for results
;dpath = file_search(spath + datadir)  ;empty?
;**************************************************************************************



;**************************************************************************************
