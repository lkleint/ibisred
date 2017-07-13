;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;;only works if savefile is not present -> delete savefile to repeat procedure
;;
;;
;;new camera
;;assign via filenames
;;the whole program is only to assign 000,001,002,... to the filenames
;;in summary_xxx in increasing order (no more time difference
;;searching)
;; the speckle_00x.sav files are in the order of file_search(*.final),
;; continuously numbered and not separated by wl.
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION assign_speckle2nb_ncam, data_dir, lambda, sdb_file, speckledir, $ 
                          SAVEFILE = savefile 


;run program in any case now
  ;;   IF (~(file_test(savefile))) THEN BEGIN

        ;; 
        ;; load speckle database
        IF (~(file_test(sdb_file))) then return,CREATE_STRUCT('filename', ' ',  'burst', 0) ;speckle not done...
        RESTORE, sdb_file, /VERB

        ;;
        ;; find scans
;;data_dir is full path incl. summary_000 : need to delete last two
;;digits for search

;search for correct wavelength
indlambda = where(strpos(speckle_db.outfile[0,*],lambda) ne -1)

filename = speckle_db.filename[*,indlambda]
outfile = speckle_db.outfile[*,indlambda]
starttime = speckle_db.starttime[*,indlambda]

;check if all entries valid
starttime = starttime[where(starttime ne '')]
outfile = outfile[where(starttime ne '')]
filename = filename[where(starttime ne '')]

ts = strmid(filename,43,15,/reve)   ;this is sorted by time
nts = diffelement(ts,/double)

;these files may only be present in selected summary directories,
;depending on 'combine'
datafiles=file_search(data_dir+'/'+ lambda + '_nb*.sav', count = nf)


;this finds all possible directories
directories = find_all_dir(strmid(data_dir,0,strlen(data_dir)-15)+'*')
;test=strpos(directories[0],'summary_')
;summarydir = strmid(directories,test,11) ;all avail dir
;remove the polcal directories from this list (may happen if polcal
;done before 'combine'
pind = where(strpos(directories,'polcal') ne -1, complement=nopind)
directories = directories[nopind]


if diffelement(directories) ne nts then begin
print,'Number of output directories does not match timestamps of speckle files'
print,'Cannot continue matching speckle to observations, returning empty structure'
return, CREATE_STRUCT('filename', ' ',  'burst', 0)
endif
;finds summary_xxx/*nb*.sav

; pgj added next line
if (nf eq 0) then return,  CREATE_STRUCT('filename',' ', 'burst', 0) ;no combined files found

;find original order of how speckle files were saved
;finalfiles = file_search(speckledir+'*.final')

;test=strpos(datafiles[0],'summary_')
;currdir = strmid(datafiles,test,11)  ;all dir that were used

currdir = strmid(datafiles,29,15,/rev)  ;all dir that were used

;find all avail speckle files
specfiles = file_search(speckledir+'/*.sav',count=nspec)
;only look at current wavelength
tmp = where(strpos(specfiles,lambda) ne -1)
if tmp[0] eq -1 then begin
print,'no speckle files for wavelength: ',lambda
print,'Cannot continue. Returning empty structure'
return, CREATE_STRUCT('filename', ' ',  'burst', 0)
endif
specfiles = specfiles[tmp]


;the task is to match datafiles (summary_xxx/wl_nb00x.sav) to
;specfiles, which simply are numbered from 000 to yyy. I.e. One needs
;to know how many observations there are in all summary_xxx, determine
;nb00x, and this is the index of the specfile

;find how many files per timestamp
fileperts = fltarr(nts)
diffts = diffelement(ts,/names)
for aa=0,nts-1 do begin
 tmp = where(diffts[aa] eq ts, nn)
 fileperts[aa]=nn  ;number of sequences taken per timestamp
endfor
        ;; create output structure
        nb_speckle_db = CREATE_STRUCT('filename', STRARR(nf),'burst', STRARR(nf))
 
        ni = speckle_db.nimg[0,indlambda]   ;images per burst

        ;; 
        ;; loop through datafiles and associate information
   

        FOR i = 0, nf-1 DO BEGIN ;number of files

                nb_speckle_db.filename[i] = datafiles[i]

                tmp = where(strpos(directories,currdir[i]) ne -1)
                if tmp eq -1 then message,'something went wrong in assign_speckle2nb_ncam'
                ind1 =tmp  ;index of timestamp

                ind2 = float(strmid(datafiles[i],6,3,/rev))  ;number of series for given timestamp

                if ind1 eq 0 then fileno = ind2 else $
                fileno = total(fileperts[0:ind1-1])+ind2
                ext = specfiles[fileno]

;                searchstring = lambda+'.'+string(i,format='(I03)')
;                ext = where(stregex(finalfiles,searchstring) ne -1 )

                nb_speckle_db.burst[i] = ext ;now filename!!!
                print,i, datafiles[i], ext
   
  
        ENDFOR
        ;;
        ;; save file
        ;;

        IF KEYWORD_SET(SAVEFILE) THEN SAVE,nb_speckle_db, FILENAME = savefile,/ve

    ;    ENDIF ELSE BEGIN;
;
;        PRINT,'--------------------------------------------------------'
;        PRINT,savefile + ' already exists!'
;        PRINT,'--------------------------------------------------------';
;
;        RESTORE, savefile, /VERB
;
;     ENDELSE
    
;;----------------------------------
;; Done
;;----------------------------------

     RETURN,nb_speckle_db

END
