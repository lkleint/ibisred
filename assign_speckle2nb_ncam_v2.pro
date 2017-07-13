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
;; 140307: technically, this program is useless and could be
;; incorporated in 'reduce'. I changed the speckle assignment here so that
;; the database contains all possible combined files and 'reduce' will
;; choose the correct speckle file.

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION assign_speckle2nb_ncam_v2, data_dir, lambda, sdb_file, speckledir, $ 
                          SAVEFILE = savefile 


;run program in any case now
  ;;   IF (~(file_test(savefile))) THEN BEGIN

        ;; 
        ;; load speckle database
        IF (~(file_test(sdb_file))) then return,CREATE_STRUCT('filename', ' ',  'burst', 0) ;speckle not done...
        RESTORE, sdb_file, /VERB

        ;;
        ;; find scans
;;data_dir is full path of all timestamps in temporary
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


;new 140307: find number of files per timestamp
tsnames = diffelement(ts,/names)
nfilesperts = fltarr(nts)

FOR i=0,nts-1 do begin
tmp = where(strpos(ts,tsnames[i]) ne -1)  ;cannot be -1
nfilesperts[i] = n_elements(tmp)
ENDFOR


nf = n_elements(filename)
; pgj added next line
if (nf eq 0) then return,  CREATE_STRUCT('filename',' ', 'burst', 0) ;no combined files found

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


nb_speckle_db = CREATE_STRUCT('filename', STRARR(nf),'burst', STRARR(nf))
ni = speckle_db.nimg[0,indlambda] ;images per burst, hoping that the wavelength file did not change during the day...


ctr=0
           FOR jj=0,nts-1 DO BEGIN
              FOR kk=0,nfilesperts[jj]-1 do begin

                 fname = data_dir[jj]+'/'+lambda+'_nb'+string(kk,format='(I03)')+'.sav'
                 print,fname
                 
                 nb_speckle_db.filename[ctr] = fname
                 nb_speckle_db.burst[ctr] = specfiles[ctr]
                 
                 ctr = ctr+1
              ENDFOR
           ENDFOR

;the task is to match datafiles (summary_xxx/wl_nb00x.sav) to
;specfiles, which simply are numbered from 000 to yyy. I.e. One needs
;to know how many observations there are in all summary_xxx, determine
;nb00x, and this is the index of the specfile



          
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
