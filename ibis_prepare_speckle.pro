;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;find all *.final files (speckle reconstructed)
;save in idl format (.sav)


;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO ibis_prepare_speckle, input_dir, output_dir, f_file,path1, $
                          BIN = bin;, SWAP = swap

time=systime()
newtext = strarr(2) ;new entries for log
newtext[0] = '\textbf{speckle to IDL: ibis\_prepare\_speckle.pro}\\'
newtext[1] = string(time)+'\\'
log_write,path1,newtext

;;***************************************************************
;; set whitelight sizes
;;***************************************************************

     nx = bin[0] & ny = bin[1]  ;orig. size
     n2x = bin[2] & n2y = bin[3] ;new size (smaller for faster speckle)

;;***************************************************************
;; load flat and aligment file
;;***************************************************************

     RESTORE, /VERB, f_file
     mask = ibis_mask(flat,cut=1) ;new own routine
     tmp = where2d(mask eq 1)
     xl = min(tmp[0,*])
     xr = max(tmp[0,*])
     yb = min(tmp[1,*])
     yt = max(tmp[1,*])

;;***************************************************************
;; find speckle reconstructions
;;***************************************************************

     files = FILE_SEARCH(input_dir + '*.final', COUNT = ns)

newtext = strarr(2) ;new entries for log
newtext[0] = string(ns,format='(I4)')+' *.final files converted to *.sav \\'
newtext[1] = ' '
log_write,path1,newtext

     print,'PGJ FILES',files
;;***************************************************************
;; Loop through the reconstructions
;;***************************************************************
  simage = fltarr(nx,ny)

     FOR i = 0, ns -1 DO BEGIN

         tmp = read_rec(files[i], n2x, n2y)

  ;       IF keyword_set(SWAP) then BYTEORDER, tmp, /LSWAP
         tmp =  swap_endian(tmp,/swap_if_big_endian) ;lk
  
         index = WHERE( finite(tmp, /NAN) EQ 1, COMPLEMENT = index_valid)

         IF index[0] NE -1 THEN tmp[index] = AVG( tmp[index_valid] )

         simage[xl:xr,yb:yt] = tmp
         statist, simage

         s = SIZE(simage)
         TVSCL, simage
  
         ;; 
         ;; save aligned reconstruction
         ;;
   ;lk: may 2013: change output name
        tmp = strsplit(files[i],'/',/extr)
        tmp = tmp[n_elements(tmp)-1]   ;remove path from filename
        tmp2 = strmid(tmp,0,strlen(tmp)-6)  ;remove .final

;         filename = output_dir + STRING(i, FORMAT='("speckle_", I3.3, ".sav")')
         filename = output_dir + 'speckle_' + tmp2 + '.sav'

         SAVE, FILENAME = filename, simage

         PRINT,'------------------------------------------------'
         PRINT,' saved: ', filename
         PRINT,'------------------------------------------------'

     ENDFOR

;;***************************************************************
;; Done
;;***************************************************************

END


