;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; DATABASE ONLY (had to redo it because of some error in the time format)
;;
;; modified lk, 11 oct 11
;; currently one speckle file per ibis file
;; burst size is given by number of extensions with same filter in
;; file
;;
;; requires: where2d (ssw)
;; ### todo: add repeat option
;; ### todo: add option to save multiple files (of same timestamp)
;; into one speckle file
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO ibis_reduce_speckle_dbonly_ncam, input_dir, output_dir, date, wldfile, wlffile, $ 
                   savefile=savefile, limb=limb

;no more defined bursts but one file per filter-sequence?

;;***************************************************************
;; Data
;;***************************************************************

     ;find all ScienceObs wl data
     subdirs = find_all_dir(input_dir) ;all possible directories
     tmp = subdirs[where(strpos(subdirs,'ScienceObservation') ne -1)]
     wfiles = file_search( tmp + '/*.ScienceObservation.fits', COUNT = nbb)
 
     sz = read_size(wfiles[0])

     PRINT,'---------------------------------------'
     PRINT,'Science broadband files found: ', nbb
     PRINT,'---------------------------------------'

   ;find number of timestamps, assume that file structure is the same for
   ;each file per timestamp -> read only s000
   ;(much faster than reading all files)
   s0files = wfiles[where(strpos(wfiles,'s000.') ne -1)] 
   ntstamps = n_elements(s0files)
   ni = fltarr(ntstamps)
   filests = fltarr(ntstamps)      ;will contain number of files per timestamp
  
   for i=0,ntstamps-1 do begin
     split = strsplit(s0files[i],'/',/extract)
     ts = split[n_elements(split)-2] ;get timestamp from filename
     filests[i] = n_elements(where(strpos(wfiles,ts) ne -1)) ;files per timestamp
     ni[i] = readni_ncam(s0files[i]) 
   endfor


;;***************************************************************
;; Output
;;***************************************************************

     data_out = 'ibis_wl_' + date + '.'
     ofn = output_dir + data_out

;;***************************************************************
;; Load aux files
;;***************************************************************

;     RESTORE, /VERB, wldfile
;     RESTORE, /VERB, wlffile

;;***************************************************************
;; Create mask for gain
;;***************************************************************

;     mask = ibis_mask(flat,cut=1) ;new own routine

;     index_msk = WHERE(mask EQ 1, COMPLEMENT = index0)
;     gain  = flat / AVG( flat[index_msk] )
;flat=0 ;free memory

if keyword_set(limb) then begin
;this part needs to be reconfigured for other limb data
;here it is assumed that disk center is to the right of the limb image
print,'LIMB data! using custom mask'
 tmp = FLOAT(READFITS(wfiles[0], hdr, EXT = 3, /SILENT))
 window,xs=1000,ys=1000
 tvscl,tmp
 mask = ibis_mask(tmp,cut=2,leftcut=10)  
 index = WHERE(mask eq 1, COMPLEMENT = index0)
endif


;;***************************************************************
;; get extension and correct wl
;;***************************************************************

;make list of filters
filterobs = fltarr(max(ni),ntstamps)
wlobs = dblarr(max(ni),ntstamps)
timestamps = strarr(ntstamps); name of timestamp

;get size of files

print,'reading headers for obs. wavelengths...'
for i=0,ntstamps-1 do begin
print,i,' of',ntstamps-1
     fits_open,s0files[i],hdr

     for j=1,ni[i] do begin  
;     tmp = headfits(s0files[i],ext=j)
     fits_read,hdr,data,tmp,exten_no=j,/header_only

     filterobs[j-1,i] = sxpar(tmp,'FILTER') ;filterobs[0,0] is ext=1 of file[0]
     wlobs[j-1,i] = sxpar(tmp,'WAVELNTH') ;filterobs[0,0] is ext=1 of file[0]
     endfor
fits_close,hdr
tmp = strsplit(s0files[i],'/',/extract)
timestamps[i] = tmp[n_elements(tmp)-2] 
endfor

;find number of filters during observation (assuming that file with
;maximum number of extensions contains the maximum number of used filters).
tmp = max(ni,pos)
nfilters = diffelement(filterobs[*,pos])
filters =  diffelement(filterobs[*,pos],/names)


;total number of images

;keep database for compatibility
   speckle_db = CREATE_STRUCT('filename', STRARR(nbb,nfilters), $ 
                                'outfile', STRARR(nbb,nfilters), $ 
                                'starttime', STRARR(nbb,nfilters), $ 
                                'endtime', STRARR(nbb,nfilters), $ 
                                'nimg', INTARR(nbb,nfilters))

 

;;***************************************************************
;; Loop through data
;;***************************************************************
;


;for each filter, do separately
for ff=0,nfilters-1 do begin

     l = 0

  ;find current wavelength
   possiblewl=[5434,5876,5890,5896,6302,6173,6563,6768,7090,7224,8542] ;list of IBIS filters
   wl = avg(wlobs[where(filters[ff] eq filterobs[*,pos]),pos])
   a=  min(abs(possiblewl-wl),pos2)
   lambda = possiblewl[pos2]
print,'current lambda: ',lambda

     FOR n = 0, nbb-1 DO BEGIN  ;all bb files

         print,'Broadband file : ', wfiles[n]
  
       ; compare timestamp to find number of extensions
       tmp = strsplit(wfiles[n],'/',/extract)
       ts = tmp[n_elements(tmp)-2]
       ind = where((ts eq timestamps)) ;index for ni
                
       nstart = min(where(filterobs[*,ind[0]] eq filters[ff])) + 1 ;+1 because of index 0
       nend = max(where(filterobs[*,ind[0]] eq filters[ff])) + 1 ;## will fail for R>1
       ni = nend-nstart+1

     ;if filter is not present in some observations:
      if nstart eq 0 and nend eq 0 then goto,nextfile

       ;  FOR k = 0, nrepeat - 1 DO BEGIN ;keyword, =1 in sample file

             tofn = ofn + string(lambda,format='(I4)') +'.' + STRTRIM(STRING(l,format='(I3.3)'),2)
 
             speckle_db.filename[n,ff] = wfiles[n]
             speckle_db.outfile[n,ff]  = tofn
             speckle_db.nimg[n,ff]  = ni

       ;      OPENW, lun, tofn, /get_lun

           fits_open,wfiles[n],hdr ;new apr 2013

             FOR i = 1L, ni DO BEGIN ;all images in filter

        ;         tmp = (READFITS(wfiles[n], EXT = nstart+(k*ni)+i-1, header, /SILENT) - dark ) / gain
          ;        header = HEADFITS(wfiles[n], EXT = nstart+i-1) 
        
                 fits_read,hdr,tmp,header,exten_no=nstart+i-1,/header_only  ;faster than readfits, apr 2013
       
         ;        tmp[index0] = AVG(tmp[index_msk])

         ;        TVSCL, REBIN(tmp, sz[0]/2, sz[1]/2)

              if i eq 1 then speckle_db.starttime[n,ff]  = sxpar(header,'DATE-OBS')
              if i eq ni then speckle_db.endtime[n,ff]  = sxpar(header,'DATE-OBS')
    
        ;     IF keyword_set(SWAP) then BYTEORDER, tmp, /LSWAP
       ;          tmp = tmp[xl:xr,yb:yt] ;smaller area for speckle -> faster

       ;          tmp =  swap_endian(tmp,/swap_if_big_endian) ;lk
  
        ;do not use the full image -> speckle takes much longer
        ;use mask to write only solar size
        ;         WRITEU, lun, tmp

    
             ENDFOR
             fits_close,hdr ;new apr 2013

             l = l + 1
            
       ;      CLOSE, lun & free_lun, lun

       ;      PRINT,'saved: ', tofn

     ;    ENDFOR
     nextfile:
     ENDFOR

  endfor ;end filters

 ;close,2 ;close speckle_dims.txt

;;***************************************************************
;; Save data base
;;***************************************************************

     IF  KEYWORD_SET(SAVEFILE) THEN SAVE, speckle_db, FILENAME = savefile

;;***************************************************************
;; Done
;;***************************************************************

END

