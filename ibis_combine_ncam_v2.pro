;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;+
; NAME:
;       ibis_combine_ncam
;
; PURPOSE:
;  saves wl and nb scans in /scans/summary... with images rotated and scaled
;  wl is dark/gain corrected, nb isn't
; CATEGORY:
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       
;
; OPTIONAL OUTPUTS:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; lk oct 11
; lk jan 2014: newwl_v2 (align NB to WL), save nb_data as [x,y,lambda,beam]
;     use info pointer during call
;    mar 2014: fixed that program will not abort if one gain file does
;    not exist. however, if one does not observe all wavelengths during
;    the whole day and does the reduction for a subset of them, the
;    program may still get confused (in this case select wavelength in
;    variables and combine only one single directory).
;lk nov 2014 fixed bug with flatfield application (stksind was
;permutated). Led to I->Q crosstalk. Note: fix will fail if r>1, in
;which case the first N states are used for flats.
; lk apr 2017: add option for spectroscopy

;-
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO ibis_combine_ncam_v2, sdir, output_dir, apath, datadirs, $ 
               movie=movie, overwrite=overwrite, info=info,$
                          singlebeam=singlebeam
 ;             xyfov, POL = pol, ATM = atm, $ 
 ;                 SINGLE = single, DUAL = dual, STOKES = stokes, $
 ;                 ,daydrift=daydrift
 ;                NREPEAT = nrepeat
;sdir : path to nb data
;wdir : path to wl data
;osdir: deleted, now output_dir (same for wl and nb)
;movie: diplay first series

wldfile = *info.wldfile
wlffile = *info.wlffile
wdir = *info.wpath

;------ test if files already reduced ------
tmp = file_search(output_dir+'/*.sav')
if (file_test(tmp[0]) and not keyword_set(overwrite)) then begin
print,'data already reduced for this timestamp for all filters'
print,'found (1st file): ',tmp[0]
return
endif


;;***************************************************************
;; Data
;;***************************************************************

     ;find full path of current directory (datadirs)
     subdirs = find_all_dir(sdir)
     currdir = subdirs[where(strmid(datadirs,14,15,/reverse) eq strmid(subdirs,14,15,/reverse))]
     sfiles = file_search( currdir + '/*.ScienceObservation.fits', COUNT = nnb)
     sz = read_size(sfiles[0])

     subdirs = find_all_dir(wdir)
     currdir = subdirs[where(strmid(datadirs,14,15,/reverse) eq strmid(subdirs,14,15,/reverse))]
  
     wfiles = file_search( currdir + '/*.ScienceObservation.fits', COUNT = nbb)
     sz2 = read_size(wfiles[0])

     if sz[0] ne sz2[0] then message,'problem with file sizes in ibis_combine_ncam'

     PRINT,'---------------------------------------'
     print,'processing:',datadirs
     PRINT,'Science narrowband files found: ', nnb
     PRINT,'Science broadband files found: ', nbb
     PRINT,'---------------------------------------'

;;***************************************************************
;; Load aux files
;;***************************************************************

     RESTORE, /VERB, wldfile
     RESTORE, /VERB, wlffile
     wldark = dark

;;***************************************************************
;; Create mask for gain
;;***************************************************************

     msk = ibis_mask(flat,cut=1) ;lk version
     mask = msk
     index_msk = WHERE(mask EQ 1, COMPLEMENT = index0)

; if keyword_set(limb) then begin
;this part needs to be reconfigured for other limb data
;here it is assumed that disk center is to the right of the limb image
;print,'LIMB data! using custom mask'
 ;find a useful wl file by timestamp
;  tstmp = strmid(datadirs,5,6,/reverse) ;get 175538
;  tstmp2 = strmid(tstmp,0,2)+':'+strmid(tstmp,2,2)+':'+strmid(tstmp,4,2) ;now 17:55:38
;  tsec = time_conv_new(tstmp2) ;convert to seconds to match wl_sid.timestamp
;  tmp = min(abs(wl_sid.time - tsec),ind)
;  tmp = FLOAT(READFITS(wl_sid.filename[ind], hdr, EXT = 3, /SILENT)) ;flat does not have limb!
; window,xs=512,ys=1024
; tvscl,tmp
; mask = ibis_mask(flat,cut=2,leftcut=60)  
; index_msk = WHERE(mask eq 1, COMPLEMENT = index0)
;endif ;use only if ctl function is not taken into accout!! (i.e. generally /limb will not be used!)
;the ctl correction will eliminate gradients and therefore one can use the 'regular' mask
;before that, leftcut=60 removed some of the steep intensity gradient

       gain  = flat / AVG( flat[index_msk] ) ;whitelight...

;;***************************************************************
;; Loop through data
;;***************************************************************

;get filters, stokes etc from s000 (rest of scan should be the same)
 next = readni_ncam(sfiles[0])
 filterobs = fltarr(next)
 wlobs = dblarr(next)

     print,'reading headers for obs. wavelengths...'
     for j=1,next do begin  
     tmp = headfits(sfiles[0],ext=j)
     filterobs[j-1] = sxpar(tmp,'FILTER') ;filterobs[0] is ext=1 of file[0]
     wlobs[j-1] = sxpar(tmp,'WAVELNTH') ;
     endfor

  nfilters = diffelement(filterobs)
  filters =  diffelement(filterobs,/names)
  possiblewl=[5434,5876,5890,5896,6302,6173,6563,6768,7090,7224,8542] ;list of IBIS filters


;------------ for each filter save scan ------------------------
for ff=0,nfilters-1 do begin

    filterind = where(filterobs eq filters[ff])    ;extension numbers-1 of current lambda
    ni = n_elements(filterind) ;number of images with current filter
    nstart = min(where(filterobs eq filters[ff])) + 1 ;+1 because of index 0
    nend = max(where(filterobs eq filters[ff])) + 1 ;## will fail for R>1
     
    ;find current wavelength
     wl = avg(wlobs[where(filters[ff] eq filterobs)])
     a=  min(abs(possiblewl-wl),pos)
     if a ge 2 then message,'filter missing in filterlist?'
     lambda = possiblewl[pos]

     ;restore alignments, nb dark and flat
     afile = apath + string(lambda,format='(I4)') + '_align_params.sav'
     restore,afile,/ve
     dfile=file_search(*info.cpath,strcompress(string(lambda),/remove)+'_dark.sav')
     gfile=file_search(*info.cpath,strcompress(string(lambda),/remove)+'_gain.sav')
  
     if gfile eq '' then print,'No gain available for: ',lambda
     if gfile eq '' then goto,nextfilter  ;if wavelength not selected, gain does not exist -> return
     
 ;now done in ibis_gain.pro...
 ;    ;restore averaged flat profiles to get intensity differences between frames (of left beam)
 ;    afpfile = apath + string(lambda,format='(I4)') + '_flatprof.sav'
 ;    restore,afpfile,/ve
 ;    safp = size(sbpprof_l)
 ;    if safp[0] eq 1 then begin 
 ;       ifac_l = 1.  ;no polarization
 ;    endif else begin
 ;       ifac_l = fltarr(safp[2])
 ;       for i=0,safp[2]-1 do ifac_l[i] = avg(sbpprof_l[*,i]/sbpprof_l[*,0])
 ;    ;this ignored the factor for the right beam, which however should be the inverse of the left
 ;    ;beam. this is not always the case, but should be corrected when both beams are combined
 ;       endelse

     restore,dfile,/ve
     restore,gfile,/ve
     
;*****************************************************************************************
;----------------------------- DUAL BEAM -------------------------------------------------
;*****************************************************************************************

IF singlebeam ne 1 then begin 
  
    ;for each scan
     FOR n = 0, nnb-1 DO BEGIN 

         print,'Narrowband file : ', sfiles[n]
         print,'Broadband file : ', wfiles[n]

         FITS_OPEN, sfiles[n], tmp_nb
         FITS_OPEN, wfiles[n], tmp_bb

         nb_data = FLTARR(sz[0]/2, sz[1], ni,2)
         bb_data = FLTARR(sz[0]/2, sz[1], ni)

         info_bb = ibis_create_struct(ni)
         info_nb = ibis_create_struct(ni)
                                ;structure with telparams, fpi and lcvr params

         ;for each extension
         FOR i = 1L, ni DO BEGIN

            ;------------- NB data ------------------
             FITS_READ, tmp_nb, data, header, exten_no = nstart+(0*ni)+i-1 ;0 was k

             ;Jan 2014: must do dark & flat before alignment
             nbwl = sxpar(header,'wavelnth')
             stks = sxpar(header,'stokes')
             nstks = diffelement(info_flat_short.stokes)  ;6 usually, permutated if using /names!!!
             nstks = info_flat_short.stokes[0:nstks-1] ;to have correct order of states!!!
             ;### this will fail with repetitions
             nwl = diffelement(info_flat_short.wave,/names,/double)  ;~20 usually
             stksind = where(stks eq nstks)  ;which polarization state
             if stksind[0] eq -1 then message,'Bug in ibis_combine_ncam_v2: Did you use I instead of I+Q?'
             ;this could be fixed by looking at voltages of etalons, I is usually I+Q
             ;if r>1 and polarimetry it will also crash at this point

             wlind = where(nbwl eq nwl)
             if wlind[0] eq -1 then message,'Bug in ibis_combine_ncam_v2: lambda index not accurate?'
             data = ((data - dark) * wcgain[*, *, wlind, stksind])
             ;wcgain is sorted by wl, nb_data may not be

             ;align NB to WL (new January 2014)
             data2 = newwl_v2(data,f1,f2,r,sfl,rotindex,off,wl_lcut,/nb)
             nb_data[*,*,i-1,0] = data2[*,*,0] ;narrowband in array
             nb_data[*,*,i-1,1] = data2[*,*,1] ;narrowband in array
             info_nb = ibis_info(header, info_nb, sfiles[n], nstart+(0*ni)+i-1, i-1)
                                ;ibis_info fills structure with sxpar from header

             ;------------- BB data -------------------
             FITS_READ, tmp_bb, data, header, exten_no = nstart+(0*ni)+i-1
             data = ( data - wldark ) / gain ;white light corrected images
                                ;      alignment (align_ibis) is done on 500x500 images, i.e. scale shift if size different
                                ;       factor = round(sz2[0]/500.)
                                ;6.1.14: align done on original size
             data[index0] = avg(data[index_msk])  
    
             data2 = newwl_v2(data,f1,f2,r,sfl,rotindex,off,wl_lcut,/wl)

             bb_data[*,*,i-1] = data2
             info_bb = ibis_info(header, info_bb, sfiles[n], nstart+(0*ni)+i-1, i-1)

          ENDFOR                 ;each extension

 ;---------- movie ----------------
         if (ff eq 0 and n eq 0) then begin 
            if keyword_set(movie) then begin
               mov = fltarr(500+250,500,20)
               mov[0:249,*,*] = (rebin(bb_data[*,*,0:19],500,500,20))[0:249,*,*]
               mov[250:749,*,*] = rebin(nb_data[*,*,0:19],500,500,20)
               anim_lk,mov        
            endif
         endif
 ;---------- end movie ------------


         ;;***************************************************************
         ;; Save data
         ;;***************************************************************

         ;;
         ;; Save narrowband and broadband separately.
         ;; exposure times and other information is included.
         ;;

         filename1 = output_dir + $
                           string(lambda, n, FORMAT='(I4, "_nb", I3.3, ".sav")')
         filename2 = output_dir + $
                           string(lambda, n, FORMAT='(I4, "_bb", I3.3, ".sav")')

         PRINT,'-------------------------------------'
         PRINT,'Scan saved : ', filename1
         PRINT,'Whitelight saved : ', filename2
         PRINT,'-------------------------------------'
                    
         SAVE, FILENAME = filename1, nb_data, info_nb, wl_obs

         ;mwld=bb_data[0:sz[0]/2-1,*,*] ;save 500x1000 images, not 1000x1000
         mwld = bb_data
         SAVE, FILENAME = filename2, mwld, info_bb

    ; ENDFOR

         FITS_CLOSE, tmp_nb
         FITS_CLOSE, tmp_bb

      ENDFOR ;each file


endif else begin   ;end dual beam
;*****************************************************************************************
;----------------------------- SINGLE BEAM -------------------------------------------------
;*****************************************************************************************
  ;for each scan
     FOR n = 0, nnb-1 DO BEGIN 

         print,'Narrowband file : ', sfiles[n]
         print,'Broadband file : ', wfiles[n]

         FITS_OPEN, sfiles[n], tmp_nb
         FITS_OPEN, wfiles[n], tmp_bb

         nb_data = FLTARR(sz[0], sz[1], ni)
         bb_data = FLTARR(sz[0], sz[1], ni)

         info_bb = ibis_create_struct(ni)
         info_nb = ibis_create_struct(ni)
                                ;structure with telparams, fpi and lcvr params

         ;for each extension
         FOR i = 1L, ni DO BEGIN

            ;------------- NB data ------------------
             FITS_READ, tmp_nb, data, header, exten_no = nstart+(0*ni)+i-1 ;0 was k

             ;Jan 2014: must do dark & flat before alignment
             nbwl = sxpar(header,'wavelnth')
             stks = sxpar(header,'stokes')
             nstks = diffelement(info_flat_short.stokes)  ;6 usually, permutated if using /names!!!
             nstks = info_flat_short.stokes[0:nstks-1] ;to have correct order of states!!!
             ;### this will fail with repetitions
             nwl = diffelement(info_flat_short.wave,/names,/double)  ;~20 usually
             stksind = where(stks eq nstks)  ;which polarization state
             if stksind[0] eq -1 then message,'Bug in ibis_combine_ncam_v2: Did you use I instead of I+Q?'
             ;this could be fixed by looking at voltages of etalons, I is usually I+Q
             ;if r>1 and polarimetry it will also crash at this point

             wlind = where(nbwl eq nwl)
             if wlind[0] eq -1 then message,'Bug in ibis_combine_ncam_v2: lambda index not accurate?'
             data = ((data - dark) * wcgain[*, *, wlind, stksind])
             ;wcgain is sorted by wl, nb_data may not be

             ;align NB to WL (new January 2014)
             data2 = newwl_v2(data,f1,f2,r,sfl,rotindex,off,wl_lcut,/nb)
             nb_data[*,*,i-1] = data2 ;narrowband in array
             info_nb = ibis_info(header, info_nb, sfiles[n], nstart+(0*ni)+i-1, i-1)
                                ;ibis_info fills structure with sxpar from header

             ;------------- BB data -------------------
             FITS_READ, tmp_bb, data, header, exten_no = nstart+(0*ni)+i-1
             data = ( data - wldark ) / gain ;white light corrected images
                                ;      alignment (align_ibis) is done on 500x500 images, i.e. scale shift if size different
                                ;       factor = round(sz2[0]/500.)
                                ;6.1.14: align done on original size
             data[index0] = avg(data[index_msk])  
    
             data2 = newwl_v2(data,f1,f2,r,sfl,rotindex,off,wl_lcut,/wl)

             bb_data[*,*,i-1] = data2
             ;note: significant shift between nb_data and bb_data (6 pixels)
             info_bb = ibis_info(header, info_bb, sfiles[n], nstart+(0*ni)+i-1, i-1)

          ENDFOR                 ;each extension


         ;;***************************************************************
         ;; Save data
         ;;***************************************************************

         ;;
         ;; Save narrowband and broadband separately.
         ;; exposure times and other information is included.
         ;;

         filename1 = output_dir + $
                           string(lambda, n, FORMAT='(I4, "_nb", I3.3, ".sav")')
         filename2 = output_dir + $
                           string(lambda, n, FORMAT='(I4, "_bb", I3.3, ".sav")')

         PRINT,'-------------------------------------'
         PRINT,'Scan saved : ', filename1
         PRINT,'Whitelight saved : ', filename2
         PRINT,'-------------------------------------'
                    
         SAVE, FILENAME = filename1, nb_data, info_nb, wl_obs
         mwld = bb_data
         SAVE, FILENAME = filename2, mwld, info_bb

         FITS_CLOSE, tmp_nb
         FITS_CLOSE, tmp_bb

      ENDFOR ;each file





endelse



nextfilter:
  endfor ;filter

END

