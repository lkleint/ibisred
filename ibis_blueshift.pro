;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;; load nb flat and dark
;; order flat by ascending wl
;; average all images of same wl and pol state
;; updated flat is saved, old flat is now called .backup
;; user manually selects wl range to use for blueshift calculation (once per wl)
;; ibis_blueshift_map to get actual blueshift
 
;; 11.10.11 removed keyword stokes: can be found from exposures
;; variable, which is restored with the flatfields
;; flagncam is flag for new camera, i.e. some arrays differ (like exposures)
;;
;; requires: diffelement.pro

;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION ibis_blueshift, f_file, d_file, wrange, npoints, aps, path1,$
                         SAVEFILE = savefile, POL = pol, SINGLE = single, DUAL = dual, $
                         flagncam=flagncam  ;    STOKES = stokes

  ;enable logging
   time=systime()
   newtext=strarr(2)
   newtext[0] = '\textbf{blueshift calc: ibis\_blueshift.pro} \\'
   newtext[1] = string(time)+' \\'
   log_write,path1,newtext
 
if file_test(savefile) then begin ;test if blueshift for given wavelength already exists
   print,'Blueshift filea for this wavelength already exists. Overwrite (y,n, def=n)?'
   print,savefile
   ans = ' '
   read,ans
   if ans ne 'y' then return,1
endif

 
;;***************************************************************
;; Load files
;;***************************************************************

     RESTORE, /VERB, f_file
    RESTORE, /VERB, d_file

;;***************************************************************
;; Get dimensions
;;***************************************************************

     dim   = SIZE(nb_flat)
     Nx    = dim[1]
     Ny    = dim[2]
 ;    IF KEYWORD_SET(STOKES) THEN Npol = stokes ELSE Npol = 1
    Npol = diffelement(exposures[*,0].stokes)  ;old cam had dimensions [n images, n scans]
    Nwave = dim[3]/Npol
    if not keyword_set(flagncam) then flagncam=0

;;***************************************************************
;; Get aperture mask
;;***************************************************************

 ;    WINDOW, 0, xs = Nx, ys = Ny
 ;   aps  = ibis_get_aperture(REFORM(nb_flat[*, *, 0]))
 ;new 2014:
     aps = ibis_mask2(reform(nb_flat[*,*,0]))
     idx  = WHERE(aps GE 1, Num)

;     WDELETE

;;***************************************************************
;; Create mean dark, no wavelength dependency assumed
;;***************************************************************

;saved dark is already averaged (april 2013)
  ;    print,'averaging dark...'
  ;    dark = AVG(nb_dark, 2)
  ;    nb_dark = 0 ;memory

;;***************************************************************
;; Order flats when in rabbit mode
;;***************************************************************

if flagncam ne 1 then $ ;old camera
     wtmp = REFORM((exposures.wavelength_offset)[*,0]) else $
     wtmp = reform(info_flat_short.wave) ;new camera


   ;LK: other method, take multiple images into account 
   wlobs=diffelement(wtmp,/name,/double) ;array of observed wl offsets
   nwlobs=diffelement(wtmp)      ;number of observed wl
   nimg = n_elements(wtmp) & series = nimg/(nwlobs*Npol) ;number of total ff images & ff directories
   nimgperser = nimg/series ;images per flatfield series  ## might fail if series do not have equal number of images


     ftmp = nb_flat[*,*,0:nimgperser-1] ;same as fltarr(512,512,nimgperser)
     ;ftmp = nb_flat

     print,'rearranging flat for wl and pol state...'
     nstart=0
     for i=0,nwlobs-1 do begin
     index = where(wtmp eq wlobs[i])  ;all images (IQUV) with a certain wl offset
     ;wlobs is ordered by ascending wl, so this automatically orders flats if in rabbit mode
          for j=0,Npol-1 do begin
          ind2 = index[indgen(series)*Npol+j] ;image of same pol state and same wl offset
     ;added lk because program crashed if only one nb_flat per pol state and wl step
          if (size(nb_flat[*,*,ind2]))[0] eq 2 then $
               ftmp[*,*,nstart]= nb_flat[*,*,ind2] else $
                  ftmp[*,*,nstart]= avg(nb_flat[*,*,ind2],2) ;averaging all images of same wl and pol state
                                ;no averaging for post-2010 data,
                                ;already done during flats
          nstart = nstart+1
          endfor ;end pol states
     endfor ;end wl offsets
  

 
     nb_flat = ftmp & ftmp = 0
     dim   = SIZE(nb_flat) 
     Nwave = dim[3]/Npol ;now smaller


     ;save this flat as current flat
     print,''
     print,'--- updated flat saved ---'
     spawn,'mv '+f_file+' '+f_file+'.backup'
     if flagncam ne 1 then SAVE, filename=f_file,nb_flat,exposures else $
      save,filename=f_file,nb_flat,fflevel,frms,exposures,info_flat_short ;new cam 
     ;the only difference of .backup and curr. flat for the new is that wavelengths are ordered
 ;info_flat_short may still have old wavelength structure

;;---------------------------------------------------------
;;---------------------------------------------------------
;; Distinguish Modes : Spectropolarimetric mode
;;---------------------------------------------------------
;;---------------------------------------------------------

     IF KEYWORD_SET(POL) THEN BEGIN

        print,'------------------------------------------------------'
        print,'Blueshift calculation for spectropolarimetric case ...'
        print,'------------------------------------------------------'

        temp = FLTARR(Nx, Ny)
        flat = FLTARR(Nx, Ny, Nwave, Npol)

        FOR n =0, Nwave-1 DO BEGIN
            FOR i = 0, Npol-1 DO BEGIN
                temp[idx] = (REFORM(nb_flat[*,*,n*Npol+i] - dark))[idx]
                flat[*, *, n, i] = temp
            ENDFOR
        ENDFOR
nb_flat =0 ; memory

;addition LK, 110216: display mean profile and user picks wl range (wrange). 
;Previously it had to be defined in the template without looking at the profile
window,5
plot,flat[nx/4,ny/2,*,0]
print,'choose wl range for blueshift calc by clicking left of line, then right of line'
xy,count=2,pos=position
wrange[0]= round(position[0,0])
wrange[1]= round(position[0,1])
print,'wl range:', wrange

        IF KEYWORD_SET(DUAL) THEN BEGIN

           if flagncam ne 1 then $
            offset = ibis_blueshift_map(flat[*,*,wrange[0]:wrange[1],*], wrange, $ 
                         npoints, aps, exposures, /POL, /DUAL, STOKES=Npol, flagncam=flagncam) else $
            offset = ibis_blueshift_map(flat[*,*,wrange[0]:wrange[1],*], wrange, $ 
                         npoints, aps, info_flat_short, /POL, /DUAL, STOKES=Npol, flagncam=flagncam) 

        ENDIF

        IF KEYWORD_SET(SINGLE) THEN BEGIN

           offset = ibis_blueshift_map(flat[*,*,wrange[0]:wrange[1],*], wrange, $ 
                                       npoints, aps, exposures, /POL, /SINGLE, STOKES=Npol, flagncam=flagncam)

        ENDIF

     ENDIF ELSE BEGIN

;;---------------------------------------------------------
;;---------------------------------------------------------
;; Distinguish Modes : Spectroscopic mode
;;---------------------------------------------------------
;;---------------------------------------------------------

           print,'------------------------------------------------------'
           print,'Blueshift calculation for spectroscopic case ...'
           print,'------------------------------------------------------'

           flat = FLTARR(Nx, Ny, Nwave)
           temp = FLTARR(Nx, Ny)

           FOR n=0, Nwave-1 DO BEGIN
               temp[idx] = (reform(nb_flat[*,*,n] - dark))[idx]
               flat[*, *, n] = temp
           ENDFOR

nb_flat = 0 ;memory
;addition LK, 110216: display mean profile and user picks wl range (wrange). 
;Previously it had to be defined in the template without looking at the profile
plot,flat[nx/4,ny/2,*,0]
print,'choose wl range for blueshift calc by clicking left of line, then right of line'
xy,count=2,pos=position
wrange[0]= round(position[0,0])
wrange[1]= round(position[0,1])



           offset = ibis_blueshift_map(flat[*,*,wrange[0]:wrange[1]], wrange, $ 
                                       npoints, aps, exposures, flagncam=flagncam)

     ENDELSE

;;***************************************************************
;; Save data
;;***************************************************************

     IF KEYWORD_SET(SAVEFILE) THEN SAVE, FILENAME = savefile, offset, aps

        PRINT,'------------------------------------------------------'
        PRINT,'Blueshift calculation done                            '
        PRINT,'File saved :',  savefile
        PRINT,'------------------------------------------------------'

;;***************************************************************
;; Done
;;***************************************************************

     RETURN, offset
END
