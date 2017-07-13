;LK June 21, 2015
;mod 26.10.2016: fixed bug for crash when only one Stokes state was
;present. sz[4] was not real in that case.

;The prefilter correction should be applied before the demodulation. This program
;applies it to the gain (and overwrites the gain file), such that it is automatically
;applied to the data during flatfielding.

;from other routines:
; wcgain = fltarr(Nx, Ny, Nwave, Npol)  ;dimensions
; data = ((data - dark) * wcgain[*, *, wlind, stksind])  ;flat correction nb

PRO flat_prefilter,cpath,lambda=lambda

print,'Correcting gain for prefilter transmission, lambda=',lambda

;--- find gain file and restore
  gfile=file_search(cpath,strcompress(string(lambda),/remove)+'_gain.sav')
  
  if gfile eq '' then print,'No gain available for: ',lambda
  if gfile eq '' then return ;if wavelength not selected, gain does not exist -> return
  restore,gfile,/ve
 ;wcgain, info_flat_short  

  if n_elements(infostring) ne 0 then begin
     print,'Gain already corrected for prefilter...skipping '+lambda
     return
  endif

;------ find and restore prefilter --------
   if not file_test(cpath+'/prefilter_corr'+string(lambda,format='(I4)')+'.sav') then begin
      print,'No prefilter profile found (file: calibration/prefilter_corr...sav)'
      print,'Omitting wavelength:',lambda
      return
   endif

restore,cpath+'/prefilter_corr'+string(lambda,format='(I4)')+'.sav',/ve
 

;-------- set up prefilter for application on gain -------

;-- relative observed wavelength scale
wlobs = diffelement(info_flat_short.grid,/double,/names)


;-- interpolate prefilter to wlobs
ip_pref = INTERPOL(prefilt_trans,prefilt_rel,wlobs, /QUADRATIC )

infostring = 'Gain is updated with prefilter. Use reltoabs to convert nb_expos.grid to solar wavelengths.'
wl_obs = wlobs+reltoabs


; wcgain = fltarr(Nx, Ny, Nwave, Npol)  ;dimensions
; data = ((data - dark) * wcgain[*, *, wlind, stksind])  ;flat correction nb

;------- correct prefilter -------
sz = size(wcgain)

if sz[0] eq 3 then sz[4] = 1  ;fix error if only one stokes state observed

FOR xx=0,sz[1]-1 do begin
   FOR yy=0,sz[2]-1 do begin
      FOR st=0,sz[4]-1 do begin
         wcgain[xx,yy,*,st] = wcgain[xx,yy,*,st] / ip_pref
      ENDFOR
   ENDFOR
ENDFOR


;OVERWRITE (!) original gain
save,filename=gfile,wcgain,info_flat_short,prefilt_trans,prefilt_rel,reltoabs,infostring,ip_pref,wl_obs
print,'Updated gain overwritten.'
undefine,infostring ;delete info variable
END
