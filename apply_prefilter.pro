
PRO apply_prefilter,cpath,path1, lambda=lambda,dir=dir,allstates=allstates

;OVERWRITES stokesout with prefilter-corrected stokesout !!!
;(reversable, since prefilter is stored)
;lk feb 2012
;requires: undefine.pro (fanning)
;keyword: allstates: by only correct Stokes I (default), use this
;keyword to overwrite
;the polarization states are differences of images, so the prefilter should have subtracted out
if keyword_set(allstates) then allstates=1 else allstates=0

;useful variables that are stored with stokesout now:
;prefilt_trans -> prefilter transmission profile
;prefilt_rel ->   relative (to atlas corrected) wavelength scale
;reltoabs ->      difference between relative and absolute wavelength scale
;                 after crosscorrelation of disk center data with atlas
;info ->          some infos + variable to test if prefilter corr done already
;ip_pref ->       prefilter profile interpolated to the observed wavelengths
;wl_obs ->        observed wavelength scale, includes possible solar Doppler shift

print,'------- Lambda: ',lambda

;------ find and restore prefilter --------
if not file_test(cpath+'/prefilter_corr'+string(lambda,format='(I4)')+'.sav') then begin
print,'No prefilter profile found (file: calibration/prefilter_corr...sav)'
print,'Omitting wavelength:',lambda
return
endif

restore,cpath+'/prefilter_corr'+string(lambda,format='(I4)')+'.sav',/ve
 
;---------- log ----------
        newtext=strarr(2+n_elements(dir))
        tstr = ''
        newtext[0] = 'wavelength: '+string(lambda)+' \\'
        newtext[1] = 'directories: '+string(dir)+'\\'
        for i=0,n_elements(newtext)-1 do begin
            res = strsplit(newtext[i],'_', count = n,/extr)

            if n gt 1 then begin
                newstring = res[0]
                for ll=1,n-1 do newstring = newstring+'\_'+res[ll]
                newtext[i] = newstring
            endif
        endfor
        log_write,path1,newtext


;------ find and restore observation -------

obsfiles = file_search(dir+'/'+string(lambda,format='(I4)')+'*tatb*.sav',count=nfiles)
print,'Number of files: ',nfiles

;if nfiles = -1 (no files for this wl), the for loop will be skipped automatically
for i=0,nfiles-1 do begin
print,'Restoring: ',obsfiles[i]
restore,obsfiles[i]

if n_elements(info) ne 0 then begin
print,'Observation already corrected for prefilter...skipping observation'
goto,skipobs
endif

;relative observed wavelength scale
wlobs = diffelement(nb_expos.grid,/double,/names)

;interpolate prefilter to wlobs
ip_pref = INTERPOL(prefilt_trans,prefilt_rel,wlobs, /QUADRATIC )


;correct I,Q,U and V by the prefilter
;fractional states are always independent of prefilter
sz = size(stokesout)

if allstates eq 1 then begin
print,'correcting I,Q,U,V for prefilter...'
for xx=0,sz[1]-1 do begin
 for yy=0,sz[2]-1 do begin
  for st = 0,sz[4]-1 do begin
    stokesout[xx,yy,0,st] = stokesout[xx,yy,*,st] / ip_pref
  endfor
 endfor
endfor
endif else begin
print,'correcting only I for prefilter...'
for xx=0,sz[1]-1 do begin
 for yy=0,sz[2]-1 do begin
    stokesout[xx,yy,0,0] = stokesout[xx,yy,*,0] / ip_pref
 endfor
endfor


endelse

info = 'stokesout*ip_pref to undo prefilter corr. Use reltoabs to convert nb_expos.grid to solar wavelengths.'
wl_obs = wlobs+reltoabs

;OVERWRITE (!) originally reduced data
save,filename=obsfiles[i],mwld,simage,stokesout,polfac,nb_expos,tp_azim,tp_elev,tp_table,tp_pee,$
  tp_llevel,tp_see,tp_slat,tp_slng,modulation,prefilt_trans,prefilt_rel,reltoabs,info,ip_pref,wl_obs


skipobs:
undefine,info ;delete info variable

endfor

end
