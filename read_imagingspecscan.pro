pro read_imagingspecscan,datapath,cpath,path1,overwrite=overwrite

;read imaging spectral scan and save profile and rel. wl scale in cpath
;lk feb 2012

;***
;will crash if images not 1000x1000 or larger
;set xl, xr, y1 and y2 below differently in that case
;***

;/home/kleintl/lsa2/14oct11/ibis/ImagingSpectralScan/20111014_134148 -> 6302
;/home/kleintl/lsa2/14oct11/ibis/ImagingSpectralScan/20111014_134213 -> 8542
;/home/kleintl/lsa2/14oct11/ibis/ImagingSpectralScan/20111014_134240 -> 6563

;---- change these paths manually --------
;spath = '/home/kleintl/lsa2/26sep11/'
;cpath = '/hao/lsa2/kleintl/reduc/20110926/calibration/'



;----- find all spectral scans ------------

subdirs = find_all_dir(datapath)           ;all subdirectories
tmp = strpos(subdirs,'ImagingSpectralScan') ;spec scan subdirectories
ssfile2 = subdirs[where(tmp ne -1)] ;shorten var
sslog = file_search(ssfile2+'/log*.txt')
ssfiles = file_search(ssfile2+'/*.fits')
if not file_test(sslog[0]) then message,'no log files found, check datapath'
if n_elements(sslog) ne n_elements(ssfiles) then message,'logs ne files!'

  ;read log for wavelength
  obswl = fltarr(n_elements(sslog))
  expt = fltarr(n_elements(sslog))
  for i=0,n_elements(sslog)-1 do begin
  tmp = read_ibis_log(sslog[i])
  obswl[i] = (tmp.wavelength)[0]
  expt[i] = (tmp.exposure_time)[0]
   endfor 

  ;number of wavelengths
  nwl = diffelement(obswl)
  diffwl = diffelement(obswl,/names,/double)
  print,'Wavelengths for this observing day:',diffwl

  ;timestamps
  ts = strmid(sslog,38,15,/rev)

  
;log
newtext=strarr(2)
time=systime()
newtext[0] = '\textbf{Reducing imaging spectral scans: read\_imagingspecscan.pro} \\'
newtext[1] = string(time)+'\\'
log_write,path1,newtext

  ;go through wavelength, user chooses timestamp
  ;if there is only one spectral scan, don't ask and reduce
  for jj=0,nwl-1 do begin
  print,'Current wavelength:',diffwl[jj]
  tmp = where(obswl eq diffwl[jj])

  ;check if already reduced
  if not keyword_set(overwrite) then begin
   if file_test(cpath+'/imgspecscan_'+string(diffwl[jj],format='(I4)')+'.sav') then begin
    print,'--------------------------------------------------------------------'
    print,'Spectral scans already reduced for this date. Use overwrite to redo.'
    print,'--------------------------------------------------------------------'
    return
   endif
  endif

  for i=0,n_elements(tmp)-1 do $ 
     print,'available timestamps and exposure times: ',ts[tmp[i]],expt[tmp[i]]

  if n_elements(tmp) gt 1 then begin ;more than 1 spec scan
  print,'Choose which one to use [0,1,...]'
      read,ans
      if ans ge n_elements(tmp) then ans=0
  endif else begin ;only 1 spec scan
  ans=0
  endelse

  print,'Using: ',ts[tmp[ans]]

 ;log
 newtext=strarr(1)
 newtext[0] = 'reduced wavelength: '+string(diffwl[jj],format='(I4)')+', using: '+string(ts[tmp[ans]])+' \\'
 ;----- check for underscores (need to be \_ in tex) ----
 for i=0,n_elements(newtext)-1 do begin
 res = strsplit(newtext[i],'_', count = n,/extr)
  if n gt 1 then begin
     newstring = res[0]
     for ll=1,n-1 do newstring = newstring+'\_'+res[ll]
     newtext[i] = newstring
  endif
 endfor
 log_write,path1,newtext

 
  ;find correct spectral scan file for this timestamp
  files = ssfiles[tmp[ans]]
  
;dark file
 d_file = cpath+string(diffwl[jj],format='(I4)')+'_dark.sav'
 if not file_test(d_file) then begin
 print,'Dark not found'
 goto,nextwl ;in this case wl is not reduced
 endif

;blueshift file
 bs_file = cpath+string(diffwl[jj],format='(I4)')+'_blueshift.sav'
 if not file_test(bs_file) then begin
 print,'Blueshift not found' 
 goto,nextwl ;in this case wl is not reduced
 endif

 ;file parameters
 hdr = headfits(files[0],ext=1)
 nimages = sxpar(hdr,'NIMAGES')
 nx = sxpar(hdr,'NAXIS1')
 ny = sxpar(hdr,'NAXIS2')

 ;array for spectral scan
 arr = fltarr(nx,ny,nimages)
 wl = fltarr(nimages)

 print,'reading images'
 for i=1,nimages do begin
  arr[*,*,i-1] = readfits(files[0],ext=i,hdr,/silent)
  wl[i-1] = sxpar(hdr,'REL_WAVE')
 endfor


 ;dark correction
 restore, /verb, d_file
 ;;dark = avg(nb_dark[*,*,1:*],2)
 ;;nb_dark =0
 for i=0,nimages-1 do arr[*,*,i] = arr[*,*,i] - dark

 ;flat not available for I only and so many wavelengths -> averaging anyway


 ;blueshift correction (to avoid broadening of the lines when averaging)
 restore,bs_file
 bsval = offset.cog_fit
 arrsh = arr

 print,'interpolating blueshift'
   FOR j = 0, Ny-1 DO BEGIN
    if j mod 100 eq 0 then print,j,' of',Ny
    FOR i = 0, Nx-1 DO BEGIN
      arrsh[i, j, *] = REFORM(INTERPOL(REFORM(arr[i, j, *]), wl, $
                                   wl + bsval[i, j]))
    ENDFOR
   ENDFOR


;average to get smooth spectral profile
xl = 100 & xr = 400
y1=80 & y2 = 900
prof = fltarr(nimages)
for i=0,nimages-1 do prof[i] = avg(arr[xl:xr,y1:y2,i])
plot,wl,prof,/xs,title=string(diffwl[jj],format='(I4)')

save,wl,prof,filename=cpath+'imgspecscan_'+string(diffwl[jj],format='(I4)')+'.sav',/ve

nextwl:
endfor

newtext=strarr(1)
newtext[0] = ' '
log_write,path1,newtext

end
