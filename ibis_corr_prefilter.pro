PRO ibis_corr_prefilter,cpath,path1,pathtolib=pathtolib,wlobs=wlobs
;January 2012, lk
;added 6173 and 5434, June 2015
;mod 26.10.2016 wlobs:string->integer to fix CASE OF
;
;------- new  ---------
;1) broaden atlas and shift to correct minimum
;4) shift flat to atlas -> wl offset
;2) compare info_flat_short.grid and info_flat_short.wave (rel/abs wl)
;to find offset to absolute wl (wscale)
;3) prefilt_6302_waves -> absolute wl
;5) shift prefilter relative to flat for best match with atlas
;6) save prefilter wl scale (new) and prefilter transmission

;cpath = '/Users/kleintl/ibisdata/calib/'
;cpath = '/hao/lsa2/kleintl/reduc/20111014/calibration/'
;wlobs = 6563
;wlobs = 6302
;wlobs=8542

print,'ibis_corr_prefilter, wavelength: ',wlobs
wlobs = fix(wlobs)  ;string->integer

;-------- line of theoretical line center in air ---------------
centerdb = [[6302,      6563,    8542,     5896,     6173.,   5434],$
            [6302.4936, 6562.81, 8542.091, 5895.924, 6173.33, 5434.52]]

tmp = where(wlobs eq centerdb[*,0])
if tmp eq -1 then message,'specify line center wavelength for your line in ibis_corr_prefilter.pro'
centerwl = centerdb[tmp,1]

;1) ------------atlas -----------------------------------------------

 ;read atlas, normalize
ftsread,data,wlobs-3,wlobs+3,xlam=xlam
if not file_test(cpath+string(wlobs,format='(I4)')+'_flatprof.sav') then begin
   print,'flatprofile not available'
   return
endif

restore,cpath+string(wlobs,format='(I4)')+'_flatprof.sav',/ve
ylam=float(data)/max(data)

 ;broaden
fwhm = [[5896, 6173, 6302, 6563, 7090, 7220, 7500, 8000, 8300, 8542], $
        [23.5, 22.5, 22.0, 22.2, 23.7, 25.0, 28.0, 36.0, 38.5, 43.0]]
currfwhm = interpol(fwhm[*,1],fwhm[*,0],wlobs)
atlas = gaussfold(xlam, ylam, currfwhm/1000.) ;43 mA at 8542


 ;shift
if wlobs eq 6302 then xlam = xlam - 0.007;min was at 6302.50 instead of 6302.4936
if wlobs eq 8542 then xlam = xlam - 0.04
if wlobs eq 5896 then xlam = xlam - 0.16
if wlobs eq 6173 then xlam = xlam - 0.012
if wlobs eq 5434 then xlam = xlam - 0.012

 ;*** xlam,atlas should be correct and a reference for everything else

;4)------------ match flat with atlas ---------

 ;interpolate atlas onto flat wl scale
 atl2 = INTERPOL(atlas,xlam,wscale_equi, /QUADRATIC )

 ;cross correlate spectra (flat to atlas)
flatprof = sbpprof_r[*,0]/max(sbpprof_r[*,0])
 cross_correlate,atl2,flatprof,offset,corr,width=50

help,wlobs
CASE wlobs OF
 ;8542 is too broad for simple crosscorrel
8542: begin
 tmp = min(atl2,pos2)
 cross_correlate,atl2[pos2-50:pos2+50],flatprof[pos2-50:pos2+50]/1.5,offset,corr,width=20 
      end
 ;6563 is too broad for simple crosscorrel
6563: begin
 tmp = min(atl2,pos2)
 cross_correlate,atl2[pos2-50:pos2+50],flatprof[pos2-50:pos2+50]/1.5,offset,corr,width=20 
 print,offset,corr
       end
 ;5896 is more complicated because the prefilter is really steep there
 ; i.e. find minima first, shift, then crosscorrelate
5896: begin
  tmp = min(flatprof,pos1)
  tmp = min(atl2,pos2)
  flatprof2 = shift(flatprof,pos2-pos1)
  cross_correlate,atl2[pos2-25:pos2+25],flatprof2[pos2-25:pos2+25],offset,corr,width=20
  offset = offset+(pos2-pos1)
      end
6173: begin
 tmp = min(atl2,pos2)
 cross_correlate,atl2[pos2-20:pos2+20],flatprof[pos2-20:pos2+20],offset,corr,width=20 
 end
ELSE: ;do nothing
ENDCASE
;test with:
;plot,atl2
;oplot,sshift(flatprof,offset)

 ;offset is given in 'data' units
 wscale_equi_diff = wscale_equi[2] - wscale_equi[1] ;wl step (~10 mA)
 wscale_flat = wscale_equi + offset* wscale_equi_diff

;for 6302 the crosscorrel will find a better fit to the Fe line than to
;the telluric line. But wavelengths should be determined from the
;telluric line.
if wlobs eq 6302 then begin
window,1
 plotflat:
 plot,xlam,atlas,/xs
 oplot,wscale_flat,flatprof,lines=3
 print,'The telluric lines should match, not the Fe lines! Options: shift flat profile (l or r) or accept (a)'
 ans = ''
 telluric:
 read,ans
 case ans of
  'a': break
  'l': begin
       print,'Enter shift in mA:'
       read,mashift
       wscale_flat = wscale_flat - mashift/1000.
       offset = ((wscale_flat - wscale_equi)/wscale_equi_diff)[0]
       goto,plotflat
       end
  'r': begin
       print,'Enter shift in mA:'
       read,mashift
       wscale_flat = wscale_flat + mashift/1000.
       offset = ((wscale_flat - wscale_equi)/wscale_equi_diff)[0]
       goto,plotflat
       end
  else: begin
     print,'choose a valid option!'
     goto,telluric
        end
  endcase
endif

; plot,xlam,atlas
; oplot,wscale_flat,flatprof,lines=3

;*** wscale_flat, flatprof is corrected to atlas wl scale
;*** the correction was -37 mA

;2)--------------- relative/absolute wavelengths --------------------------

relabsoffset = info_flat_short.wave[0] - info_flat_short.grid[0]+offset*wscale_equi_diff  
  ;relative 0.1297 is absolute 6302.1079 before flat shifting
  ;the offset (after flat shifting) is ~6301.94 A


;3)------- prefilter -> absolute (may not be true because of PM/solar beam difference)

        ;temporary
        ;routinepaths = (routine_info(_EXTRA=_extra,/source)).path
        ;index = where(strmatch(routinepaths,'*ibis_gui.pro',/fold_case) eq 1)
        ;pathtolib = routinepaths[index[0]]
        ;posibis = strpos(pathtolib,'ibis_gui')
        ;pathtolib = strmid(pathtolib,0,posibis)

 ;find prefilter file 
 preffile = file_search(pathtolib+'prefilter.profile.'+string(wlobs,format='(I4)')+'*.sav')

 if (size(preffile))[2] ne 7 then begin 
  print,'no prefilter file found'
  return
 endif
 if n_elements(preffile) gt 1 then begin
  print,'Found ', n_elements(preffile), ' prefilter files:'
  for xx=0, n_elements(preffile)-1 do print,preffile[xx]
  PRINT,'Choose one (Enter 0,1,2,...):'
  read,ans
  if ans ge n_elements(preffile) then ans=0 ;for stupid users
  preffile = preffile[ans]
 endif

 ;restore prefilter
 print,'Using: ',preffile
 restore,preffile,/ve

 ;variable names depend on wavelength...
 ;these statements make uniform variable names
 r = execute('prefilt_waves = PREFILT_'+string(wlobs,format='(I4)')+'_WAVES')
 r = execute('prefilt_trans = PREFILT_'+string(wlobs,format='(I4)')+'_TRANS')

 pref_abs = prefilt_waves+relabsoffset ;possibly ~40 mA off

 ;*** pref_abs and prefilt_trans are variables for the prefilter
 
;x) ----------- imaging spectral scan: coarse, but full solar spectrum ----
;full wavelength range and same relative wl scale
;but spectral scan has blueshift -> even relative scale is wrong

;restore
if not file_test(cpath+'imgspecscan_'+string(wlobs,format='(I4)')+'.sav') then begin
  print,'imaging spectral scan not found'
  return
endif

restore,cpath+'/imgspecscan_'+string(wlobs,format='(I4)')+'.sav',/ve

;interpolate atlas to spec scan resolution
 atl3 = INTERPOL(atlas,xlam,wl+relabsoffset, /QUADRATIC )


;crosscorrelate spectral scan to atlas for wl shift
 sscan = prof/max(prof)

CASE wlobs of 
; if wlobs eq 6302 then cross_correlate,atl3[20:40],sscan[20:40],offset,corr,width=10  ;first Fe line
 6302: cross_correlate,atl3[40:55],sscan[40:55],offset,corr,width=10  ;first telluric line
 8542: cross_correlate,atl3[40:60],sscan[40:60],offset,corr,width=10
 6563: cross_correlate,atl3[35:60],sscan[35:60],offset,corr,width=10
 5896: cross_correlate,atl3[25:40],sscan[25:40],offset,corr,width=10
 6173: cross_correlate,atl3[60:80],sscan[60:80],offset,corr,width=10
 5434: cross_correlate,atl3[35:55],sscan[35:55],offset,corr,width=10
 else: cross_correlate,atl3,sscan,offset,corr,width=10
ENDCASE
;plot,atl3
;oplot,sshift(sscan,offset)


 ssoffset = offset * (wl[1]-wl[0]) ;~30 mA

 sswl = wl+relabsoffset+ssoffset
 
 tek_color
 plot,sswl,sscan,/xs,lines=2,/nodata
 oplot,sswl,sscan,lines=2,color=2
 oplot,xlam,atlas
 oplot,wscale_flat,flatprof,color=4
 oplot,pref_abs,prefilt_trans,lines=1
 al_legend,['spec scan','fts atlas','flat'],color=[2,1,4],lines=[2,0,0] 
 loadct,0

;*** sswl, sscan contain wl corrected spectral scan


;5) -------- shift prefilter, interpolation for best match --------------

  ;interpol prefilter and atlas to sswl
  prefilter3 = INTERPOL(prefilt_trans,pref_abs,sswl, /QUADRATIC )
  atl3 = INTERPOL(atlas,xlam,sswl, /QUADRATIC )


 tek_color
 plot,sswl,sscan,/xs,lines=2,/nodata
 oplot,sswl,sscan,lines=2,color=2
 oplot,sswl,atl3*prefilter3
 al_legend,['spec scan','fts atlas*prefilter'],color=[2,1],lines=[2,0] 
 loadct,0
  ;red and white profiles should agree more or less (certainly in 
  ;wavelength, not necessarily in intensity)


 ;optimization: shift prefilter by several mA and divide by factor
 ;to create parameter array
 fshift = findgen(160)/1000. - .08 ;-80 mA, 80mA, steps 1mA
; fshift = findgen(200)/1000. - .1 ;-80 mA, 80mA, steps 1mA
 fmult = findgen(100)/100. + .5 ;0.5-1.5, 0.01
 ptsobs = n_elements(sswl)
 paramarr = fltarr(ptsobs,n_elements(fshift),n_elements(fmult))
 

 for bb=0,n_elements(fmult)-1 do begin
  for aa=0,n_elements(fshift)-1 do begin
     wlpref = pref_abs + fshift[aa]
     ipref =  INTERPOL(prefilt_trans,wlpref,sswl, /QUADRATIC )
     paramarr[*,aa,bb] = ipref/fmult[bb]*atl3
  endfor
 endfor


 ;xvec will be different for each spectral line 
;spectral ranges for crosscorr (exclude telluric lines)
 

;if wlobs eq 6302 then xvec = [10,40,55,67,75,90] ;only Fe lines
;if wlobs eq 6302 then xvec = [10,20,40,55,65,90] ;only telluric
;lines...works for 20110 prefilter
case wlobs of
6302: xvec = [10,32,42,50,65,90] ;only telluric lines...for 2014
8542: xvec = [5,20,35,90]
6563: xvec = [10,75,85,95]
5896: xvec = [10,40,60,90]
6173: xvec = [0,7,17,62,73,95]
5434: xvec = [10,40,52,75,84,89,93,96]
else: xvec = [0,90]
endcase

 plot,paramarr[*,n_elements(fshift)/2,n_elements(fmult)/2]
 xline,xvec,lines=1
 print,'vertical lines denote spectral ranges for best fit search'
 chisq = fltarr(n_elements(fshift),n_elements(fmult))


 ;do least squares minimization in chosen wl ranges
 for bb=0,n_elements(fmult)-1 do begin
  for aa=0,n_elements(fshift)-1 do begin
       tmp =0.
   for cc=0,n_elements(xvec)/2-1 do begin
        tmp = total((paramarr[xvec[cc*2:cc*2+1],aa,bb]-sscan[xvec[cc*2:cc*2+1]])^2.) + tmp
   endfor
   chisq[aa,bb] = tmp
   endfor
 endfor


 ;find minimum chisq
 tmp = min(chisq,pos)    ;find minimum, i.e. best fit
 res = array_indices(chisq,pos)


 prefshift = fshift[res[0]]*1000.
 prefmult = fmult[res[1]]
 ;*** prefshift is the mA shift of the prefilter wrt atlas
 ;*** prefmult is the multiplication factor for the prefilter (not urgent for data)

if res[0] eq 0 then begin
print,'The maximum shift for the prefilter was found. This is unlikely, probably something went wrong with the crosscorrelation.'
print,'edit ibis_corr_prefilter.pro to fix this'
stop
endif 
;if res[0] eq 0 then you probably have to do the alignment manually. I
;did: plot,sscan/shift(prefilter3,-6) and varied the -6 until
;continuum was straight on both sides of the spectrum. The shift -6
;corresponds to print,sswl[0]-sswl[6], which then needs to be manually
;set for prefshift in mA (*100).
;this is a way to overwrite the program if needed:
;if wlobs eq 6302 then prefshift=-210 ;-193
;if wlobs eq 6302 then fshift[res[0]] = prefshift/1000.


  ;problem: observations not taken at disk center, i.e. cannot simply shift 
  ;obs line to match atlas because prefilter stays constant. 
  ;-> need to find conversion from relative scale of observations to absolute scale

  ;wscale_equi[0] = array[77]
  ;info_flat_short.wave[0] = array[138] with same [0] as wscale_equi
  ; wscale_flat[0] -> corresponds to info_flat_short.grid[0]
   reltoabs = wscale_flat[0] - info_flat_short.grid[0]
 
   prefilt_wl = pref_abs + fshift[res[0]]
   prefilt_rel = prefilt_wl - reltoabs

  save,prefshift,prefilt_trans,prefilt_wl,prefilt_rel,reltoabs,prefmult,$
     filename=cpath+'/prefilter_corr'+string(wlobs,format='(I4)')+'.sav'

  ;prefshift: shift in mA
  ;prefilt_trans: prefilter intensity
  ;prefilt_wl: (corrected) prefilter wavelength (absolute)
  ;prefilt_rel: (corrected) prefilter wavelength (relative)
  ;reltoabs: difference between relative and absolute wl scale

;create log entry
log_add_prefilter,path1,wlobs,wl,sscan,prefilt_waves,prefilt_trans,xlam,atlas,sswl,prefilt_wl,$
  wscale_flat,prefmult,flatprof


;----- plotting ------
set_plot,'PS'
device,filename=cpath+'ibis_corr_prefilter'+string(wlobs,format='(I4)')+'.ps',/color
device,xoffset=0,yoffset=0,encapsulated=0
device,xsize=21,ysize=29.5

;page 1: atlas and flat alignment
 plot,xlam,atlas,/xs,position=[0.1,0.6,0.9,0.9],title='fts atlas and initial flat profile',/norm
 oplot,wscale_equi,flatprof,lines=3

 plot,xlam,atlas,position=[0.1,0.2,0.9,0.5],title='fts atlas and shifted flat profile',/noer,/xs,/norm
 oplot,wscale_flat,flatprof,lines=3


;page 2: spectral and prefilter profile
 plot,wl,sscan,title='spectral profile (from imaging spectral scan)',/xs,$
    position=[0.1,0.6,0.9,0.9],psym=-4,/norm
 oplot,prefilt_waves,prefilt_trans,lines=1
 plot,prefilt_waves,prefilt_trans,/xs,title='prefilter profile (from lamp tuning)',$
   position=[0.1,0.2,0.9,0.5],/noer,/norm
 xyouts,0.1,0.1,'Lamp/PM and Sun/Camera have different optical path -> ',/norm
 xyouts,0.1,0.08,'there is a spectral offset in the prefilter',/norm

;page 3: atlas and ss before optimization and after

 tek_color
 plot,sswl,sscan,/xs,lines=2,/nodata,title='before optimization',position=[0.1,0.6,0.9,0.9],$
   yrange=[0,1.1],/ys,/norm
 oplot,sswl,sscan,lines=2,color=2
 oplot,sswl,atl3*prefilter3
 al_legend,['fts atlas*prefilter','spec scan'],color=[0,2],lines=[0,2],pspacing=1.5
 loadct,0
  ;red and white profiles should agree more or less (certainly in 
  ;wavelength, not necessarily in intensity)
 xyouts,0.1,0.55,'red and black profiles should agree more or less (not necessarily in I), ',/norm
 xyouts,0.1,0.53,'but differential effects may be present because prefilter not at correct wl',/norm

 ;interpol atlas to new prefilter wl
  atl4 = INTERPOL(atlas,xlam,prefilt_wl, /QUADRATIC )
 
 ;plot result
 tek_color
 plot,sswl,sscan,thick=2,/xs,title='final result after optimization',position=[0.1,0.2,0.9,0.5],$
   /noer,yrange=[0,1.1],/ys,/nodata,/norm
 oplot,sswl,sscan,color=2,lines=2
 
  oplot,prefilt_wl,atl4*prefilt_trans ;atlas*shifted prefilter
  oplot,prefilt_wl,prefilt_trans,lines=1 ;shifted prefilter
 oplot,wscale_flat, flatprof,color=4 ;flat
 al_legend,['atlas * shifted pref','spec scan','shifted pref','flat profile'],$
   color=[0,2,0,4],lines=[0,2,1,0],pspacing=1.5
 loadct,0
  xyouts,0.1,0.1,'additional shift was:'+string(prefshift)+' mA',/norm

 ;page 4: corrected IBIS profiles

  tek_color
  plot,xlam,atlas,/xs,position=[0.1,0.6,0.9,0.9],title='corrected IBIS profiles',$
    xrange=[min(sswl),max(sswl)],yrange=[0,1.1],/ys

  ;interpolate prefilter for wscale_flat
  prefforflat = INTERPOL(prefilt_trans,prefilt_wl,wscale_flat, /QUADRATIC )
  oplot,wscale_flat,flatprof/prefforflat*fmult[res[1]],color=4

  ;interpolate prefilter for sswl
  prefforss = INTERPOL(prefilt_trans,prefilt_wl,sswl, /QUADRATIC )
  oplot,sswl,sscan/prefforss*fmult[res[1]],color=2
  
  al_legend,['FTS atlas','corrected spectral scan','corrected flat'],color=[0,2,4],$
     lines=[0,0,0],pspacing=1.5

  loadct,0

device,/close
set_plot,'x'
print,'Created: ibis_corr_prefilter'+string(wlobs,format='(I4)')+'.ps'


END
