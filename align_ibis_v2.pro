;lk alignment of broadband and narrow band
;- search for all grid files (better than USAF target)
;- manual image rotation/mirroring
;- use auto_align_images after manual selection and first guess
;- modified oct11, removed target and added flagncam for new camera,
;  filter and wavelength now in header -> use that info
;- apr 28,2012: added path1 for log events
;- 3.1.2014: new principle: align NB wrt to BB to be able to create
;  movies with no shifting
;not perfect, but align is better than 1% in factors and rotation
;- 13.3.2017: added keyword singlebeam

;--- requires:
; ibis_mask.pro
; avg.pro
; diffelement.pro
; setpts_roi.pro (SSW)
; setpts_lk.pro (modified ssw, otherwise contour plots wrong in idl 8.0)
; caltrans.pro
; auto_align_images_lk.pro (modified SSW for contour plots)
; pq2rss.pro (SSW)
; anim.pro (by H. Uitenbroek)

;--- restriction:
; in my case, sxpar crashes when using sswidl
; had to add ssw path in .cshrc as last entry in IDL_PATH and then it worked

; new camera is chosen for observing date 2011 or later. the program
; would need to be adapted for observations from aug 2010-dez 2010
; (include month in checks).

; make sure you have the newest ssw version. also, idl 8.0 seems to have a different
; contour routine making the plots look strangely compressed. My modified ssw routines
; solve this problem.

; it is possible that wl and nb alignment varies during the day. -> take more target 
; images when observing and maybe use rigid alignment in pipeline

;--- problem with wavelengths
; the old ibis files didn't contain a wl in the fits header, only the filter
; number was used. Therefore, using a filter list as defined below
;


;----------------------------------------------------------------
PRO align_ibis_v2,spath,wpath,cpath,wldfile,wlffile,griddir,path1,flagncam=flagncam,singlebeam=singlebeam

;------- test if alignment already done -------
IF FILE_TEST(cpath+'*_align_params.sav') THEN BEGIN
        PRINT,'----------------------------------'
        PRINT, 'At least one alignment already exists'
        PRINT,'----------------------------------'
        print,'continue with alignment (y/n, default=n)?'
        ans = ''
        read, ans
        if ans ne 'y' then return else goto,mainprog

ENDIF ELSE BEGIN
;--------------  main program  ----------------------------------  
mainprog:
print,'starting alignment'

;-- the first grid image of each filter is read by default --
;in case of new cam, this is the first extension. For old cam, it's a
;number in the file-sequence (sXXX). Change imgind for an offset to
;this value (imgind=2 -> 3rd grid file for each filter)
imgind =0


;------- user selects which griddir to use -------
 print,'found the following grid directories. Choose one (preferably dots, def=0):'
 print,griddir
 ans = '' & read,ans
 if ans eq '' then ans=0
 if ans gt n_elements(griddir)-1 then ans=0
 griddir2 = griddir[ans]

;write chosen directory into log
 log_add_align,path1,griddir2

;------------------------------------------------------------------
;---- nb target and grid ----------------------------------
;------------------------------------------------------------------

;cmd='find -L ' + spath + ' -wholename "*'+ griddir2[0] + '*" -name "*.fits" '
;spawn, cmd, nbgrid
nbgrid = file_search(griddir2[0]+'/*.fits')
;nbgrid contains s000 for new cam or s000-sXXX for old cam

;read in files, need to separate by wl!!!
  ni = readni_ncam(nbgrid[0]) ;new cam
  wlarray = dblarr(ni)        ;wavelength in header for new cam
  filterarray = fltarr(ni)
  stokesarray = strarr(ni)

  print,'Searching for number of observed wl'
  for i=0,ni-1 do begin
     tmp=headfits(nbgrid[0],ext=i+1)
     filterarray[i] = sxpar(tmp,'FILTER')
     stokesarray[i] = sxpar(tmp,'STOKES') ;not needed...
     wlarray[i] = sxpar(tmp,'WAVELNTH')
  endfor
diffwl = diffelement(filterarray,/name) ;different filters (i.e. 6,7)
print,'filters used in observations:',diffwl
nst = diffelement(stokesarray)  ;different stokes parameter (I+Q, ...)
nwl = n_elements(diffwl)        ;number of different wl
szx = sxpar(tmp,'NAXIS1')
szy = sxpar(tmp,'NAXIS2')


;-- read nb grid files
nbg = fltarr(szx,szy,nwl)
nbg2 = fltarr(szx,szy,nwl)
print,'reading one file per filter'
for i=0,nwl-1 do nbg[*,*,i] = readfits(nbgrid[0],ext=min(where(filterarray eq diffwl[i]))+1+imgind,hdr)  ;new cam
;the first grid image of each filter is read by default, may be
;changed with imgind

nbg2 = nbg

;------------------------------------------------------------------
;---- whitelight grid ---------------------------------------------
;------------------------------------------------------------------
      subdirs = find_all_dir(wpath)
      tmp = strpos(subdirs,'GridImages') ;grid subdirectories
      tmp2 = strpos(subdirs,'TargetImages')
      wlgrid=file_search(subdirs[where(tmp ne -1)],'*.fits')
      wlgrid = diffelement(wlgrid,/names) ;directories are found twice... this cuts half of them
      if wlgrid[0] eq '' then wlgrid=file_search(subdirs[where(tmp2 ne -1)],'*.fits') ;in case there are no grid
      print,'wl files found:'
      print,wlgrid
;     using same timestamp for wl as for nb above
      wlind = where(strpos(wlgrid,strmid(griddir2,14,15,/rev)) ne -1)
      if wlind[0] eq -1 then message,'something went wrong in align_ibis_v2'
      wlgrid = wlgrid[wlind]


;-grid: read all extensions and make avg -> bad idea, grid moving, take only one ext
if flagncam ne 1 then headwlg = headfits(wlgrid,ext=0) else headwlg = headfits(wlgrid[0],ext=1)
         ;to get number of wl images 
wlg = 0.
nimgg = sxpar(headwlg,'NIMAGES')
if flagncam ne 1 then date = sxpar(headwlg,'DATE') else date=sxpar(headwlg,'DATE-OBS')
year = fix(strmid(date,0,4))
if flagncam eq 1 then begin
   szx = sxpar(headwlg,'NAXIS1')
   szy = sxpar(headwlg,'NAXIS2')
endif else begin                ;old cam didnt have these keywords
   tmp = readfits(wlgrid[0],ext=1)
   szx = (size(tmp))[1]
   szy = (size(tmp))[2]
endelse

wlg = fltarr(szx,szy,nwl)
wlg2 = fltarr(szx,szy,nwl)

;new version: only read one file per filter
if flagncam ne 1 then begin
   for i=0,nwl-1 do begin
      wlg[*,*,i] = readfits_ibis(wlgrid,ext=min(where(filterarray eq diffwl[i]))+1+imgind)
   endfor
endif else begin
   for i=0,nwl-1 do begin
      wlg[*,*,i] = readfits(wlgrid[0], ext=min(where(filterarray eq diffwl[i]))+1+imgind)
   endfor
endelse

;---- grid is moving in both nb and whitelight! no idea why, can't be
;     AO or actual slide -> take same extension for wl and nb


;-- flatfield and dark correct wl target & grid
restore,wlffile,/ve  ;variable 'flat'
restore,wldfile,/ve  ;variable 'dark'

;-- create mask
;otherwise avg ff cannot be set to 1 and division leads to incorrect values
;mask too small with create_aperture!!!
mask = ibis_mask(flat,cut=4)    ;new own routine

gain =  (avg(flat[where(mask eq 1)])) / float(flat) 
gain[where(mask ne 1)] = 1.     ;only average part where sunlight was

for aa =0,nwl-1 do wlg2[*,*,aa] = FLOAT((wlg[*,*,aa] - dark) * gain)
s=size(wlg2)

;------------------------------------------------------------------
;---- nb target and grid ----------------------------------
;------------------------------------------------------------------



;------------ image orientation ------------------
;seems to be variable with each new camera, therefore manual selection
;the rotation index is saved with the alignment parameters and used
;in the reduction pipeline
;the dust on the dot grid should work for alignment

window,/free,xsize=1000,ysize=520
print,'Find orientation of image (if image appears blurry it is because of binning)'
if year le 2007 then tvscl,rebin(wlg,512,512)
if year gt 2007 and year le 2010 then tvscl,rebin(wlg,256,512)
if year ge 2011 then tvscl,rebin(wlg,500,500)
if year le 2010 then tvscl,rebin(nbg2[*,*,0],512,512),512,0
if year ge 2011 then tvscl,rebin(nbg2[*,*,0],500,500),510,0

i=0
print,'press r for rotation, s to save and quit'
  while get_kbrd() ne 's' do begin
    for aa=0,nwl-1 do wlg2[*,*,aa] = rotate(wlg[*,*,aa],i)
    if year le 2007 then tvscl,rebin(wlg2[*,*,0],512,512)
    if year gt 2007 and year le 2010 then tvscl,rebin(wlg2,256,512)
    if year ge 2011 then tvscl,rebin(wlg2,500,500)
    if year le 2010 then tvscl,rebin(nbg2[*,*,0],512,512),512,0
    if year ge 2011 then tvscl,rebin(nbg2[*,*,0],500,500),510,0
    xyouts,0.1,0.1,'rotation index '+string(i),/norm
    rotindex = i
    i=i+1
  endwhile

mask2=rotate(mask,rotindex)
erase
;---------- alignment ------------------------------------------
;--- bin everything to max 512x512
if year le 2010 then begin
if szx ne 512 then nbg2 = rebin(nbg2,512,512,nwl)
;wl is smaller (can be 512x512 for 2007 data)
if s[2] ne 512 then wlg2 = rebin(wlg2,256,512,nwl)
if s[2] ne 512 then mask2 = rebin(mask2,256,512)


endif else begin ;new camera
;test: no rebin
;nbg2 = rebin(nbg2,500,500,nwl)
;wlg2 = rebin(wlg2,500,500,nwl)
;mask2 = rebin(mask2,500,500)

endelse

szx = (size(nbg2))[1]
szy = (size(nbg2))[2]
s =  size(wlg2)

;normalize
;wlg2 = wlg2/max(wlg2)
for i=0,nwl-1 do nbg2[*,*,i] = nbg2[*,*,i] / (max(nbg2[*,*,i]))
for i=0,nwl-1 do wlg2[*,*,i] = wlg2[*,*,i] / (max(wlg2[*,*,i]))



;******************************************************************************
;---------------------- DUAL BEAM --------------------------------------------
;******************************************************************************

if singlebeam ne 1 then begin 
;create cutout from wl that is 250x500
tmp = centroid(mask2)
wl_lcut = round(tmp[0] - szx/4.)

;-------------- new approach with auto_align_images -------------------------
   for i=0,nwl-1 do begin

;--- define outfile name and lambda --------
   erase

;make filterlist for new camera
   tmp = where(diffwl[i] eq filterarray)
   wl = avg(wlarray[tmp])
   possiblewl=[5434,5876,5890,5896,6302,6173,6563,6768,7090,7224,8542] ;list of IBIS filters
   a=  min(abs(possiblewl-wl),pos)
   lambda = possiblewl[pos]
;outfilename
   afile = strcompress(cpath + string(lambda) + '_align_params.sav',/remove_all)

;--- test if outfile already done ---
   if file_test(afile)  then begin
        PRINT,'----------------------------------'
        PRINT, 'file exists: ',afile
        PRINT,'----------------------------------'
        print,'continue with alignment (y/n, default=n)?'
        ans = ''
        read, ans
        if ans ne 'y' then goto,nextwl
     endif


   wlsmall = wlg2[wl_lcut:wl_lcut+szx/2-1,*,i]
   nbleft = nbg2[0:szx/2-1,*,i]
   nbright = nbg2[szx/2:*,*,i]


print,'*** aligning:'+string(lambda)

;setpts_lk,pp,wlg2[*,*,i],nbg2[*,*,i]
;new 2014: switch nb and wl
setpts_lk,pp,nbleft,wlsmall

tt = caltrans(pp)
pin2 = tt[*,0] & qin2 = tt[*,1]

print,'Alignment running...'
;check if sobel(wlg2)*mask is better (edges would disappear)
tfimg = sobel(wlsmall)*mask2[wl_lcut:wl_lcut+szx/2-1,*] ;image to transform, no edges
inew2=auto_align_images_lk(sobel(nbleft),tfimg,pin2,qin2,pout,qout,scale=0.3,/double,/quiet)
;;(transf. images, reference image, initial guess)
;inew2=auto_align_images_lk(nbleft>.3,wlsmall>.3,pin2,qin2,pout,qout,scale=0.5,/double,/quiet) ;(transf. images, reference image, initial guess)
;  rotation =  -0.338552 degrees
;   scale x =  1.01426
;   scale y =  0.992891
;   shift x =  -26.9041
;   shift y =  2.26684

;if accuracy is not good enough, try running auto_align_images_lk twice and change scale to
;0.1 or lower for the second run
;;;inew2=auto_align_images_lk(sobel(wlg2),sobel(nbg2[*,*,i]),pin2,qin2,pout,qout,scale=0.1,/double)

;coordinate transformation
pq2rss,pout,qout,r,f1,f2,exshft,eyshft,enrss,s[1],s[2],/center


;scale and rotate full NB image
img=congrid(nbg2[*,*,i],f1*s[1],f2*s[2],cubic=-0.5) ;scaling
img2 = rot(img,-r,cubic=-0.5) ;rotation
img2 = ibis_set(img2,s[1],s[2])


;leftnb = fltarr(s[1],s[2])
;leftnb[0,0] = nbg2[*,*,i]

;off = xyoff(leftnb,img2[0:szx-1,0:szy-1],szx-4,szy-4) ;don't do this
;here, it may align the right beam

;apply shifts to NB
img3   = interpolate(img2[0:s[1]-1,0:s[2]-1], findgen(s[1])-exshft, findgen(s[2])-eyshft, $
                       /GRID, CUBIC=-0.5)
img3=ibis_set(img3,szx,szy)

;exshft and eyshft are not perfect - extra shift for NB
off = xyoff(wlsmall,img3[0:s[1]/2,*],s[1]/2-4,s[2]-4)
sfl=off+[exshft,eyshft] ;save extra shift
img3   = interpolate(img2[0:s[1]-1,0:s[2]-1], findgen(s[1])-sfl[0], findgen(s[2])-sfl[1], $
                       /GRID, CUBIC=-0.5)

;sample values
;  rotation =  -0.439905 degrees
;   scale x =  1.00991
;   scale y =  1.03082
;   shift x =  -116.451
;   shift y =  10.3361

 
;transformation = rss2pq(szx,szy,/center,xshift=exshft,yshift=eyshft-2,rot12=r,$
;                             xscale=f1,yscale=f2,p=pnew,q=qnew)


;------ animation as test --------------
;dots should not be moving (mask might be moving slightly)
temp=fltarr(szx/2,szy,2)
temp[*,*,0]=wlsmall
;if flagncam ne 1 then begin
;if szx le 511 then xr = szx else xr = 511 
;if szy le 511 then yr = szy else yr = 511 
;endif else begin
;if szx le 500 then xr = szx else xr = 500 
;if szy le 500 then yr = szy else yr = 500 
;endelse
temp[*,*,1]=img3[0:szx/2-1,0:szy-1]
anim_lk,temp

tvscl,temp[*,*,0]-temp[*,*,1]

;find shift of r image to l NB image
leftnb = img2[0:szx/2-1,*]
rightnb = img2[szx/2:*,*]
off = xyoff(leftnb,rightnb,s[1]/2-4,s[2]-4)

print,'left and right nb align'
temp[*,*,0] = leftnb
temp[*,*,1] = ibis_set(interpolate(rightnb,findgen(s[1])-off[0], findgen(s[2])-off[1], /GRID, CUBIC=-0.5),szx/2,szy)
anim_lk,temp

;add info to log
newtext=strarr(2)
newtext[0] = 'wavelength: '+string(lambda)+' \\'
newtext[1] = 'parameters (fx,fy,rot,shift[x,y],rindex): \textit{'+string(f1,f2,r,sfl[0],sfl[1],rotindex,format='(F7.3,",",F7.3,",",F7.3,",",F8.3,",",F8.3,",",I2)')+'} \\'
log_write,path1,newtext

;--- save parameters
print,'saving ',afile
save, f1,f2,r,sfl,rotindex,wl_lcut,off, filename = afile 

nextwl:
endfor

endif else begin ;end dual beam
;******************************************************************************
;---------------------- SINGLE BEAM --------------------------------------------
;******************************************************************************
   for i=0,nwl-1 do begin
wl_lcut=0
;--- define outfile name and lambda --------
   erase

;make filterlist for new camera
   tmp = where(diffwl[i] eq filterarray)
   wl = avg(wlarray[tmp])
   possiblewl=[5434,5876,5890,5896,6302,6173,6563,6768,7090,7224,8542] ;list of IBIS filters
   a=  min(abs(possiblewl-wl),pos)
   lambda = possiblewl[pos]
;outfilename
   afile = strcompress(cpath + string(lambda) + '_align_params.sav',/remove_all)

;--- test if outfile already done ---
   if file_test(afile)  then begin
        PRINT,'----------------------------------'
        PRINT, 'file exists: ',afile
        PRINT,'----------------------------------'
        print,'continue with alignment (y/n, default=n)?'
        ans = ''
        read, ans
        if ans ne 'y' then goto,nextwl2
     endif

    wlsmall = wlg2[wl_lcut:szx-1,*,i]
    nbfull = nbg2[0:szx-1,*,i]

    print,'*** aligning:'+string(lambda)

    setpts_lk,pp,nbfull,wlsmall
    tt = caltrans(pp)
    pin2 = tt[*,0] & qin2 = tt[*,1]

    print,'Alignment running...'
    tfimg = sobel(wlsmall)*mask2[wl_lcut:szx-1,*] ;image to transform, no edges
    inew2=auto_align_images_lk(sobel(nbfull),tfimg,pin2,qin2,pout,qout,scale=0.3,/double,/quiet)

    ;coordinate transformation
    pq2rss,pout,qout,r,f1,f2,exshft,eyshft,enrss,s[1],s[2],/center


    ;scale and rotate full NB image
    img=congrid(nbg2[*,*,i],f1*s[1],f2*s[2],cubic=-0.5) ;scaling
    img2 = rot(img,-r,cubic=-0.5)                       ;rotation
    img2 = ibis_set(img2,s[1],s[2])

;apply shifts to NB
    img3   = interpolate(img2[0:s[1]-1,0:s[2]-1], findgen(s[1])-exshft, findgen(s[2])-eyshft, $
                       /GRID, CUBIC=-0.5)
    img3=ibis_set(img3,szx,szy)
;exshft and eyshft are not perfect - extra shift for NB
    off = xyoff(wlsmall,img3[0:s[1]-1,*],s[1]-1,s[2]-1)
    sfl=off+[exshft,eyshft]     ;save extra shift
    img3   = interpolate(img2[0:s[1]-1,0:s[2]-1], findgen(s[1])-sfl[0], findgen(s[2])-sfl[1], $
                       /GRID, CUBIC=-0.5)

    temp=fltarr(szx,szy,2)
    temp[*,*,0]=wlsmall
    temp[*,*,1]=img3[0:szx-1,0:szy-1]
    anim_lk,temp
    tvscl,temp[*,*,0]-temp[*,*,1]





;add info to log
    newtext=strarr(2)
    newtext[0] = 'wavelength: '+string(lambda)+' \\'
    newtext[1] = 'parameters (fx,fy,rot,shift[x,y],rindex): \textit{'+string(f1,f2,r,sfl[0],sfl[1],rotindex,format='(F7.3,",",F7.3,",",F7.3,",",F8.3,",",F8.3,",",I2)')+'} \\'
    log_write,path1,newtext

    off=0. ;no left/right image offset
;--- save parameters
    print,'saving ',afile
    save, f1,f2,r,sfl,rotindex,wl_lcut,off, filename = afile 

    nextwl2:
 endfor
endelse ;end single beam



print,'all alignments done'
;add space in log
newtext= ' '
log_write,path1,newtext

ENDELSE
END

 
