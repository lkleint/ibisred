;atmospheric dispersion produces a shift of the NB image with respect
;to the broadband image. This shift can be up to ~15 px in the
;morning/evening.
;This program needs to be run after ibis_combine and will shift the
;NB to match the BB images through crosscorrelation.
;the shift in 8542 is the largest
;It OVERWRITES the 'combined' data!!!

;run alignment for file 000, for other files of same wavelength ask
;user if they want to apply same shifts
;set keyword /oneperdir to only align first file of directory and
;apply the same shifts to all other files

;14.4.17, fixed to apply shift for L and R beam

PRO ibis_shiftatmosdisp,output_dir,oneperdir=oneperdir
;output_dir = '/Users/kleintl/sanhome/ibis/reduc/20110924/scans/summary_001/'
loadct,0

;---- find all .sav files after 'combine' ---------
bbfiles = file_search(output_dir+'*bb*.sav', count=nbb)
nbfiles = file_search(output_dir+'*nb*.sav', count=nnb)
if nbb ne nnb then message,'NB and BB number of files does not match'

nwl = diffelement(strmid(nbfiles,13,4,/rev))
filesperwl = nnb/nwl

print,'# files of each wavelength: ',filesperwl

;----- find shift for each file -------
FOR i=0,nbb-1 DO BEGIN

   
   print,'Working on file: ',nbfiles[i]
   restore,nbfiles[i]
   ;nb_data and mwld

   if n_elements(atmshift) ne 0 then begin
      print,'Observation already corrected for atmospheric shift...skipping observation'
      goto,skipobs
   endif

   restore,bbfiles[i]
   sz = size(mwld)
   currwl = strmid(nbfiles[i],13,4,/rev)  ;take wavelength from filename

   ;test if alignment already done for this wavelength
   alignfile = output_dir+currwl+'align.txt'
   if file_test(alignfile) then begin
      openr,1,alignfile
      readf,1,tmp1,tmp2
      ashfts=[tmp1,tmp2]
      close,1

      if ~keyword_set(oneperdir) then begin
         print,'Alignment parameters already exist. Does this look ok?'
         ;test alignment
         window,1,xsize=sz[1],ysize=sz[2]
         tvscl,mwld[*,*,0]
         window,2,xsize=sz[1],ysize=sz[2]
         tvscl,interpolate(nb_data[0:sz[1]-1,*,0],findgen(sz[1])-ashfts[0],findgen(sz[2])-ashfts[1],$
                           /grid,cubic=-.5)
         blink,[1,2]
         print,'press y to apply existing shifts, n to continue with alignment (default)'
         ans = ''
         read,ans
      endif else begin
         ans = 'y'
      endelse

      if ans eq 'y' then goto,applyshifts
   endif


   nimg = 10<sz[3]              ;make sure enough images exist
   shfts = fltarr(2,nimg)       ;test offset in the first 10 wavelength positions

   for jj=0,nimg-1 do begin
      tmp = xyoff(mwld[*,*,jj],nb_data[0:sz[1]-1,*,jj],200,400)
      shfts[*,jj] = tmp
   endfor

   tmp = where(shfts[0,*] ge 20.,complement=okshift) ; a shift of 20 px or larger is not realistic
   if okshift[0] eq -1 then message,'No alignment found'
   shfts = shfts[*,okshift]
;### todo: also test in y-direction

;test which images are blurry by using the sobel filter
   data = mwld[sz[1]/2-100:sz[1]/2+100,sz[2]/2-200:sz[2]/2+200,okshift]
   sdata = fltarr(n_elements(okshift))
   for jj=0,n_elements(okshift)-1 do sdata[jj] = total(sobel(data[*,*,jj]))
   srt = sort(sdata)

;take the sharper images (50% of all)
   ashfts = avg(shfts[*,srt[n_elements(srt)/2:*]],1)

   print,'Shifts found: ',ashfts

;test alignment
   window,1,xsize=sz[1],ysize=sz[2]
   tvscl,mwld[*,*,0]
   window,2,xsize=sz[1],ysize=sz[2]
   tvscl,interpolate(nb_data[0:sz[1]-1,*,0],findgen(sz[1])-ashfts[0],findgen(sz[2])-ashfts[1],/grid,cubic=-.5)
   blink,[1,2]

testshift:
   ans =''
   print,'Alignment OK? (y=default,n)'
   read,ans
   if ans eq 'n' then begin
      print,'Enter your best guess shift in x:'
      read,xshft
      print,'Enter your best guess shift in y:'
      read,yshft
      
      ashfts = [xshft,yshft]
      window,1,xsize=sz[1],ysize=sz[2]
      tvscl,mwld[*,*,0]
      window,2,xsize=sz[1],ysize=sz[2]
      tvscl,interpolate(nb_data[0:sz[1]-1,*,0],findgen(sz[1])-xshft,findgen(sz[2])-yshft,/grid,cubic=-.5)
      blink,[1,2]
      goto,testshift
   endif


applyshifts:
   sz2 = size(nb_data)
   print,'Applying shifts to all NB data...'
   for jj=0,sz[3]-1 do nb_data[*,*,jj,0] = interpolate(nb_data[*,*,jj,0],findgen(sz2[1])-ashfts[0],findgen(sz2[2])-ashfts[1],/grid,cubic=-.5)
   if sz[0] eq 4 then nb_data[*,*,jj,1] = interpolate(nb_data[*,*,jj,1],findgen(sz2[1])-ashfts[0],findgen(sz2[2])-ashfts[1],/grid,cubic=-.5)

      atmshift = ashfts
   ;   stop
      save,nb_data,info_nb,atmshift,filename=nbfiles[i]
      if file_test(alignfile) ne 1 then begin
         openw,1,alignfile
         printf,1,atmshift
         close,1
      endif



skipobs:
      undefine,atmshift         ;delete info variable


ENDFOR



END
