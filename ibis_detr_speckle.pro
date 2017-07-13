;detrend speckle images (destretch) and save them again
;finds all *.sav images, orders them by time, 
; finds all images of same target, destretch, overwrites *.sav

;run after ibis_prepare_speckle
;if something goes wrong, ibis_prepare_speckle can always get the old
;*.sav files back (from the binary files)

pro ibis_detr_speckle,speckle_dir,sdbfile,path1,flagncam=flagncam,movie=movie

time=systime()
newtext = strarr(3) ;new entries for log
newtext[0] = '\textbf{destretch speckle: ibis\_detr\_speckle.pro}\\'
newtext[1] = string(time)+'\\'
newtext[2] = ' '
log_write,path1,newtext

;---- find all speckle files ------------------------------------------------
sfiles = file_search(speckle_dir+'*.sav',count = nfiles)
if nfiles eq 0 then begin
print,'No speckle files found'
return
endif

if flagncam ne 1 then begin
  print,'this program is not necessary for old cam'
  return
endif


;----- array for speckle files and resize them ----
specklearr = fltarr(1000,1000,nfiles)
indarr = intarr(nfiles)

;----- calibration file to assign wl and path -----
restore,sdbfile,/ve
;file created by ibis_reduce_speckle

nframes = (size(speckle_db.outfile))[1]
nwl = (size(speckle_db.outfile))[2]

;------ make one-dimensional arrays ---------------
outfile = strarr(nframes*nwl)
starttime = strarr(nframes*nwl)
filename = strarr(nframes*nwl)
ctr=0 ;counter

for wl=0,nwl-1 do begin
  for fr=0,nframes-1 do begin
  outfile[ctr] = speckle_db.outfile[fr,wl]
  filename[ctr] = speckle_db.filename[fr,wl]
  starttime[ctr] = speckle_db.starttime[fr,wl]
  ctr = ctr+1
  endfor
endfor

;--- some wl may not have been observed at all times ---
starttime = starttime[where(starttime ne '')]
outfile = outfile[where(starttime ne '')]
filename = filename[where(starttime ne '')]


;sind denotes how .sav files are sorted
;i.e. speckle_000.sav is outfile[sind[0]]
;;;sind = sort(outfile)

;;;starttime = starttime[sind]
;;;outfile = outfile[sind]
;;;filename = filename[sind]
;now all three variables are organized according to the sav files

;get timestamps
ts = strmid(filename,43,15,/reve)   

;sort index for time
stime = ibis_date_to_s(starttime)
;sort index for filename, outfile and ts
sind = sort(stime)


;------ restore all speckle files and save in array --------------------------
;ts[tind]
if nfiles ne n_elements(stime) then begin
print,'number of sav files:',nfiles
print,'number of expected files (from log):', n_elements(stime)
print,'did you speckle-reconstruct all images?'
print,'Not recommended to destretch if some observing times are missing!'
print,'Continue (y,n=default)?'
ans = ''
read,ans
if ans ne 'y' then return      
endif


for i=0,nfiles-1 do begin
 restore,sfiles[i],/ve
 specklearr[*,*,i] = simage

 numpart = strmid(sfiles[i],20,17,/rev)
 indarr[i] =  where(strpos(outfile,numpart) ne -1) ;match parts of the filenames

endfor
;specklearr is ordered with t -> not anymore

;indarr relates sfiles with starttimes
currsorting = stime[indarr]
tind = sort(currsorting)

specklearr2 = specklearr[*,*,tind]  ;sorted correcty by time
specklearr = specklearr2
specklearr2 = 0 ;memory




;-------- find range of mask --------
mask = ibis_mask(specklearr[*,*,0],cut=5)
     tmp = where2d(mask eq 1)
     xl = min(tmp[0,*])
     xr = max(tmp[0,*])
     yb = min(tmp[1,*])
     yt = max(tmp[1,*])



;-------- find all images of current target --------
;imgvec will contain same index for images of the same target
imgvec = fltarr(nfiles)
currind = 0

for i=1,nfiles-1 do begin
 if (ts[sind])[i] eq (ts[sind])[i-1] then begin
             imgvec[i] = currind ;same timestamp -> same target        
 endif else begin
             img1 = specklearr[xl:xr,yb:yt,i]
             img2 = specklearr[xl:xr,yb:yt,i-1]
             if correlate(img1,img2) ge 0.7 then begin ;-> still same target
                    imgvec[i] = currind 
             endif else begin
                    currind = currind+1 ;new set of images
                    imgvec[i] = currind ;new index for this set
             endelse
 endelse
endfor


;--- destretch same targets ------------------------------------------------
print,'destretching all speckle images...'
ntgt = diffelement(imgvec) ;number of targets

for i=0,ntgt-1 do begin
 ref = specklearr[xl:xr,yb:yt,where(imgvec eq i)]
;ref contains a subset of speckle images. all images in ref are similar.

 for j=0,(size(ref))[3]-2 do begin 
 dt_detr = 0           ;detrend param
 ds_kernel = [0,64,32] ;subfields
 region = [[400,190],[500,230]] ; some region for rigid alignment

 ;only destretch if seeing is good enough (i.e. two images correlate)
 ;-> not feasible, neither sobel(), nor correl are good indicators

 ;destretch each image wrt previous image
 ref[*,*,j+1] = destretch_ibis(ref[*,*,j], ref[*,*,j+1], DS_KERNEL, region, DT_DETR, shifts=shft_tot)
 endfor

 specklearr[xl:xr,yb:yt,where(imgvec eq i)] = ref
endfor

if keyword_set(movie) then anim_lk,specklearr

;----- save (overwrite) existing speckle images in sav format -------------
print,'saving files...'

for i=0,nfiles-1 do begin
simage=specklearr[*,*,i]>0
save,filename=sfiles[tind[i]],simage
endfor

end


