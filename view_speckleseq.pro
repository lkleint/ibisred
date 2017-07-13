;pro view_speckleseq
;display movie (time ordered) of all speckle images for that day
;call from wl/results/

files=file_search('*.sav',count=nfiles)
files=find_file('*.sav',count=nfiles)
specklearr2 = fltarr(500,1000,nfiles)

restore,'../../calibration/6302_align_params.sav',/ve


print,nfiles
for i=0,nfiles-1 do begin
restore,files[i],/ve
tmp = rotate(simage,rotindex)
tmp = newwl(tmp,f1,f2,r,2*sfl)
tmp = ibis_set(tmp,1000,1000)
specklearr2[*,*,i] = tmp[0:499,0:999]
endfor

restore,'../../calibration/speckle_sid.sav',/ve

;--- sort by wl
sz= size(speckle_db.outfile)
wl = fltarr(sz[2])
for i=0,sz[2]-1 do wl[i] = strmid(speckle_db.outfile[0,i],7,4,/rev)
wlsort = sort(wl) ;ascending file names for *.final -> also for .sav


dbs = speckle_db.starttime[*,wlsort]
b = where(dbs ne '')
a=sort(dbs[b])

obstimes = (dbs[b])[a] ;correctly sorted, check: plot,anytim(obstimes),/xs,/ys

sid = fltarr(n_elements(obstimes))
for i=0,n_elements(obstimes)-1 do sid[i] = time_conv_new(strmid(obstimes[i],11,12))

tmp=sort(sid)

anim_lk,scaleimg(specklearr2[*,*,a],left=400,right=5400)
;anim_lk,scaleimg(specklearr2[*,*,tmp],left=400,right=5400)

;write_jpeg,'../../spot_1418.jpg',scaleimg(specklearr2[*,*,a[5]],left=400,right=5400),quality=100

;openw,2,'~/lsa/speckle_log_110924.txt'
;for i=0,nfiles-1 do printf,2,i,' ',files[a[i]],' ',obstimes[i]
;close,2


;clockmov,specklearr2[*,*,a],obstimes=sid
;clockmov,scaleimg(specklearr2[70:460,35:980,a[0:309]],left=1000,right=6400), obstimes=sid[0:309],/png,savefile='/hao/lsa2/kleintl/reduc/20110925/results_000/movie/speckle_'

end
