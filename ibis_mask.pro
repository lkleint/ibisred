;lk 110209
;returns mask with 1 for beam position, 0 for dark areas
;sobel filter is used for edge detection
;works on single beam images

FUNCTION ibis_mask,img,cut=cut,leftcut=leftcut
;input: img = ideally a flatfield
;       cut = optional cut of several px at edges
;      cutleft = optional cut of x px at left edge (for limb data)

sz=size(img)
sob = sobel(img)
threshold = max(sob)/3.
;deriv = sob - shift(sob,1,1) ;"derivative"
;dthres = max(deriv)/3. ;sobel finds more than one pixel per row.
;therefore, use derivative as other criterion for edge
avgimg = avg(img)/2. ;lk 0325 added /2 because 2010 flatfields have less black edges

filling = 0. ;variable which determines if inside or outside mask
msk = sob
msk[*,*] = 0.

;go through image and find area inside edges
for i=0,sz[1]-1 do begin
 for j=0,sz[2]-1 do begin
      if sob[i,j] gt threshold then begin ;at edges of sobel filter
        msk[i,j] = 1
      endif
      if sob[i,j] le threshold then begin 
        if img[i,j] ge avgimg then begin
             msk[i,j] = 1 ;still inside fov
        endif else begin 
             msk[i,j] = 0
        endelse
      endif  
 endfor
endfor

;option to make mask smaller by n (=cut) pixels
;--- in y direction ---
if keyword_set(cut) then begin
  for i=0,sz[1]-1 do begin
  column = msk[i,*]
  tmp = where(column eq 1) ;indices of inside FOV
  szcut = size(tmp)
    if szcut[0] ne 1 then goto,notinside
    if szcut[1] le 2*cut then begin
          msk[i,*]=0 
    endif else begin
          msk[i,tmp[0:cut]] = 0 ;edge larger at lower end of image
          msk[i,tmp[szcut[1]-cut-1:szcut[1]-1]] = 0
    endelse
  notinside:
  endfor


;--- in x direction ---

  for j=0,sz[2]-1 do begin
  row = msk[*,j]
  tmp = where(row eq 1) ;indices of inside FOV
  szcut = size(tmp)
    if szcut[0] ne 1 then goto,notinside2
    if szcut[1] le 2*cut then begin
          msk[*,j]=0 
    endif else begin
          msk[tmp[0:cut],j] = 0 ;edge larger at lower end of image
          msk[tmp[szcut[1]-cut-1:szcut[1]-1],j] = 0
    endelse
  notinside2:
  endfor
endif


;--- only left x direction ---
if keyword_set(leftcut) then begin
  for j=0,sz[2]-1 do begin
  row = msk[*,j]
  tmp = where(row eq 1) ;indices of inside FOV
  szcut = size(tmp)
    if szcut[0] ne 1 then goto,notinside3
    if szcut[1] le leftcut then begin
          msk[*,j]=0 
    endif else begin
          msk[tmp[0:leftcut],j] = 0 ;edge larger at lower end of image
    endelse
  notinside3:
  endfor
endif

;img2 = shift_diff(sob)
;edges = array_indices(img2,where(img2 lt -300))
;sob[*,*]=0.
;for i=0,(size(edges))[2]-1 do sob[edges[0,i],edges[1,i]] = 1.

return,msk

END 
