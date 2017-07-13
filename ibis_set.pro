;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;; 2011-Jun-22, ali@nso: created.
;;
;; if wl image too large after transformation -> cut
;; if wl image too small after transformation -> make of size nx ny
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION ibis_set, tmp, nx, ny

s = size(tmp)
critx = s[1] ge nx
crity = s[2] ge ny

if critx eq 1 and crity eq 1 then tmpn = tmp[0:nx-1, 0:ny-1]

if critx eq 0 and crity eq 0 then begin
   tmpn = fltarr(nx,ny)
   tmpn[0,0] = tmp
endif

if critx eq 0 and crity eq 1 then begin
   tmpn = fltarr(nx,ny)
   tmpn[0,0] = tmp[*,0:ny-1]
endif

if critx eq 1 and crity eq 0 then begin
   tmpn = fltarr(nx,ny)
   tmpn[0,0] = tmp[0:nx-1,*]
endif

RETURN, tmpn
END
