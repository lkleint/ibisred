function norm_angle,phiin,limit=limit
; Bring an angle to the 0->360 regime
; (or whatever limit is)

phi=phiin

if (n_elements(limit) eq 0) then limit=360

for i=0l,n_elements(phi)-1 do begin
    while (phi(i) ge limit) do $
      phi(i)=phi(i)-limit
    while (phi(i) lt 0) do $
      phi(i)=phi(i)+limit
endfor

return,phi

end
