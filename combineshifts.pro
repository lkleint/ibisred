FUNCTION combineshifts, Nx, Ny, r1, r2, shift1, shift2

  ;; Combines two time series of shift vectors with different
  ;; grid sizes for the purpose of destretching.

  ;; NX and NY are the horizontal and vertical sizes of the solar
  ;; images that need to be destretched.

  ;; The grid size of the resulting shifts is returned in the
  ;; keywords NX_FINAL and NY_FINAL.


  if ((r1)[0] eq -1) then align1 = 1 else align1 = 0  ;true for first run after rigid alignment
  dim1 = size(shift1)
  if (dim1[0] eq 4) then begin
    Nx1 = dim1[2]
    Ny1 = dim1[3]
    Nt1 = dim1[4]
  endif else begin
    Nx1 = dim1[2]
    Ny1 = dim1[3]
    Nt1 = 1
  endelse

  if ((r2)[0] eq -1) then align2 = 1 else align2 = 0  ;never true
  dim2 = size(shift2)
  if (dim2[0] eq 4) then begin
    Nx2 = dim2[2]
    Ny2 = dim2[3]
    Nt2 = dim2[4]
  endif else begin
    Nx2 = dim2[2]
    Ny2 = dim2[3]
    Nt2 = 1
  endelse

  temp = max([Nx1*Ny1, Nx2*Ny2], which)   ; finer grid has more subfields

  if (which eq 0) then sh_final = 0.*shift1 else sh_final = 0.*shift2

  if (align1) then begin  ;for first run after rigid alignment
    FOR i=0, 1 DO BEGIN
      ;; Loop over time
      FOR j=0, Nt1-1 DO BEGIN
          sh_final[i, *, *, j] = - shift1[i, 0, 0, j] + shift2[i, *, *, j]
      ENDFOR
    ENDFOR
  endif else if (align2) then begin ;never true
    FOR i=0, 1 DO BEGIN
      ;; Loop over time
      FOR j=0, Nt1-1 DO BEGIN
          sh_final[i, *, *, j] = + shift1[i, *, *, j] - shift2[i, 0, 0, j]
      ENDFOR
    ENDFOR
  endif else begin
    ; bring to the 'same' grid
    temp = fltarr(Nx, Ny, /nozero)
    if (which eq 0) then begin
      ksizeh = (r2[0,1,0] - r2[0,0,0])/2
      ;; Loop over the two independent directions x and y
      FOR i=0, 1 DO BEGIN
        ;; Loop over time
        FOR j=0, Nt1-1 DO BEGIN
          temp *= 0.
          ; create fine grid from coarse grid
          for l=0,Ny2-1 do for k=0,Nx2-1 do $
            temp[r2[0,k,l]-ksizeh:r2[0,k,l]+ksizeh-1, $
                 r2[1,k,l]-ksizeh:r2[1,k,l]+ksizeh-1] = $
              shift2[i, k, l, j] - r2[i, k, l]
          ; now get the values at the reference positions
          for l=0,Ny1-1 do for k=0,Nx1-1 do $
            sh_final[i, k, l, j] = $
              temp[r1[0, k, l], r1[1, k, l]] + shift1[i, k, l, j]
        ENDFOR
      ENDFOR
    endif else begin
      ksizeh = (r1[0,1,0] - r1[0,0,0])/2
      ;; Loop over the two independent directions x and y
      FOR i=0, 1 DO BEGIN
        ;; Loop over time
        FOR j=0, Nt1-1 DO BEGIN
          temp *= 0.
          ; create fine grid from coarse grid
          for l=0,Ny1-1 do for k=0,Nx1-1 do $
            temp[r1[0,k,l]-ksizeh:r1[0,k,l]+ksizeh-1, $
                 r1[1,k,l]-ksizeh:r1[1,k,l]+ksizeh-1] = $
              shift1[i, k, l, j] - r1[i, k, l]
          ; now get the values at the reference positions
          for l=0,Ny2-1 do for k=0,Nx2-1 do $
            sh_final[i, k, l, j] = $
              temp[r2[0, k, l], r2[1, k, l]] + shift2[i, k, l, j]
        ENDFOR
      ENDFOR
    endelse
  endelse

  return, sh_final
END

