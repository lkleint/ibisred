;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;;
;;
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION ibis_blueshift_map, flat, wrange, npoints, aps, info_str, $ 
                             Pol = pol, SINGLE = single, DUAL = dual, $
                             STOKES = stokes, flagncam=flagncam

;;***************************************************************
;; get dimensions
;;***************************************************************

     dim   = size(flat)
     Nx    = dim[1]
     Ny    = dim[2]
     Nwave = dim[3]
     IF KEYWORD_SET(STOKES) THEN Npol = stokes ELSE Npol = 1
     IF not keyword_set(flagncam) then flagncam = 0

;;***************************************************************
;; create output structure
;;***************************************************************

     offset_map = create_struct('cog', dblarr(Nx,Ny), 'cog_fit', dblarr(Nx,Ny), $
                                'poly', dblarr(Nx,Ny), 'poly_fit', dblarr(Nx,Ny))

;;***************************************************************
;; Determine blueshift maps
;;***************************************************************
;;---------------------------------------------------------
;;---------------------------------------------------------
;; Distinguish Modes : Spectropolarimetric mode
;;---------------------------------------------------------
;;---------------------------------------------------------

     IF KEYWORD_SET(POL) THEN BEGIN

        print,'-------------------------------aaa-----------'
        print,'ok, we are in spectropolarimetric mode ....'
        print,'-------------------------------------------'

        ;;
        ;; get wavelength scale and make equidistant on 10 mA grid
        ;;
        
        if flagncam ne 1 then $ ;old cam
        wtmp = REFORM((info_str.wavelength_offset)[*,0]) else $
        wtmp = reform(info_str.wave) ;new cam


        ;index = uniq(wtmp) ;does not work with multiple flat series
        ;wscale = wtmp(index)  

        ;new 110217 lk:
        wscale=diffelement(wtmp,/name,/double) ;array of observed wl offsets
 
        wscale = wscale[sort(wscale)]
        wscale      = wscale[wrange[0]:wrange[1]]
        nwscale     = ROUND((MAX(wscale) - MIN(wscale))/0.01d)          
        wscale_equi = FINDGEN(nwscale)/(nwscale-1) $ 
                          * (MAX(wscale) - MIN(wscale)) + MIN(wscale)

        IF keyword_set(DUAL) then begin

           print,'---------------------------------------------'
           print,'DUAL-BEAM mode ....  '
           print,'---------------------------------------------'

           ;; get both aperture indeces

           idxxl  = where(aps[0:Nx/2-1,*] ge 1)
           idxxr  = where(aps[Nx/2:Nx-1,*] ge 1)

           ;; Get the aperture for left and right channel

           idxl  = where_n(aps[0:Nx/2-1,*] GE 1)
           idxlx = idxl[*,0]
           idxly = idxl[*,1]
           Numl = N_ELEMENTS(idxlx)

           idxr  = where_n(aps[Nx/2:Nx-1,*] GE 1)
           idxrx = idxr[*,0]
           idxry = idxr[*,1]
           Numr = N_ELEMENTS(idxrx)

           ;; Determine the position of the average profile.

           avgprof_l=fltarr(Nwave, Npol)
           avgprof_r=fltarr(Nwave, Npol)

           FOR j = 0,Npol-1 DO BEGIN
               FOR i = 0, Nwave-1 DO avgprof_l[i, j] = mean((REFORM(flat[0:Nx/2-1,*,i,j]))[idxxl])
               FOR i = 0, Nwave-1 DO avgprof_r[i, j] = mean((REFORM(flat[Nx/2:Nx-1,*,i,j]))[idxxr])
           ENDFOR

           avgoffs_l=fltarr(Npol)
           avgoffs_r=fltarr(Npol)
           avgoffs1_l=fltarr(Npol)
           avgoffs1_r=fltarr(Npol)

           FOR j = 0,Npol-1 DO BEGIN

               ; make equidistant

               pavg = interpol(reform(avgprof_l[*,j]), wscale, wscale_equi, /quadratic)
               lpff_pol, pavg, tmp
               avgoffs_l[j] = tmp ;pos of line core with fft method
               avgoffs1_l[j] = parmin_ibis(pavg,npoints) ;pos of line core by parabol. fit
;              avgoffs and avgoffs1 seem to be the same

               pavg = interpol(reform(avgprof_r[*,j]), wscale, wscale_equi, /quadratic)
               lpff_pol, pavg, tmp
               avgoffs_r[j] = tmp
               avgoffs1_r[j] = parmin_ibis(pavg,npoints)

           ENDFOR

           ;; determine offsets over the FOV.

           bshift_cog_l = fltarr(Nx/2, Ny, Npol)
           bshift_poly_l = fltarr(Nx/2, Ny, Npol)
           bshift_cog_r = fltarr(Nx/2, Ny, Npol)
           bshift_poly_r = fltarr(Nx/2, Ny, Npol)

           flat_l = flat[0:Nx/2-1,*,*,*] ;left part of all flats
           flat_r = flat[Nx/2:Nx-1,*,*,*] ;right part of all flats

           FOR j = 0, Npol-1 DO BEGIN ;for all pol. states

               FOR i = 0L, Numl-1 DO BEGIN ;for all pts in mask

                   p = interpol(reform(flat_l[idxlx[i], idxly[i], *, j]), wscale, wscale_equi, /quadratic) ;quadratic profile interpolation
                   lpff_pol, p, tmp ;line core pos with fft
 ;two versions for the same thing: get difference of line center position wrt avg profile:
                   bshift_cog_l[idxlx[i], idxly[i], j] = tmp - avgoffs_l[j]
                   bshift_poly_l[idxlx[i], idxly[i], j] = parmin_ibis(p, npoints) - avgoffs1_l[j]

               ENDFOR

               FOR i=0L, Numr-1 DO BEGIN
                   p = interpol(reform(flat_r[idxrx[i], idxry[i], *, j]), wscale, wscale_equi, /quadratic)
                   lpff_pol, p, tmp
                   bshift_cog_r[idxrx[i], idxry[i], j] = tmp - avgoffs_r[j]
                   bshift_poly_r[idxrx[i], idxry[i], j] = parmin_ibis(p, npoints) - avgoffs1_r[j]
              ENDFOR

           ENDFOR

           ;; average over polarisation states

           IF (Npol EQ 1) THEN BEGIN

              bshift_cog_l = bshift_cog_l
              bshift_poly_l = bshift_poly_l

              bshift_cog_r = bshift_cog_r
              bshift_poly_r = bshift_poly_r

           ENDIF ELSE BEGIN

                 bshift_cog_l = avg(bshift_cog_l, 2)
                 bshift_poly_l = avg(bshift_poly_l, 2)

                 bshift_cog_r = avg(bshift_cog_r, 2)
                 bshift_poly_r = avg(bshift_poly_r, 2)

           ENDELSE

           ;; put together

           bshift_cog = dblarr(Nx,Ny)
           bshift_poly = dblarr(Nx,Ny)

           bshift_cog[0:Nx/2-1,*] = bshift_cog_l
           bshift_poly[0:Nx/2-1,*] = bshift_poly_l
           bshift_cog[Nx/2:Nx-1,*] = bshift_cog_r
           bshift_poly[Nx/2:Nx-1,*] = bshift_poly_r

           ;; now do the surface fit of the two offset maps
           ;; divide FOV. First the left side, then the right side.

           bshift_cog_fit = dblarr(Nx,Ny)
           bshift_poly_fit = dblarr(Nx,Ny)

           bshift_cog_fit_left = dblarr(Nx/2,Ny)
           bshift_poly_fit_left = dblarr(Nx/2,Ny)
           bshift_cog_fit_right = dblarr(Nx/2,Ny)
           bshift_poly_fit_right = dblarr(Nx/2,Ny)

           xmatrix = rebin(findgen(Nx/2), Nx/2, Ny)                      
           ymatrix = rebin(reform(findgen(Ny), 1, Ny), Nx/2, Ny)

           ;; left

           xmatrix = rebin(findgen(Nx/2), Nx/2, Ny)                      
           ymatrix = rebin(reform(findgen(Ny), 1, Ny), Nx/2, Ny)

           zmatrix_cog = bshift_cog_l
           zmatrix_poly = bshift_poly_l

           index = where(zmatrix_cog NE zmatrix_cog[10,10]) 
           vectors_cog_left = [reform(xmatrix(index), 1, n_elements(index)) , reform(ymatrix(index), $ 
                               1, n_elements(index)), reform(zmatrix_cog(index), 1, n_elements(index))]

           vectors_poly_left = [reform(xmatrix(index), 1, n_elements(index)) , reform(ymatrix(index), $ 
                                1, n_elements(index)), reform(zmatrix_poly(index), 1, n_elements(index))]

           nterms = 2

           ;sfit: surface fit of degree nterms (/max=1 max degree in each dim is 1)
           fit_cog_left = sfit(vectors_cog_left, nterms, /irr, kx=coeff, /max)   
           fit_poly_left = sfit(vectors_poly_left, nterms, /irr, kx=coeff, /max)   
           ;resulting coeff for surface are stored in coeff variable

           bshift_cog_fit_left[vectors_cog_left[0, *], vectors_cog_left[1, *]] = fit_cog_left
           bshift_poly_fit_left[vectors_poly_left[0, *], vectors_poly_left[1, *]] = fit_poly_left

           ;; right

           zmatrix_cog = bshift_cog_r
           zmatrix_poly = bshift_poly_r

           index = where(zmatrix_cog NE zmatrix_cog[10,10]) 
           vectors_cog_right = [reform(xmatrix(index), 1, n_elements(index)) , reform(ymatrix(index), $ 
                                1, n_elements(index)), reform(zmatrix_cog(index), 1, n_elements(index))]

           vectors_poly_right = [reform(xmatrix(index), 1, n_elements(index)) , reform(ymatrix(index), $ 
                                 1, n_elements(index)), reform(zmatrix_poly(index), 1, n_elements(index))]

           nterms = 2

           fit_cog_right = sfit(vectors_cog_right, nterms, /irr, kx=coeff, /max)   
           fit_poly_right = sfit(vectors_poly_right, nterms, /irr, kx=coeff, /max)   

           bshift_cog_fit_right[vectors_cog_right[0, *], vectors_cog_right[1, *]] = fit_cog_right
           bshift_poly_fit_right[vectors_poly_right[0, *], vectors_poly_right[1, *]] = fit_poly_right

           ;; put it all together

           bshift_cog_fit[0:Nx/2-1,*] = bshift_cog_fit_left
           bshift_poly_fit[0:Nx/2-1,*] = bshift_poly_fit_left

           bshift_cog_fit[Nx/2:Nx-1,*] = bshift_cog_fit_right
           bshift_poly_fit[Nx/2:Nx-1,*] = bshift_poly_fit_right

           offset_map.cog = bshift_cog * 0.01d
           offset_map.cog_fit = bshift_cog_fit * 0.01d
           offset_map.poly = bshift_poly * 0.01d
           offset_map.poly_fit = bshift_poly_fit * 0.01d

        ENDIF

        IF keyword_set(SINGLE) then begin

           print,'---------------------------------------------'
           print,'SINGLE-BEAM mode ....  '
           print,'---------------------------------------------'

           ;; get both aperture indeces

           idxn  = where(aps ge 1)

           ;; Get the aperture for left and right channel

           idx  = where_n(aps GE 1)
           idxx = idx[*,0]
           idxy = idx[*,1]
           Num = N_ELEMENTS(idxx)

           ;; Determine the position of the average profile.

           avgprof=fltarr(Nwave, Npol)

           FOR j = 0,Npol-1 DO BEGIN
               FOR i = 0, Nwave-1 DO avgprof[i, j] = mean((REFORM(flat[*,*,i,j]))[idxn])
           ENDFOR

           avgoffs=fltarr(Npol)
           avgoffs1=fltarr(Npol)

           FOR j = 0,Npol-1 DO BEGIN

               ; make equidistant

               pavg = interpol(reform(avgprof[*,j]), wscale, wscale_equi, /quadratic)
               lpff_pol, pavg, tmp
               avgoffs[j] = tmp
               avgoffs1[j] = parmin_ibis(pavg,npoints)

           ENDFOR

           ;; determine offsets over the FOV.

           bshift_cog = fltarr(Nx, Ny, Npol)
           bshift_poly = fltarr(Nx, Ny, Npol)

           flat = flat

           FOR j = 0, Npol-1 DO BEGIN
              print,'pol state:',j,' of',npol-1
               FOR i = 0L, Num-1 DO BEGIN

                   p = interpol(reform(flat[idxx[i], idxy[i], *, j]), wscale, wscale_equi, /quadratic)
                   lpff_pol, p, tmp
                   bshift_cog[idxx[i], idxy[i], j] = tmp - avgoffs[j]
                   bshift_poly[idxx[i], idxy[i], j] = parmin_ibis(p, npoints) - avgoffs1[j]

               ENDFOR

           ENDFOR

           ;; average over polarisation states

           IF (Npol NE 1) THEN BEGIN

              bshift_cog = avg(bshift_cog, 2)
              bshift_poly = avg(bshift_poly, 2)

           ENDIF 

           ;; now do the surface fit of the two offset maps
           ;; divide FOV. First the left side, then the right side.
  ;LK: comment wrong, only one map now. Also commented out double lines

           bshift_cog_fit = dblarr(Nx,Ny)
           bshift_poly_fit = dblarr(Nx,Ny)

   ;        bshift_cog_fit = dblarr(Nx,Ny)
   ;        bshift_poly_fit = dblarr(Nx,Ny)

           xmatrix = rebin(findgen(Nx), Nx, Ny)                      
           ymatrix = rebin(reform(findgen(Ny), 1, Ny), Nx, Ny)

           ;; left

    ;       xmatrix = rebin(findgen(Nx), Nx, Ny)                      
    ;       ymatrix = rebin(reform(findgen(Ny), 1, Ny), Nx, Ny)

           zmatrix_cog = bshift_cog
           zmatrix_poly = bshift_poly

           index = where(zmatrix_cog NE zmatrix_cog[10,10]) 
           vectors_cog = [reform(xmatrix(index), 1, n_elements(index)) , reform(ymatrix(index), $ 
                               1, n_elements(index)), reform(zmatrix_cog(index), 1, n_elements(index))]

           vectors_poly = [reform(xmatrix(index), 1, n_elements(index)) , reform(ymatrix(index), $ 
                                1, n_elements(index)), reform(zmatrix_poly(index), 1, n_elements(index))]

           nterms = 2

           fit_cog = sfit(vectors_cog, nterms, /irr, kx=coeff, /max)   
           fit_poly = sfit(vectors_poly, nterms, /irr, kx=coeff, /max)   

           bshift_cog_fit[vectors_cog[0, *], vectors_cog[1, *]] = fit_cog
           bshift_poly_fit[vectors_poly[0, *], vectors_poly[1, *]] = fit_poly

           offset_map.cog = bshift_cog * 0.01d
           offset_map.cog_fit = bshift_cog_fit * 0.01d
           offset_map.poly = bshift_poly * 0.01d
           offset_map.poly_fit = bshift_poly_fit * 0.01d

        ENDIF

     ENDIF ELSE BEGIN

;;---------------------------------------------------------
;;---------------------------------------------------------
;; Distinguish Modes : Spectroscopic mode
;;---------------------------------------------------------
;;---------------------------------------------------------

        print,'-------------------------------------'
        print,'ok, we are in spectroscopic mode ....'
        print,'-------------------------------------'

        ;; get wavelength scale and make equidistant on 10 mA grid

        if flagncam ne 1 then wtmp = reform((info_str.wavelength_offset)[*,0]) else $
                              wtmp = reform(info_str.wave)
 
        wscale=diffelement(wtmp,/name,/double) ;array of observed wl offsets
 
        wscale = wscale[wrange[0]:wrange[1]]
        nwscale = (max(wscale) - min(wscale))/0.01d           
        wscale_equi = findgen(nwscale)/(nwscale-1) $ 
                      * (max(wscale) - min(wscale)) + min(wscale)
  
        ;; get aperture indeces

        idx  = where(aps ge 1, Num)

        avgprof=fltarr(Nwave)

        for i=0, Nwave-1 do avgprof[i] = mean((reform(flat[*,*,i]))[idx])

        ; make equidistant

        avgprof = interpol(avgprof,wscale,wscale_equi, /quadratic)

        ; determine average position

        lpff_pol, avgprof, tmp
        avgoffs = tmp
        avgoffs1 = parmin_ibis(avgprof, npoints)

        ;; determine offsets over the FOV.

        bshift_cog = fltarr(Nx, Ny)
        bshift_poly = fltarr(Nx, Ny)

        FOR i=0L, Num-1 DO BEGIN

            p = reform(flat[idx[i] mod Nx, idx[i]/Nx, *])
            p = interpol(p,wscale,wscale_equi, /quadratic)

            lpff_pol, p, tmp
            bshift_cog[idx[i] mod Nx, idx[i]/Nx] = tmp - avgoffs
            bshift_poly[idx[i] mod Nx, idx[i]/Nx] = parmin_ibis(p,npoints) - avgoffs1

        ENDFOR
  
        ;; now do the fit of the offset map to start with the
        ;; voodoo 

        xmatrix = rebin(findgen(Nx), Nx, Ny)                      
        ymatrix = rebin( reform(findgen(Ny), 1, Ny), Nx, Nx)
        zmatrix_cog = bshift_cog
        zmatrix_poly = bshift_poly

        index = where(bshift_cog NE bshift_cog[10,10]) 
        vectors_cog = [reform(xmatrix(index), 1, n_elements(index)) , reform(ymatrix(index), $ 
                     1, n_elements(index)), reform(zmatrix_cog(index), 1, n_elements(index))]

        vectors_poly = [reform(xmatrix(index), 1, n_elements(index)) , reform(ymatrix(index), $ 
                     1, n_elements(index)), reform(zmatrix_poly(index), 1, n_elements(index))]

        nterms = 2

        fit_cog = sfit(vectors_cog, nterms, /irr, kx=coeff, /max)   
        fit_poly = sfit(vectors_poly, nterms, /irr, kx=coeff, /max)   

        bshift_cog_fit = dblarr(Nx,Ny)
        bshift_poly_fit = dblarr(Nx,Ny)
        bshift_cog_fit[vectors_cog[0, *], vectors_cog[1, *]] = fit_cog
        bshift_poly_fit[vectors_poly[0, *], vectors_poly[1, *]] = fit_poly

        offset_map.cog = bshift_cog * 0.01d
        offset_map.cog_fit = bshift_cog_fit * 0.01d
        offset_map.poly = bshift_poly * 0.01d
        offset_map.poly_fit = bshift_poly_fit * 0.01d

  endelse

  return, offset_map

END
