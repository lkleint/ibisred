FUNCTION ArrayLocation, Pos, onedim=OneDim, $
    sizearr=sizearr, dimension=dimension, zero2one=zero2one

   ; @compile_opt.pro        ; On error, return to caller

InitVar, OneDim, /key

n = n_elements(dimension)

CASE n EQ 0 OF
0: BEGIN
    p = 1L*dimension[0]
    FOR i=1,n-1 DO p = p*dimension[i]
    sza = [n, dimension, 2, p]
END
1: sza = SizeArr
ENDCASE

szp = size(Pos)

CASE OneDim OF
0: BEGIN                            ; 1-dim ---> multi-dim
    IF sza[0] EQ 0 THEN RETURN, -1

    InitVar, Zero2One, /key

    CASE Zero2One OF
    0: BEGIN
        szn = replicate(1,sza[0])
        Loc = lonarr(sza[0],n_elements(Pos))
    END
    1: BEGIN
        szn = float(sza[1:sza[0]]-1)
        Loc = fltarr(sza[0],n_elements(Pos))
    END
    ENDCASE

    n = 1
    FOR i=1,sza[0] DO BEGIN
        Loc[i-1,*] = (Pos/n mod sza[i])/szn[i-1]
        n = n*sza[i]
    ENDFOR

    IF szp[0] GE 1 THEN Loc = reform(Loc,[sza[0],szp[1:szp[0]]],/overwrite)
END
1: BEGIN                            ; Multi-dim ---> 1-dim
    IF sza[0] LT szp[1]-1 THEN RETURN, -1

    Pos = reform(Pos,szp[1],szp[szp[0]+2]/szp[1],/overwrite)

    Loc = reform(Pos[0,*])
    n = 1
    FOR i=1,szp[1]-1 DO BEGIN
        n = n*sza[i]
        Loc = Loc+Pos[i,*]*n
    ENDFOR

    Pos = reform(Pos,szp[1:szp[0]],/overwrite)
    IF szp[0] GE 2 THEN Loc = reform(Loc,szp[2:szp[0]],/overwrite)

END
ENDCASE

RETURN, reform(Loc)  &  END
