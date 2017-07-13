;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION ibis_create_struct, Ntotal

     tmp = create_struct('filename', strarr(Ntotal), $
                         'extension', lonarr(Ntotal), $ 
                         'time', dblarr(Ntotal), $
                         'date_obs',strarr(Ntotal),$ ;mod lk
                         'filter', lonarr(Ntotal), $ 
                         'wave', dblarr(Ntotal), $ 
                         'grid', dblarr(Ntotal), $ 
                         'fpi1', fltarr(Ntotal), $ 
                         'fpi2', fltarr(Ntotal), $ 
                         'lcvr1', fltarr(Ntotal), $ 
                         'lcvr2', fltarr(Ntotal), $ 
                         'stokes', strarr(Ntotal), $
                         'frame', dblarr(Ntotal), $ 
                         'dst_time', dblarr(Ntotal), $ 
                         'dst_az', dblarr(Ntotal), $ 
                         'dst_el', dblarr(Ntotal), $ 
                         'dst_tbl', fltarr(Ntotal), $ 
                         'dst_gdrn', dblarr(Ntotal), $ 
                         'dst_xgdr', dblarr(Ntotal), $ 
                         'dst_ygdr', dblarr(Ntotal), $ 
                         'dst_slat', dblarr(Ntotal), $ 
                         'dst_slng', dblarr(Ntotal), $ 
                         'dst_rv', dblarr(Ntotal), $ 
                         'dst_pee', dblarr(Ntotal), $ 
                         'dst_bee', dblarr(Ntotal), $ 
                         'dst_ell', dblarr(Ntotal), $ 
                         'dst_sdim', dblarr(Ntotal), $ 
                         'dst_llevel', dblarr(Ntotal), $ 
                         'dst_see', dblarr(Ntotal), $ 
                         'dst_saz', dblarr(Ntotal), $ 
                         'dst_sel', dblarr(Ntotal), $ 
                         'pt4_time', dblarr(Ntotal), $ 
                         'pt4_pstg', strarr(Ntotal), $ 
                         'pt4_pol', dblarr(Ntotal), $ 
                         'pt4_rstg', strarr(Ntotal), $ 
                         'pt4_ret', dblarr(Ntotal))

RETURN, tmp
END
