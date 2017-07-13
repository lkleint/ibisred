;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION ibis_info, hdr, tmp, filename, extension, index
;mod Oct 1 2013: ensure that index is allowed

if index ge n_elements(tmp.filename) then return,tmp

;-----------------------------------------------
; external card
;-----------------------------------------------

     tmp.filename[index]  = filename
     tmp.extension[index] = extension

;-----------------------------------------------
; Aux card
;-----------------------------------------------

     tmp.time[index]   = time_conv_new(STRMID(SXPAR(hdr, 'DATE-OBS'), 11))
     tmp.date_obs[index]   = SXPAR(hdr, 'DATE-OBS')
     tmp.filter[index] = SXPAR(hdr, 'FILTER')
     tmp.wave[index]   = SXPAR(hdr, 'WAVELNTH')
     tmp.grid[index]   = SXPAR(hdr, 'REL_WAVE')
     tmp.fpi1[index]   = SXPAR(hdr, 'FP1_VOLT')
     tmp.fpi2[index]   = SXPAR(hdr, 'FP2_VOLT')
     tmp.lcvr1[index]  = SXPAR(hdr, 'LCVR1_V')
     tmp.lcvr2[index]  = SXPAR(hdr, 'LCVR2_V')
     tmp.stokes[index] = SXPAR(hdr, 'STOKES')
     tmp.frame[index]  = SXPAR(hdr, 'FRAMENUM')

;-----------------------------------------------
;; DST card
;-----------------------------------------------

     tmp.dst_time[index]   = SXPAR(hdr, 'DST_TIME')
     tmp.dst_az[index]     = SXPAR(hdr, 'DST_AZ')
     tmp.dst_el[index]     = SXPAR(hdr, 'DST_EL')
     tmp.dst_tbl[index]    = SXPAR(hdr, 'DST_TBL')
     tmp.dst_gdrn[index]   = SXPAR(hdr, 'DST_GDRN')
     tmp.dst_xgdr[index]   = SXPAR(hdr, 'DST_XGDR')
     tmp.dst_ygdr[index]   = SXPAR(hdr, 'DST_YGDR')
     tmp.dst_slat[index]   = SXPAR(hdr, 'DST_SLAT')
     tmp.dst_slng[index]   = SXPAR(hdr, 'DST_SLNG')
     tmp.dst_rv[index]     = SXPAR(hdr, 'DST_RV')
     tmp.dst_pee[index]    = SXPAR(hdr, 'DST_PEE')
     tmp.dst_bee[index]    = SXPAR(hdr, 'DST_BEE0')
     tmp.dst_ell[index]    = SXPAR(hdr, 'DST_ELL0')
     tmp.dst_sdim[index]   = SXPAR(hdr, 'DST_SDIM')
     tmp.dst_llevel[index] = SXPAR(hdr, 'DST_LLVL')
     tmp.dst_see[index]    = SXPAR(hdr, 'DST_SEE')
     tmp.dst_saz[index]    = SXPAR(hdr, 'DST_SAZ')
     tmp.dst_sel[index]    = SXPAR(hdr, 'DST_SEL')

;-----------------------------------------------
;; PT4 card
;-----------------------------------------------

     tmp.pt4_time[index] = SXPAR(hdr, 'PT4_TIME')
     tmp.pt4_pstg[index] = SXPAR(hdr, 'PT4_PSTG')
     tmp.pt4_pol[index]  = SXPAR(hdr, 'PT4_POL')
     tmp.pt4_rstg[index] = SXPAR(hdr, 'PT4_RSTG')
     tmp.pt4_ret[index]  = SXPAR(hdr, 'PT4_RET')

RETURN, tmp
END
