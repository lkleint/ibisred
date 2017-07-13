;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;mod. 31.5.2013: fits_close previously called the wrong variable to
;close, resulting in still open files and therefore crashes if too
;many files were opened. fits_open_lk should not be needed anymore

FUNCTION readni_ncam, fn
                                ;this function finds correct number of
                                ;extensions which may not be equal to
                                ;NIMAGES, especially for whitelight
;      FITS_OPEN_lk, fn, fcb ;lk modified version which closes unit
      FITS_OPEN, fn, fcb 
      ni = fcb.NEXTEND
;      FITS_CLOSE, fn
       FITS_CLOSE,fcb

;       tmp = headfits(fn,ext=1) ;incorrect for aborted files
;       ni = sxpar(tmp,'NIMAGES')

RETURN, ni
END
