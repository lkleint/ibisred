;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION readpx, fn

     a = readfits(fn, hdr, EXT = 1, /SILENT)
     a = 0
     x = FIX(sxpar(hdr,'NAXIS1'))
     y = FIX(sxpar(hdr,'NAXIS2'))

RETURN, [x,y]
END
