;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION readni, fn

     a = readfits(fn, hdr, EXT = 0, /SILENT)
     ni = FIX(sxpar(hdr, 'NIMAGES'))

RETURN, ni
END
