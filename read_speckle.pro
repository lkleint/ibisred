;reads *.final files into IDL array

FUNCTION read_speckle,fnames,xsize,ysize


nfiles = n_elements(fnames)
arr = fltarr(xsize,ysize,nfiles)

FOR i=0,nfiles-1 do begin
   speckle = read_rec(fnames[i], xsize, ysize)
   speckle =  swap_endian(speckle,/swap_if_big_endian)
   index = WHERE( finite(speckle, /NAN) EQ 1, COMPLEMENT = index_valid)
   IF index[0] NE -1 THEN speckle[index] = AVG( speckle[index_valid] )
   arr[*,*,i] = speckle
ENDFOR



return,arr

END
