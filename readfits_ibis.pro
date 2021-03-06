function READFITS_IBIS, $
                        filename, header, NOSCALE = noscale, NaNvalue = NaNvalue, $
                        SILENT = silent, EXTEN_NO = exten_no, NUMROW = numrow, $
                        POINTLUN = pointlun, STARTROW = startrow
;+
; NAME:
;	READFITS
; PURPOSE:
;	Read a FITS file into IDL data and header variables. Will also
;	read gzip compressed FITS files on Unix machines.
;
; CALLING SEQUENCE:
;	Result = READFITS( Filename,[ Header, /NOSCALE, EXTEN_NO = ,
;			/SILENT , NaNVALUE = , STARTROW = , NUMROW = ] )
;
; INPUTS:
;	FILENAME = Scalar string containing the name of the FITS file  
;		(including extension) to be read. If the filename has
;		a *.gz extension, it will be treated as a gzip compressed
;		file.
;
; OUTPUTS:
;	Result = FITS data array constructed from designated record.
;
; OPTIONAL OUTPUT:
;	Header = String array containing the header from the FITS file.
;
; OPTIONAL INPUT KEYWORDS:
;	NOSCALE - If present and non-zero, then the ouput data will not be
;		scaled using the optional BSCALE and BZERO keywords in the 
;		FITS header.   Default is to scale.
;
;	SILENT - Normally, READFITS will display the size the array at the
;		terminal.  The SILENT keyword will suppress this
;
;	NaNVALUE - This scalar is only needed on Vax architectures.   It 
;		specifies the value to translate any IEEE "not a number"
;		values in the FITS data array.   It is needed because
;		the Vax does not recognize the "not a number" convention.
;
;	EXTEN_NO - scalar integer specify the FITS extension to read.  For
;		example, specify EXTEN = 1 or /EXTEN to read the first 
;		FITS extension.    Extensions are read using recursive
;		calls to READFITS.
;
;	POINT_LUN  -  Position (in bytes) in the FITS file at which to start
;		reading.   Useful if READFITS is called by another procedure
;		which needs to directly read a FITS extension.    Should 
;		always be a multiple of 2880.
;
;	STARTROW - This keyword only applies when reading a FITS extension
;		It specifies the row (scalar integer) of the extension table at
;		which to begin reading. Useful when one does not want to read 
;		the entire table.
;
;	NUMROW -  This keyword only applies when reading a FITS extension. 
;		If specifies the number of rows (scalar integer) of the 
;		extension table to read.   Useful when one does not want to
;		read the entire table.
;
; EXAMPLE:
;	Read a FITS file TEST.FITS into an IDL image array, IM and FITS 
;	header array, H.   Do not scale the data with BSCALE and BZERO.
;
;		IDL> im = READFITS( 'TEST.FITS', h, /NOSCALE)
;
;	If the file contain a FITS extension, it could be read with
;
;		IDL> tab = READFITS( 'TEST.FITS', htab, /EXTEN )
;
;	To read only rows 100-149 of the FITS extension,
;
;		IDL> tab = READFITS( 'TEST.FITS', htab, /EXTEN, 
;					STARTR=100, NUMR = 50 )
;
;	To read in a file that has been compressed:
;
;		IDL> tab = READFITS('test.fits.gz',h)
;
; RESTRICTIONS:
;	(1) Cannot handle random group FITS
;	(2) Cannot read the heap area in a variable length binary table
;
; NOTES:
;	The procedure FXREAD can be used as an alternative to READFITS.
;	FXREAD has the option of reading a subsection of the primary FITS data.
;
; PROCEDURES USED:
;	Functions:   SXPAR, WHERENAN
;	Procedures:  IEEE_TO_HOST, SXADDPAR, FDECOMP
;
; MODIFICATION HISTORY:
;	MODIFIED, Wayne Landsman  October, 1991
;	Added call to TEMPORARY function to speed processing     Feb-92
;	Added STARTROW and NUMROW keywords for FITS tables       Jul-92
;	Work under "windows"   R. Isaacman                       Jan-93
;	Check for SIMPLE keyword in first 8 characters           Feb-93
;	Removed EOF function for DECNET access                   Aug-93
;	Work under "alpha"                                       Sep-93
;       Null array processing fixed:  quotes in a message 
;          properly nested, return added.  Affected case when
;          readfits called from another procedure.   R.S.Hill    Jul-94
;	Correct size of variable length binary tables W.Landsman Dec-94
;	To read in compressed files on Unix systems. J. Bloch	 Jan-95
;-
  On_error,2                    ;Return to user

; Check for filename input

   if N_params() LT 1 then begin		
      print,'Syntax - im = READFITS( filename, [ h, /NOSCALE, /SILENT, '
      print,'                 NaNValue = ,EXTEN_NO =, STARTROW = , NUMROW = ] )'
      return, -1
   endif

;
;	Determine if the input file is compressed with gzip by the extension
;
   fdecomp,filename,disk,dir,name,ext,ver
   if ext eq "gz" then gzip = 1 else gzip = 0
;
;
;
   silent = keyword_set( SILENT )
   if not keyword_set( EXTEN_NO ) then exten_no = 0

; Open file and read header information

    if gzip then begin

	if not (silent) then $
	    message,"Input file compressed with gzip",/inform
	spawn,"gzip -l "+filename,unit=unit
	tmp=" "
	readf,unit,tmp,format="(A80)"
	compsize=0L
	filesize=0L
	dum = " "
	origpcnt = " "
	readf,unit,compsize,filesize,origpcnt
	free_lun,unit
	if not (silent) then begin

	    message,"Compressed size:"+string(compsize)+" bytes",/inform
	    message,"Uncompressed size:"+string(filesize)+" bytes",/inform
	    message,"% Compress/Original Filename: "+origpcnt,/inform

	endif

	spawn,"gzip -cd "+filename,unit=unit
	file=fstat(unit)

    endif else begin

  	openr, unit, filename, /GET_LUN, /BLOCK, /SWAP_ENDIAN
        file = fstat(unit)

    endelse

; On VMS, FITS file must be fixed record length (actual length doesn't matter)

        if !VERSION.OS EQ 'vms' then begin
            if file.rec_len EQ 0 then $
                 message,'WARNING - ' + strupcase(filename) + $
                  ' is not a fixed record length file',/CONT
        endif

        if keyword_set( POINTLUN) then begin

		if gzip then begin

			tmp=bytarr(pointlun)
			readu,unit,tmp

		endif else begin

			point_lun, unit, pointlun 

		endelse

	endif else pointlun = 0

	if gzip then nbytesleft = filesize - pointlun else $
		nbytesleft = file.size - pointlun 

      	hdr = bytarr( 80, 36, /NOZERO )
        if nbytesleft LT 2880 then begin 
           free_lun, unit
           message, 'ERROR - EOF encountered while reading FITS header'
        endif
        readu, unit, hdr
	nbytesleft = nbytesleft - 2880
        header = string( hdr > 32b )
        if ( pointlun EQ 0 ) then $
		if strmid( header(0), 0, 8)  NE 'SIMPLE  ' then begin
		free_lun, unit
		message,'ERROR - Header does not contain required SIMPLE keyword'
        endif

        endline = where( strmid(header,0,8) EQ 'END     ', Nend )
        if Nend GT 0 then header = header( 0:endline(0) ) 

        while Nend EQ 0 do begin
            if nbytesleft LT 2880 then begin
                free_lun, unit 
                message, 'ERROR - EOF encountered while reading FITS header'
             endif
        readu, unit, hdr
        nbytesleft = nbytesleft - 2880
        hdr1 = string( hdr > 32b )
        endline = where( strmid(hdr1,0,8) EQ 'END     ', Nend )
        if Nend GT 0 then hdr1 = hdr1( 0:endline(0) ) 
        header = [ header, hdr1 ]
        endwhile

; Get parameter values

 Naxis = sxpar( header, 'NAXIS' )

 bitpix = sxpar( header, 'BITPIX' )
 if !ERR EQ -1 then message, $
        'ERROR - FITS header missing required BITPIX keyword'
 gcount = sxpar( header, 'GCOUNT') > 1
 pcount = sxpar( header, 'PCOUNT')

 case BITPIX of 
	   8:	IDL_type = 1          ; Byte
	  16:	IDL_type = 2          ; Integer*2
	  32:	IDL_type = 3          ; Integer*4
	 -32:   IDL_type = 4          ; Real*4
         -64:   IDL_type = 5          ; Real*8
        else:   message,'ERROR - Illegal value of BITPIX (= ' +  $
                               strtrim(bitpix,2) + ') in FITS header'
  endcase     

; Check for dummy extension header

 if Naxis GT 0 then begin 
        Nax = sxpar( header, 'NAXIS*' )	  ;Read NAXES
        ndata = nax(0)
        if naxis GT 1 then for i = 2, naxis do ndata = ndata*nax(i-1)

  endif else ndata = 0

  nbytes = (abs(bitpix)/8) * gcount * (pcount + ndata)

  if pointlun EQ 0 then begin 

          extend = sxpar( header, 'EXTEND') 
   	  if !ERR EQ -1 then extend = 0
          if not ( SILENT) then begin
          if (exten_no EQ 0) then message, $
               'File may contain FITS extensions',/INF  $
          else if not EXTEND  then message, $
               'ERROR - EXTEND keyword not found in primary header',/CON
          endif

  endif

  if keyword_set( EXTEN_NO ) then begin

           nrec = long(( nbytes +2879)/ 2880)

	   if gzip then pointlun = filesize - nbytesleft else $
           point_lun, -unit, pointlun          ;Current position

           pointlun = pointlun + nrec*2880l     ;Next FITS extension
           free_lun, unit
           im = READFITS( filename, header, POINTLUN = pointlun, $
                          SILENT = silent, NUMROW = numrow, $
                          EXTEN = exten_no - 1, STARTROW = startrow )
           return, im
  endif                  


 if nbytes EQ 0 then begin
	if not SILENT then message, $
  	        "FITS header has NAXIS or NAXISi = 0,  no data array read",/CON
	free_lun, unit
	return,-1
 endif

; Check for FITS extensions, GROUPS

 groups = sxpar( header, 'GROUPS' ) 
 if groups then MESSAGE,'WARNING - FITS file contains random GROUPS', /CON

; If an extension, did user specify row to start reading, or number of rows
; to read?

   if not keyword_set(STARTROW) then startrow = 0
   if not keyword_set(NUMROW) then $
         if naxis GE 2 then numrow = nax(1) else numrow = 0
   if (pointlun GT 0) and ((startrow NE 0) or (numrow NE 0)) then begin
        nax(1) = nax(1) - startrow    
        nax(1) = nax(1) < numrow
        sxaddpar, header, 'NAXIS2', nax(1)
	if gzip then pointlun = filesize - nbytesleft else $
        point_lun, -unit, pointlun          ;Current position
        pointlun = pointlun + startrow*nax(0)      ;Next FITS extension
	if gzip then begin
		tmp=bytarr(startrow*nax(0))
		readu,unit,tmp
	endif else point_lun, unit, pointlun
    endif

  if not (SILENT) then begin   ;Print size of array being read

         if pointlun GT 0 then begin
                   xtension = sxpar( header, 'XTENSION' )
                   if !ERR GE 0 then message, $ 
                      'Reading FITS extension of type ' + xtension, /INF else $
                   message,'ERROR - Header missing XTENSION keyword',/CON
         endif
         snax = strtrim(NAX,2)
         st = snax(0)
         if Naxis GT 1 then for I=1,NAXIS-1 do st = st + ' by '+SNAX(I) $
                            else st = st + ' element'
         message,'Now reading ' + st + ' array',/INFORM    
   endif

; Read Data in a single I/O call

    data = make_array( DIM = nax, TYPE = IDL_type, /NOZERO)

    if (pointlun GT 0) and (pcount GT 0) then message, /INF, $
	'Warning - Heap area in variable length binary table not read'
    if nbytesleft LT N_elements(data) then message, $
	'ERROR - End of file encountered while reading data array'
    readu, unit, data
    free_lun, unit

; If necessary, replace NaN values, and convert to host byte ordering
        
   if keyword_set( NaNvalue) then NaNpts = whereNaN( data, Count)
   ieee_to_host, data
   if keyword_set( NaNvalue) then  $
           if ( Count GT 0 ) then data( NaNpts) = NaNvalue

; Scale data unless it is an extension, or /NOSCALE is set

   if not keyword_set( NOSCALE ) and (PointLun EQ 0 ) then begin

          bscale = float( sxpar( header, 'BSCALE' ))

; Use "TEMPORARY" function to speed processing.  

	  if !ERR NE -1  then $ 
	       if ( Bscale NE 1. ) then begin
                   data = temporary(data) * Bscale 
                   sxaddpar, header, 'BSCALE', 1.
   	       endif

         bzero = float( sxpar ( header, 'BZERO' ) )
	 if !ERR NE -1  then $
	       if (Bzero NE 0) then begin
                     data = temporary( data ) + Bzero
                     sxaddpar, header, 'BZERO', 0.
	       endif
	   endif

; Return array

	return, data    
 end 
