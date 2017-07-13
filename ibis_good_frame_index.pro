;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;+
; NAME:
;       ibis_good_frame_index
; PURPOSE:
;       return an array of ordered indices of a specified (nominal) length, 
;       but excluding those indices in a provided "bad indices" list
; CATEGORY:
; CALLING SEQUENCE:
;       good_indices = good_frame_index(index_len, bad_index)
; INPUTS:
;       index_len = total length full array of indices.     in
;       bad_index = indices to be excluded from returned index array.     in
; KEYWORD PARAMETERS:
;       indices_valid = will return 1 if good_indices contains a valid list of indices
;       verbose = can be set to 0 to turn off error message. default=1
; OUTPUTS:
;       good_indices =     array of valid indices   
; COMMON BLOCKS:
; NOTES:
;       Bad indices outside of the range of indices given by index_len
;       are ignored.
;
;       If there are no valid good index values (e.g. bad_index contains all the
;           indices in the full index array), the function will return -1, 
;           and should not be used to select valid indices from other arrays.
;       The INDICES_VALID keyword can be set as an output value to provide easy
;           checking on whether good_indices can be used or not.
;       This is important because ARRAY[-1] will not return an error, but rather
;           the last value in ARRAY (i.e. the first index counting from end of the array),
;           which is not what is intended in this case.
;
;       Can be used to "clean" data series as follows:
;
;       help,data_sequence
;           DATA_SEQUENCE    FLOAT = ARRAY[192]
;       bad_data = [17, 41, 163, 164, 191]
;       good_indices =  good_frame_index(192, bad_data, indices_valid=indices_valid)
;       print,good_indices(15:18)
;          15          16          18          19
;       IF indices_valid THEN data_sequence_cln = data_sequence[good_indices]
;       help,data_sequence_cln
;           DATA_SEQUENCE_CLN    FLOAT = ARRAY[187]
;
;
; MODIFICATION HISTORY:
;       Kevin Reardon,        , 2009
;       Kevin Reardon,  August, 2011 -- added better error checking
;
;-
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION ibis_good_frame_index, index_len, bad_index, $
                           VERBOSE=verbose, INDICES_VALID=indices_valid

     IF N_ELEMENTS(verbose) EQ 0 THEN verbose=1

     indices_valid = 1
     index_len_sz = size(index_len,/ST)

     IF (index_len LE 0)  OR (index_len GE 2e7) OR (index_len_sz.N_DIMENSIONS GE 5) THEN BEGIN

        IF KEYWORD_SET(verbose) THEN PRINT,'Warning: invalid index length provided - aborting'
        indices_valid = 0
        RETURN,-1

     ENDIF

     good_index = LINDGEN(index_len)

     bad_index_len = N_ELEMENTS(bad_index)

     IF bad_index_len GE 1 THEN BEGIN

        FOR idx = 0, bad_index_len-1 DO BEGIN

            index_match = WHERE(good_index NE bad_index(idx), match_count)

            ; if no indices were found which do not match the bad_index value,
            ; then there are no valid good indices - set to -1 and flag

            IF match_count GE 1 THEN good_index = good_index(index_match) $
                            ELSE good_index = -1

        ENDFOR

     ENDIF

; provide two checks on the validity of good index, 
; and raise flag is good_index in (conceptually) empty
;   -- note: in current code, good_index_len can actually never be less than 1, but check anyway

     good_index_len = N_ELEMENTS(good_index)
     good_index_max = MAX(good_index)

     IF (good_index_len LT 1) OR (good_index_max LT 0) THEN BEGIN

        indices_valid=0
        IF KEYWORD_SET(verbose) THEN PRINT,'Warning: no valid good indices found'

     ENDIF

RETURN,good_index

END
