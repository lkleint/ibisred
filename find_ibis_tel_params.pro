;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;
;; Search telescope parameter data base and interpolate values to
;; appropriate time specified in nb_time_str
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
FUNCTION find_ibis_tel_params, tp_db, nb_time_str, NB = nb, verbose=verbose
;lk: added keyword verbose to avoid hundreds of lines of text

     s = SIZE(tp_db.time)

     ; FW:
     ; workaround for UT day change
     ; I am being so bold as to assume that this happens only
     ; ONCE in a given dataset ;) (if at all)

     idx = where( ((tp_db.time) - shift(tp_db.time, 1))[1:*] LT 0D ) + 1
   if n_elements(idx) eq 1 then begin ;case: 1 jump in time or none
     if ((idx)[0] gt 0) then $ ;if jump present:
       tp_db.time[idx[0]:*] += 86400.0d
   endif else begin ;more than 1 jump:
  ;for the new file system the files may
  ;not be found in the correct order -> order them by time
    sindex = sort(tp_db.time)
    ntags = n_tags(tp_db) ;number of tags in structure 
   for i=0,ntags-1 do tp_db.(i) = (tp_db.(i))[sindex]
   endelse

;;***************************************************************
;; Convert time string to seconds after midnight
;;***************************************************************

     IF (keyword_set(NB)) THEN $

        nb_time = time_conv_new(STRMID(nb_time_str,12)) $

     ELSE $

     nb_time = time_conv_new(STRMID(nb_time_str,11))

;;***************************************************************
;; Find interpolation index
;;***************************************************************

     tabinv, tp_db.time[0:s[1]-2], nb_time, time_index
     if keyword_set(verbose) then PRINT, nb_time, INTERPOLATE(tp_db.time, time_index)

;;***************************************************************
;; Create output structure
;;***************************************************************

     tb_str = CREATE_STRUCT('time', dblarr(1), 'azim', dblarr(1), $
                            'elev', dblarr(1), 'table', dblarr(1), $
                            'llevel', dblarr(1), 'pee', dblarr(1), $
                            'bee', dblarr(1), 'slat', dblarr(1), $
                            'slng', dblarr(1), 'see', dblarr(1))

;;***************************************************************
;; Interpolate on narrowband time
;;***************************************************************

     tb_str.time   = nb_time
     tb_str.azim   = INTERPOLATE(tp_db.azim, time_index)
     tb_str.elev   = INTERPOLATE(tp_db.elev, time_index)
     tb_str.table  = INTERPOLATE(tp_db.table, time_index)
     tb_str.llevel = INTERPOLATE(tp_db.llevel, time_index)
     tb_str.pee    = INTERPOLATE(tp_db.pee, time_index)
     tb_str.bee    = INTERPOLATE(tp_db.bee, time_index)
     tb_str.see    = INTERPOLATE(tp_db.see, time_index)
     tb_str.slng   = INTERPOLATE(tp_db.slng, time_index)
     tb_str.slat   = INTERPOLATE(tp_db.slat, time_index)

;;***************************************************************
;; Done
;;***************************************************************

     RETURN, tb_str

END
