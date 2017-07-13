;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; 
;; Main procedure to calibrate the IBIS observations for darks
;; gain, blueshift and image distortions (destretch). The procedure
;; uses the output of ibis_combine.pro as input data.
;;
;; oct 11: modified for flagncam
;;; ### todo: check alignment because params refer to 500x500 images
;;; single and spectroscopy speckle not done yet
;;
;; idl_bridge parallelism added by Tom Schad on 2 Apr 2012
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


PRO ibis_calibrate, o_dir, g_file, d_file, s_file, wl_file, a_file, bshift, $ 
                    pupil, snumber, $ 
                    POL = pol, TPFILE = tpfile, DSTR_IDL = dstr_idl, $
                    SPECKLE = speckle, SINGLE = single, DUAL = dual, $
                    limb=limb, flagncam=flagncam, flag_bridge = flag_bridge

IF (flag_bridge EQ 1) THEN BEGIN

	PRINT,' '
	PRINT,' USING IDL_IDLBRIDGES FOR PARALLEL COMPUTING'

	ibis_calibrate_bridges, o_dir, g_file, d_file, s_file, wl_file, a_file, bshift, $ 
		          pupil, snumber, $ 
                    POL = pol, TPFILE = tpfile, DSTR_IDL = dstr_idl, $
                    SPECKLE = speckle, SINGLE = single, DUAL = dual, $
                    limb=limb, flagncam=flagncam

ENDIF ELSE BEGIN

	PRINT,' '
	PRINT,' NOT USING IDL_IDLBRIDGES FOR PARALLEL COMPUTING'

	ibis_calibrate_no_bridges, o_dir, g_file, d_file, s_file, wl_file, a_file, bshift, $ 
		          pupil, snumber, $ 
                    POL = pol, TPFILE = tpfile, DSTR_IDL = dstr_idl, $
                    SPECKLE = speckle, SINGLE = single, DUAL = dual, $
                    limb=limb, flagncam=flagncam

ENDELSE


END
    
 
