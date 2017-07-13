PRO InitVar, X, Xinit, keyword_var=keyword_var, set=set, count=count

 ;   @compile_opt.pro            ; On error, return to caller

CASE keyword_set(keyword_var) OF

0: BEGIN

    IF n_elements(count) EQ 0 THEN count = 0

    IF n_elements(X) EQ count THEN BEGIN
        CASE n_elements(Xinit) OF
        0   : message, /info, 'no data for initialization'
        ELSE: IF arg_present(set) THEN set = Xinit ELSE X = Xinit
        ENDCASE
    ENDIF ELSE  $
        set = X

END

1: X = keyword_set(X)

ENDCASE

RETURN  &  END
