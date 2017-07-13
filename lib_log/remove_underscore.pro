pro remove_underscore,newtext
;for latex output: _ becomes \_
  
        for i=0,n_elements(newtext)-1 do begin
            res = strsplit(newtext[i],'_', count = n,/extr)

            if n gt 1 then begin
                newstring = res[0]
                for ll=1,n-1 do newstring = newstring+'\_'+res[ll]
                newtext[i] = newstring
            endif
        endfor
 

end
