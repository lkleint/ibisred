pro log_add_xsingle,path1,xmat,wpoff,wpret,savefile1,lambda
;adds variables that were set to log file
time=systime()

newtext = strarr(10) ;new entries for log

newtext[0] = 'ibis_polcal_xcalc.pro to calculate X matrix for '+string(lambda)+' \\'
newtext[1] = string(time)+'\\'
newtext[2] = ' Wave Plate Offset [deg]: '+string(wpoff)+$
      ', and Retardance [deg]: '+string(wpret)
newtext[3] = '$$ X =  \left( \begin{array}{cccc}'
newtext[4] = string(xmat[0,0])+' & '+string(xmat[1,0])+$
       ' & '+string(xmat[2,0])+' & '+string(xmat[3,0])+' \\'
newtext[5] = string(xmat[0,1])+' & '+string(xmat[1,1])+$
       ' & '+string(xmat[2,1])+' & '+string(xmat[3,1])+' \\'
newtext[6] = string(xmat[0,2])+' & '+string(xmat[1,2])+$
       ' & '+string(xmat[2,2])+' & '+string(xmat[3,2])+' \\'
newtext[7] = string(xmat[0,3])+' & '+string(xmat[1,3])+$
       ' & '+string(xmat[2,3])+' & '+string(xmat[3,3])+' \end{array} \right) $$' 
newtext[8] = 'Saved in: '+string(savefile1)
newtext[9] = ' '


;----- check for underscores (need to be \_ in tex) ----
remove_underscore,newtext

print,'writing variables into log'
log_write,path1,newtext

end
