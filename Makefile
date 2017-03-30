#FFLAGS=  -O0 -m32 -ffixed-line-length-170 -c -g -fd-lines-as-comments
FFLAGS=  -O0 -ffixed-line-length-170 -c -g -fd-lines-as-comments

OBJ= hypo.o trans.o    \
 datum.o days.o abort.o real8_to_int.o  \
 td_all.o rt_3d.o term.o layer.o lt.o sort_d.o search_l.o velocity.o vl.o   \
 output.o sort_x.o create_dbfile.o   \
 sphere_step.o magni.o origin_time.o   \
 inpt_3d.o search_cr_model.o spline_in.o   \
 clf_3d_1.o   \
 clf_3d_2.o  rt_3d_l.o     \
 nearest_source.o sort_source.o pause.o   \
 dialog_1.o dialog_2.o dialog_2_1.o   \
 dialog_3.o rec_time_name.o valid_arrival.o   \
 iter_1.o iter_2.o break.o cov_matrix.o rms_net_1.o rms_net_2.o spline_value.o   \
 show_matrix.o   \
 eigen.o minv.o  setfn.o\
 model.o xy2fl.o



hypo3d: $(OBJ)
#	gfortran -m32 $(OBJ)  -o hypo3d
	gfortran  $(OBJ)  -o hypo3d

clean:
	rm -f $(OBJ) core hypo3d

hypo.o: hypo.f min_shift.fi error.fi param.fi pname.fi list.fi \
		model_3d.fi onset.fi term.fi stmod.fi 
	gfortran $(FFLAGS) hypo.f
trans.o: trans.f pname.fi
	gfortran $(FFLAGS) trans.f           #  transl.f trans_new.f


datum.o: datum.f term.fi
	gfortran $(FFLAGS) datum.f
days.o: days.f
	gfortran $(FFLAGS) days.f
abort.o: abort.f
	gfortran $(FFLAGS) abort.f
real8_to_int.o: real8_to_int.f
	gfortran $(FFLAGS) real8_to_int.f

td_all.o: td_all.f param.fi list.fi
	gfortran $(FFLAGS) td_all.f
rt_3d.o: rt_3d.f param.fi list.fi term.fi
	gfortran $(FFLAGS) rt_3d.f
term.o: term.f param.fi
	gfortran $(FFLAGS) term.f
layer.o: layer.f param.fi
	gfortran $(FFLAGS) layer.f
lt.o: lt.f param.fi term.fi
	gfortran $(FFLAGS) lt.f
sort_d.o: sort_d.f
	gfortran $(FFLAGS) sort_d.f
search_l.o: search_l.f 
	gfortran $(FFLAGS) search_l.f
velocity.o: velocity.f param.fi term.fi
	gfortran $(FFLAGS) velocity.f
vl.o: vl.f param.fi term.fi
	gfortran $(FFLAGS) vl.f

output.o: output.f param.fi pname.fi data_dir.fi error.fi
	gfortran $(FFLAGS) output.f
sort_x.o: sort_x.f
	gfortran $(FFLAGS) sort_x.f
create_dbfile.o: create_dbfile.f pname.fi data_dir.fi
	gfortran $(FFLAGS) create_dbfile.f

sphere_step.o: sphere_step.f radius.fi list.fi term.fi pname.fi
	gfortran $(FFLAGS) sphere_step.f
magni.o: magni.f param.fi
	gfortran $(FFLAGS) magni.f
origin_time.o: origin_time.f param.fi
	gfortran $(FFLAGS) origin_time.f

inpt_3d.o: inpt_3d.f param.fi pname.fi model_3d.fi data_dir.fi model_dir.fi term.fi source.fi
	gfortran $(FFLAGS) inpt_3d.f
search_cr_model.o: search_cr_model.f error.fi
	gfortran $(FFLAGS) search_cr_model.f
spline_in.o: spline_in.f term.fi
	gfortran $(FFLAGS) spline_in.f

clf_3d_1.o: clf_3d_1.f param.fi data_dir.fi
	gfortran $(FFLAGS) clf_3d_1.f

clf_3d_2.o: clf_3d_2.f  param.fi onset.fi
	gfortran $(FFLAGS) clf_3d_2.f  
rt_3d_l.o: rt_3d_l.f  param.fi list.fi term.fi
	gfortran $(FFLAGS) rt_3d_l.f  

nearest_source.o: nearest_source.f source.fi pname.fi
	gfortran $(FFLAGS) nearest_source.f
sort_source.o: sort_source.f source.fi
	gfortran $(FFLAGS) sort_source.f
pause.o: pause.f pname.fi term.fi
	gfortran $(FFLAGS) pause.f
dialog_1.o: dialog_1.f list.fi pname.fi data_dir.fi term.fi
	gfortran $(FFLAGS) dialog_1.f
dialog_2.o: dialog_2.f pname.fi term.fi
	gfortran $(FFLAGS) dialog_2.f
dialog_2_1.o: dialog_2_1.f pname.fi term.fi
	gfortran $(FFLAGS) dialog_2_1.f

dialog_3.o: dialog_3.f pname.fi param.fi term.fi
	gfortran $(FFLAGS) dialog_3.f

rec_time_name.o: rec_time_name.f param.fi pname.fi term.fi
	gfortran $(FFLAGS) rec_time_name.f
valid_arrival.o: valid_arrival.f pname.fi param.fi term.fi
	gfortran $(FFLAGS) valid_arrival.f

iter_1.o: iter_1.f param.fi pname.fi error.fi
	gfortran $(FFLAGS) iter_1.f
iter_2.o: iter_2.f param.fi
	gfortran $(FFLAGS) iter_2.f
break.o: break.f pname.fi term.fi
	gfortran $(FFLAGS) break.f
cov_matrix.o: cov_matrix.f param.fi
	gfortran $(FFLAGS) cov_matrix.f
rms_net_1.o: rms_net_1.f pname.fi list.fi data_dir.fi term.fi
	gfortran $(FFLAGS) rms_net_1.f
rms_net_2.o: rms_net_2.f pname.fi list.fi term.fi
	gfortran $(FFLAGS) rms_net_2.f
spline_value.o: spline_value.f term.fi
	gfortran $(FFLAGS) spline_value.f

show_matrix.o: show_matrix.f pname.fi list.fi term.fi
	gfortran $(FFLAGS) show_matrix.f

model.o: model.f
	gfortran $(FFLAGS) model.f

eigen.o: eigen.f
	gfortran $(FFLAGS) eigen.f

minv.o: minv.f
	gfortran $(FFLAGS) minv.f

setfn.o: setfn.f
	gfortran $(FFLAGS) setfn.f

xy2fl.o: xy2fl.f
	gfortran $(FFLAGS) xy2fl.f
