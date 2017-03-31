
FC=gfortran

#FFLAGS=  -O0 -m32 -ffixed-line-length-170 -c -g -fd-lines-as-comments
#LDFLAGS= -m32
FFLAGS=-g -O0 -ffixed-line-length-170 -c -fd-lines-as-comments
LDFLAGS=

OBJ= hypo.o trans.o    \
 datum.o days.o abort.o real8_to_int.o  \
 td_all.o rt_3d.o term.o layer.o lt.o sort_d.o search_l.o velocity.o vl.o   \
 output.o sort_x.o create_dbfile.o   \
 sphere_step.o magni.o origin_time.o   \
 inpt_3d.o search_cr_model.o spline_in.o   \
 nearest_source.o sort_source.o \
 dialog_1.o dialog_2.o dialog_2_1.o   \
 dialog_3.o rec_time_name.o \
 iter_1.o iter_2.o cov_matrix.o rms_net_1.o rms_net_2.o spline_value.o   \
 eigen.o minv.o \
 model.o xy2fl.o

#unused
# clf_3d_1.o clf_3d_2.o rt_3d_l.o    
# valid_arrival.o   
# show_matrix.o 
# pause.o
# setfn.o
# break.o

hypo3d: $(OBJ)
	$(FC)  $(LDFLAGS) $(OBJ)  -o hypo3d

clean:
	rm $(OBJ) hypo3d

hypo.o: hypo.f min_shift.fi error.fi param.fi pname.fi list.fi \
		model_3d.fi term.fi stmod.fi
 
	$(FC) $(FFLAGS) hypo.f
trans.o: trans.f pname.fi
	$(FC) $(FFLAGS) trans.f


datum.o: datum.f term.fi
	$(FC) $(FFLAGS) datum.f
days.o: days.f
	$(FC) $(FFLAGS) days.f
abort.o: abort.f
	$(FC) $(FFLAGS) abort.f
real8_to_int.o: real8_to_int.f
	$(FC) $(FFLAGS) real8_to_int.f

td_all.o: td_all.f param.fi list.fi
	$(FC) $(FFLAGS) td_all.f
rt_3d.o: rt_3d.f param.fi list.fi term.fi
	$(FC) $(FFLAGS) rt_3d.f
term.o: term.f param.fi
	$(FC) $(FFLAGS) term.f
layer.o: layer.f param.fi
	$(FC) $(FFLAGS) layer.f
lt.o: lt.f param.fi term.fi
	$(FC) $(FFLAGS) lt.f
sort_d.o: sort_d.f
	$(FC) $(FFLAGS) sort_d.f
search_l.o: search_l.f 
	$(FC) $(FFLAGS) search_l.f
velocity.o: velocity.f param.fi term.fi
	$(FC) $(FFLAGS) velocity.f
vl.o: vl.f param.fi term.fi
	$(FC) $(FFLAGS) vl.f

output.o: output.f param.fi pname.fi data_dir.fi error.fi
	$(FC) $(FFLAGS) output.f
sort_x.o: sort_x.f
	$(FC) $(FFLAGS) sort_x.f
create_dbfile.o: create_dbfile.f pname.fi data_dir.fi
	$(FC) $(FFLAGS) create_dbfile.f

sphere_step.o: sphere_step.f radius.fi list.fi term.fi pname.fi
	$(FC) $(FFLAGS) sphere_step.f
magni.o: magni.f param.fi
	$(FC) $(FFLAGS) magni.f
origin_time.o: origin_time.f param.fi
	$(FC) $(FFLAGS) origin_time.f

inpt_3d.o: inpt_3d.f param.fi pname.fi model_3d.fi data_dir.fi model_dir.fi term.fi source.fi
	$(FC) $(FFLAGS) inpt_3d.f
search_cr_model.o: search_cr_model.f error.fi
	$(FC) $(FFLAGS) search_cr_model.f
spline_in.o: spline_in.f term.fi
	$(FC) $(FFLAGS) spline_in.f

nearest_source.o: nearest_source.f source.fi pname.fi
	$(FC) $(FFLAGS) nearest_source.f
sort_source.o: sort_source.f source.fi
	$(FC) $(FFLAGS) sort_source.f

dialog_1.o: dialog_1.f list.fi pname.fi data_dir.fi term.fi
	$(FC) $(FFLAGS) dialog_1.f
dialog_2.o: dialog_2.f pname.fi term.fi
	$(FC) $(FFLAGS) dialog_2.f
dialog_2_1.o: dialog_2_1.f pname.fi term.fi
	$(FC) $(FFLAGS) dialog_2_1.f

dialog_3.o: dialog_3.f pname.fi param.fi term.fi
	$(FC) $(FFLAGS) dialog_3.f

rec_time_name.o: rec_time_name.f param.fi pname.fi term.fi
	$(FC) $(FFLAGS) rec_time_name.f

iter_1.o: iter_1.f param.fi pname.fi error.fi
	$(FC) $(FFLAGS) iter_1.f
iter_2.o: iter_2.f param.fi
	$(FC) $(FFLAGS) iter_2.f

cov_matrix.o: cov_matrix.f param.fi
	$(FC) $(FFLAGS) cov_matrix.f
rms_net_1.o: rms_net_1.f pname.fi list.fi data_dir.fi term.fi
	$(FC) $(FFLAGS) rms_net_1.f
rms_net_2.o: rms_net_2.f pname.fi list.fi term.fi
	$(FC) $(FFLAGS) rms_net_2.f
spline_value.o: spline_value.f term.fi
	$(FC) $(FFLAGS) spline_value.f

model.o: model.f
	$(FC) $(FFLAGS) model.f

eigen.o: eigen.f
	$(FC) $(FFLAGS) eigen.f

minv.o: minv.f
	$(FC) $(FFLAGS) minv.f

xy2fl.o: xy2fl.f
	$(FC) $(FFLAGS) xy2fl.f
