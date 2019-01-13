
FC=gfortran

FFLAGS=-g -O0 -Wall -Wtabs -c -ffixed-line-length-170 -fd-lines-as-comments
LDFLAGS=

OBJ= trans.o    \
 datum.o days.o real8_to_int.o  \
 td_all.o rt_3d.o term.o layer.o lt.o sort_d.o search_l.o velocity.o vl.o   \
 o_hy3.o sort_x.o create_dbfile.o   \
 sphere_step.o magni.o origin_time.o   \
 i_hyp_mod.o read_model.o spline_in.o   \
 dialog_1.o dialog_2.o dialog_2_1.o   \
 dialog_3.o rec_time_name.o \
 iter_1.o iter_2.o cov_matrix.o rms_net_1.o rms_net_2.o spline_value.o   \
 eigen.o minv.o \
 surfb.o xy2fl.o

hypo3d: hypo.o $(OBJ)
	$(FC)  $(LDFLAGS) hypo.o $(OBJ)  -o hypo3d

clean:
	rm *.o hypo3d

hypo.o: hypo.f param.fi pname.fi list.fi
	$(FC) $(FFLAGS) hypo.f

trans.o: trans.f pname.fi
	$(FC) $(FFLAGS) trans.f

datum.o: datum.f 
	$(FC) $(FFLAGS) datum.f
days.o: days.f
	$(FC) $(FFLAGS) days.f
real8_to_int.o: real8_to_int.f
	$(FC) $(FFLAGS) real8_to_int.f

td_all.o: td_all.f param.fi list.fi
	$(FC) $(FFLAGS) td_all.f
rt_3d.o: rt_3d.f param.fi list.fi 
	$(FC) $(FFLAGS) rt_3d.f
term.o: term.f param.fi
	$(FC) $(FFLAGS) term.f
layer.o: layer.f param.fi
	$(FC) $(FFLAGS) layer.f
lt.o: lt.f param.fi 
	$(FC) $(FFLAGS) lt.f
sort_d.o: sort_d.f
	$(FC) $(FFLAGS) sort_d.f
search_l.o: search_l.f 
	$(FC) $(FFLAGS) search_l.f
velocity.o: velocity.f param.fi 
	$(FC) $(FFLAGS) velocity.f
vl.o: vl.f param.fi 
	$(FC) $(FFLAGS) vl.f

o_hy3.o: o_hy3.f param.fi pname.fi
	$(FC) $(FFLAGS) o_hy3.f
sort_x.o: sort_x.f
	$(FC) $(FFLAGS) sort_x.f
create_dbfile.o: create_dbfile.f pname.fi
	$(FC) $(FFLAGS) create_dbfile.f

sphere_step.o: sphere_step.f list.fi  pname.fi
	$(FC) $(FFLAGS) sphere_step.f
magni.o: magni.f param.fi
	$(FC) $(FFLAGS) magni.f
origin_time.o: origin_time.f param.fi
	$(FC) $(FFLAGS) origin_time.f

i_hyp_mod.o: i_hyp_mod.f param.fi pname.fi
	$(FC) $(FFLAGS) i_hyp_mod.f
read_model.o: read_model.f
	$(FC) $(FFLAGS) read_model.f
spline_in.o: spline_in.f 
	$(FC) $(FFLAGS) spline_in.f

dialog_1.o: dialog_1.f list.fi pname.fi 
	$(FC) $(FFLAGS) dialog_1.f
dialog_2.o: dialog_2.f pname.fi 
	$(FC) $(FFLAGS) dialog_2.f
dialog_2_1.o: dialog_2_1.f pname.fi 
	$(FC) $(FFLAGS) dialog_2_1.f

dialog_3.o: dialog_3.f pname.fi param.fi 
	$(FC) $(FFLAGS) dialog_3.f

rec_time_name.o: rec_time_name.f param.fi pname.fi 
	$(FC) $(FFLAGS) rec_time_name.f

iter_1.o: iter_1.f param.fi pname.fi
	$(FC) $(FFLAGS) iter_1.f
iter_2.o: iter_2.f param.fi
	$(FC) $(FFLAGS) iter_2.f

cov_matrix.o: cov_matrix.f param.fi
	$(FC) $(FFLAGS) cov_matrix.f
rms_net_1.o: rms_net_1.f pname.fi list.fi 
	$(FC) $(FFLAGS) rms_net_1.f
rms_net_2.o: rms_net_2.f pname.fi list.fi 
	$(FC) $(FFLAGS) rms_net_2.f
spline_value.o: spline_value.f 
	$(FC) $(FFLAGS) spline_value.f

surfb.o: surfb.f
	$(FC) $(FFLAGS) surfb.f

eigen.o: eigen.f
	$(FC) $(FFLAGS) eigen.f

minv.o: minv.f
	$(FC) $(FFLAGS) minv.f

xy2fl.o: xy2fl.f
	$(FC) $(FFLAGS) xy2fl.f
