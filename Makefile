

.PHONY: all
all: functions.o weekpack.o driver_cpp.o example_cpp.exe


.PHONY: all_gfor
all_gfor: functions.o weekpack.o driver_gfor.o example_gfor.exe

example_gfor.exe: driver_gfor.o functions.o weekpack.o
	gfortran -O3 -Wall driver_gfor.o functions.o weekpack.o -o example_gfor.exe

example_cpp.exe: driver_cpp.o functions.o weekpack.o
	g++ -O3 -Wall driver_cpp.o weekpack.o functions.o -o example_cpp.exe -lgfortran

driver_cpp.o: driver.cpp
	g++ -c driver.cpp  -o driver_cpp.o

driver_gfor.o: driver.f
	gfortran -c driver.f  -o driver_gfor.o


functions.o: functions.f
	gfortran -c functions.f

weekpack.o: weekpack.f
	gfortran -c weekpack.f


overflow.exe:
	gpp -Wall overflow.c -o overflow.exe

.PHONY: clean
clean:
	rm -rf *.o

.PHONY: clean_exe
clean_exe:
	rm -rf *.exe

