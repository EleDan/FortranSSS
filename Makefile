# Makefile per shamir.f90

shamir = shamir.exe
modules = functions.o types.o arithmetics.o
cleanVariable = del /f # Windows
# cleanVariable = rm -f # Linux
wall =  # -Wall

$(shamir): shamir.f90 functions.o functions.mod types.o types.mod arithmetics.mod arithmetics.o
	gfortran $(wall) shamir.f90 $(modules) -o $@

functions.mod: functions.o

functions.o: functions.f90 types.mod arithmetics.mod
	gfortran $(wall) -c functions.f90

types.mod: types.o

types.o: types.f90 arithmetics.mod
	gfortran $(wall) -c types.f90

arithmetics.mod: arithmetics.o

arithmetics.o: arithmetics.f90
	gfortran $(wall) -c arithmetics.f90






clean:
	$(cleanVariable) $(wildcard *.exe) $(wildcard *.mod) $(wildcard *.o) $(wildcard *.txt)