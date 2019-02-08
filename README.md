# Servos and Utilities
## Introduction

The scope of this project is to provide a series of open-source open-access libraries with models of simple servos.

The libraries are initially designed to be used with the aeroservoelastic code HAWC2 but can be generalized for a broader use.

The libraries so far included are:
- pitch actuator
- generator 
- mechanical brake
- flap actuator
- general filters

## Documentation

The documentation will come as soon as possible.

## Compiling on Windows (as a DLL)

Use the Visual Studio project included in this repository.

## Compiling on Unix (as a shared object)

In case that another compiler rather than the default one wants to be used, edit the file /src/config.mk by modifying the line below:

```>> FC=mpif90```

With a suitable alternative (e.g. ifort). Once the compiler is defined, the different shared objects can be 
compiled just by typing in the /src folder the following command through a Unix terminal:

```>> make```

## Dependencies

The project depends on the [Basic DTU Wind Energy controller](https://github.com/DTUWindEnergy/BasicDTUController) because a file is shared between the projects. Both the Visual Studio projects and the Unix Makefile do support these dependencies, at the condition that the ServoAndUtilities and the BasicDTUController repositories are located in the same folder. In addition, the UNIX makefile assume that, before its execution, the BasicDTUController has been already compiled with a compatible compiler. 

## License

Servos and Utilities are distributed under the [GNU General Public License v3.0](http://en.wikipedia.org/wiki/GNU_General_Public_License).
 

