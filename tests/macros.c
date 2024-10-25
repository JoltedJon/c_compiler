#ifdef HELLO
#error ifdef not currently working
#endif


#define HELLO

#ifndef HELLO
#error ifndef not currently working
#endif

#undef HELLO

#ifdef HELLO
#error undef currently not working
#endif