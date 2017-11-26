#define A 10
#define B 20
#define C B > A
#define D A > B
#define X > A

#if C
hello
#endif

#if B X
partial expansion
#endif

#if D
world
#endif
