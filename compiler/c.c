// This file is for documentaion only.  It explores the different
// namespaces in c and how various issues are resolved.

// Namespaces:
// 1. Tagged types
//  - 'struct c', 'enum c', and 'union c' are all in the same
//    namespace, but not the same namespaces as straight-up types like
//    'int'.
//
// 2. Labels
// 3. Everything else,
//  - Types
//  - Variables
//  - Functions
//
// 4. Every Struct/union has it's own namespace

typedef int c;

c see() { c c; return c + c; }
c main(void) {
  c cee();
  return cee(); }

c cee() {
  typedef struct c { c c; } c;
  // typedef union c { c c; } see; // This conflicts with 'struct c'
  c sea;
  return see(sea.c); }
