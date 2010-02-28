// despite the '.c' extensions, this file is just documentaion.  It
// explores the different namespaces of c and how various issues are
// handled.

// TODO This is not handled the same in tcc.  Read the c standard and
// see what is actuall the correct behavior.

// Namespaces:
// 1. Tagged types:
//
//    'struct c', 'enum c', and 'union c' are all in the same
//    namespace, but not the same namespaces as straight-up types like
//    'int'.
//
//    These are lexically scoped, there is a global scope, and every
//    block introduces a new scope.
//
// 2. Labels
//
//    Lables use the functions scope.  There is no scope nesting of
//    labels.  There are no global lables, and
//    functions-definined-in-functions have their own,
//    completely-separate labels scope.
//
// 3. Everything else:
//
//    - Types
//    - Variables
//    - Functions
//
//    These are lexically scoped, there is a global scope, and every
//    block introduces a new scope.
//
// 4. Every Struct/union has it's own namespace within it.
//
// Aside from that

struct f { int a; };

typedef int c;

c see() {
  c c;
  switch ('a') {
  case 'a': break;
  default: return; }
 c:
  return c + c; }

c main(void) {
  c cee();
 c:
  return cee(); }

c cee() {
  c C;
  typedef struct c { c c; } c;
  // int c; // This conflicts with the typedef above
  // typedef union c { c c; } see; // This conflicts with 'struct c'
 c:
  { c c;
    //c d; // Not cool.  Using c as a type was fine above but not here
           // since the variable c hides it.
    if (!c.c) return 0;
    typedef struct c { int c; } C;
    // c: // Not cool.  Label scopes don't nest
    C sea;
    struct c see() {
    c: // Nested functions have their own label scopes (although they
       // are not standard c).  However, you can't goto labels in the
       // enclosing lexical scope.
      goto c;
      sea; }}
  c sea;
  return C;
  return see(sea.c); }
