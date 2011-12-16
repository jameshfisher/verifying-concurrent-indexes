#ifndef NODES_H_
#define NODES_H_

typedef char Type;
// MSB> 0 0 0 0  0 E L C <LSB
//
// C: { 0: Black, 1: Red }
// L: color of left child
// E: { 0: no error, 1: error }
//
// Interpretation of E flag is dependent on context.

typedef struct N N;
typedef N * Np;
struct N {
  Type type;
  Np l, r;
  int v;
};

bool type_is_red(Type);
bool type_left_is_red(Type);
bool type_is_error(Type);

Type type_set_red(Type, bool);
Type type_set_left_red(Type, bool);
Type type_set_error(Type, bool);

bool is_red(Np);
bool left_is_red(Np);
bool is_error(Np);

void set_red(Np, Bool);
void set_left_red(Np, Bool);
void set_error(Np, Bool);

#endif
