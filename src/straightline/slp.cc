#include "straightline/slp.h"

#include <iostream>

namespace A {
int A::CompoundStm::MaxArgs() const {
  // TODO: put your code here (lab1).
  return std::max(stm1->MaxArgs(), stm2->MaxArgs());
}

Table *A::CompoundStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
  return stm2->Interp(stm1->Interp(t));
}

int A::AssignStm::MaxArgs() const {
  // TODO: put your code here (lab1).
  return exp->MaxArgs();
}

Table *A::AssignStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
  IntAndTable *res = exp->Interp(t);
  Table *tmp = res->t;
  return tmp->Update(id, res->i);
}

int A::PrintStm::MaxArgs() const {
  // TODO: put your code here (lab1).
  return std::max(exps->MaxArgs(), exps->NumExps());
}

Table *A::PrintStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
  IntAndTable *res = exps->Interp(t);
  return res->t;
}


int Table::Lookup(const std::string &key) const {
  if (id == key) {
    return value;
  } else if (tail != nullptr) {
    return tail->Lookup(key);
  } else {
    assert(false);
  }
}

Table *Table::Update(const std::string &key, int val) const {
  return new Table(key, val, this);
}


int A::IdExp::MaxArgs() const {
  return 0;
}

IntAndTable *A::IdExp::Interp(Table *t) const {
  return new IntAndTable(t->Lookup(id), t);
}

int A::NumExp::MaxArgs() const {
  return 0;
}

IntAndTable *A::NumExp::Interp(Table *t) const {
  return new IntAndTable(num, t);
}

int A::OpExp::MaxArgs() const {
  return std::max(left->MaxArgs(), right->MaxArgs());
}

IntAndTable *A::OpExp::Interp(Table *t) const {
  IntAndTable *a = left->Interp(t);
  IntAndTable *b = right->Interp(a->t);

  int num;
  switch (oper) {
  case PLUS: num = a->i + b->i; break;
  case MINUS: num = a->i - b->i; break;
  case TIMES: num = a->i * b->i; break;
  case DIV: num = a->i / b->i; break;
  default: assert(false);
  }

  return new IntAndTable(num, b->t);
}

int A::EseqExp::MaxArgs() const {
  return stm->MaxArgs();
}

IntAndTable *A::EseqExp::Interp(Table *t) const {
  Table *tmp = stm->Interp(t);
  return exp->Interp(tmp);
}


int A::PairExpList::MaxArgs() const {
  return std::max(exp->MaxArgs(), tail->MaxArgs());
}

int A::PairExpList::NumExps() const {
  return 1 + tail->NumExps();
}

IntAndTable *A::PairExpList::Interp(Table *t) const {
  IntAndTable *res = exp->Interp(t);
  std::cout << res->i << " ";
  return tail->Interp(res->t);
}

int A::LastExpList::MaxArgs() const {
  return exp->MaxArgs();
}

int A::LastExpList::NumExps() const {
  return 1;
}

IntAndTable *A::LastExpList::Interp(Table *t) const {
  IntAndTable *res = exp->Interp(t);
  std::cout << res->i << std::endl;
  return res;
}

}  // namespace A


