#include "tiger/escape/escape.h"
#include "tiger/absyn/absyn.h"

namespace esc {
void EscFinder::FindEscape() { absyn_tree_->Traverse(env_.get()); }
} // namespace esc

namespace absyn {

void AbsynTree::Traverse(esc::EscEnvPtr env) {
  /* TODO: Put your lab5-part1 code here */
  this->root_->Traverse(env, 1);
}

void SimpleVar::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  esc::EscapeEntry *entry = env->Look(this->sym_);
  if (depth > entry->depth_)
    *(entry->escape_) = true;
}

void FieldVar::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->var_->Traverse(env, depth);
}

void SubscriptVar::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->var_->Traverse(env, depth);
  this->subscript_->Traverse(env, depth);
}

void VarExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->var_->Traverse(env, depth);
}

void NilExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  return;
}

void IntExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  return;
}

void StringExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  return;
}

void CallExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  const std::list<absyn::Exp *> &exp_list = this->args_->GetList();
  for (const auto &exp : exp_list) {
    exp->Traverse(env, depth);
  }
}

void OpExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->left_->Traverse(env, depth);
  this->right_->Traverse(env, depth);
}

void RecordExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  const std::list<absyn::EField *> &efield_list = this->fields_->GetList();
  for (const auto &efield : efield_list) {
    efield->exp_->Traverse(env, depth);
  }
}

void SeqExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  const std::list<absyn::Exp *> &exp_list = this->seq_->GetList();
  for (const auto &exp : exp_list) {
    exp->Traverse(env, depth);
  }
}

void AssignExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->var_->Traverse(env, depth);
  this->exp_->Traverse(env, depth);
}

void IfExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->test_->Traverse(env, depth);
  this->then_->Traverse(env, depth);
  if (this->elsee_)
    this->elsee_->Traverse(env, depth);
}

void WhileExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->test_->Traverse(env, depth);
  this->body_->Traverse(env, depth);
}

void ForExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  env->BeginScope();
  this->escape_ = false;
  esc::EscapeEntry *entry = new esc::EscapeEntry(depth, &this->escape_);
  env->Enter(this->var_, entry);
  this->lo_->Traverse(env, depth);
  this->hi_->Traverse(env, depth);
  this->body_->Traverse(env, depth);
  env->EndScope();
}

void BreakExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  return;
}

void LetExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  env->BeginScope();
  std::list<absyn::Dec *> dec_list = this->decs_->GetList();
  for (const auto &dec : dec_list) {
    dec->Traverse(env, depth);
  }
  this->body_->Traverse(env, depth);
  env->EndScope();
}

void ArrayExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->size_->Traverse(env, depth);
  this->init_->Traverse(env, depth);
}

void VoidExp::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  return;
}

void FunctionDec::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  const std::list<absyn::FunDec *> &fun_dec_list = this->functions_->GetList();
  for (const auto &fun_dec : fun_dec_list) {
    env->BeginScope();
    const std::list<absyn::Field *> field_list = fun_dec->params_->GetList();
    for (const auto &field : field_list) {
      field->escape_ = false;
      esc::EscapeEntry *entry = new esc::EscapeEntry(depth + 1, &field->escape_);
      env->Enter(field->name_, entry);
    }
    fun_dec->body_->Traverse(env, depth + 1);
    env->EndScope();
  }
}

void VarDec::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  this->escape_ = false;
  esc::EscapeEntry *entry = new esc::EscapeEntry(depth, &this->escape_);
  env->Enter(this->var_, entry);
  this->init_->Traverse(env, depth);
}

void TypeDec::Traverse(esc::EscEnvPtr env, int depth) {
  /* TODO: Put your lab5-part1 code here */
  return;
}

} // namespace absyn
