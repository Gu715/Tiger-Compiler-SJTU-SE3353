#include "tiger/liveness/flowgraph.h"
#include <iostream>
namespace fg {

void FlowGraphFactory::AssemFlowGraph() {
  /* TODO: Put your lab6 code here */
  std::map<assem::Instr *, FNodePtr> instr_to_node;
  auto instr_list = instr_list_->GetList();

  for (auto instr : instr_list) {
    auto node = flowgraph_->NewNode(instr);
    instr_to_node[instr] = node;

    if (auto label_instr = dynamic_cast<assem::LabelInstr *>(instr)) {
      label_map_->emplace(label_instr->label_->Name(), node);
    }
  }

  for (auto it = instr_list.begin(); it != instr_list.end(); it++) {
    auto current_instr = *it;
    auto current_node = instr_to_node[current_instr];

    // see if this is a jmp instruction
    if (auto oper_instr = dynamic_cast<assem::OperInstr *>(current_instr)) {
      if (oper_instr->jumps_ && oper_instr->jumps_->labels_) {
        auto target_label = (*oper_instr->jumps_->labels_)[0];  // assume only one target label in an instruction
        auto target_node = label_map_->at(target_label->Name());
        flowgraph_->AddEdge(current_node, target_node);

        if (oper_instr->assem_.find("jmp") != std::string::npos) {  // won't execute next instruction
          continue;
        }
      }
    }

    // add an edge to next instruction
    auto next_it = std::next(it);
    if (next_it != instr_list.end()) {
      auto next_instr = *next_it;
      auto next_node = instr_to_node[next_instr];
      flowgraph_->AddEdge(current_node, next_node);
    }
  }
}

} // namespace fg

namespace assem {

temp::TempList *LabelInstr::Def() const { return new temp::TempList(); }

temp::TempList *MoveInstr::Def() const { return dst_; }

temp::TempList *OperInstr::Def() const { return dst_; }

temp::TempList *LabelInstr::Use() const { return new temp::TempList(); }

temp::TempList *MoveInstr::Use() const {
  return (src_) ? src_ : new temp::TempList();
}

temp::TempList *OperInstr::Use() const {
  return (src_) ? src_ : new temp::TempList();
}
} // namespace assem
