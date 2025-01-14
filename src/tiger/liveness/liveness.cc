#include "tiger/liveness/liveness.h"

extern frame::RegManager *reg_manager;

namespace live {

bool MoveList::Contain(INodePtr src, INodePtr dst) {
  return std::any_of(move_list_.cbegin(), move_list_.cend(),
                     [src, dst](std::pair<INodePtr, INodePtr> move) {
                       return move.first == src && move.second == dst;
                     });
}

void MoveList::Delete(INodePtr src, INodePtr dst) {
  assert(src && dst);
  auto move_it = move_list_.begin();
  for (; move_it != move_list_.end(); move_it++) {
    if (move_it->first == src && move_it->second == dst) {
      break;
    }
  }
  move_list_.erase(move_it);
}

MoveList *MoveList::Union(MoveList *list) {
  auto *res = new MoveList();
  for (auto move : move_list_) {
    res->move_list_.push_back(move);
  }
  for (auto move : list->GetList()) {
    if (!res->Contain(move.first, move.second))
      res->move_list_.push_back(move);
  }
  return res;
}

MoveList *MoveList::Intersect(MoveList *list) {
  auto *res = new MoveList();
  for (auto move : list->GetList()) {
    if (Contain(move.first, move.second))
      res->move_list_.push_back(move);
  }
  return res;
}

void LiveGraphFactory::LiveMap() {
  /* TODO: Put your lab6 code here */
  auto node_list = flowgraph_->Nodes()->GetList();
  for (auto node : node_list) {
    in_->Enter(node, new temp::TempList());
    out_->Enter(node, new temp::TempList());
  }

  while (1) {
    bool all_same = true;
    for (auto node : node_list) {
      auto old_in = in_->Look(node);
      auto old_out = out_->Look(node);

      // in = use ∪ (out - def)
      auto out = old_out;
      auto def = node->NodeInfo()->Def();
      auto use = node->NodeInfo()->Use();
      auto new_in = use->Union(out->Diff(def));

      // out = ∪succ in
      auto succ_list = node->Succ()->GetList();
      auto new_out = new temp::TempList();
      for (auto succ_node : succ_list) {
        auto succ_in = in_->Look(succ_node);
        new_out = new_out->Union(succ_in);
      }

      if (!old_in->Equal(new_in) || !old_out->Equal(new_out)) {
        all_same = false;
      }

      in_->Set(node, new_in);
      out_->Set(node, new_out);
      delete old_in;
      delete old_out;
    }

    if (all_same) {
      break;
    }
  }
}

void LiveGraphFactory::InterfGraph() {
  /* TODO: Put your lab6 code here */
  // set node for all registers
  // note that we don't process rsp
  auto register_list = reg_manager->Registers()->GetList();
  for (auto reg : register_list) {
    temp_node_map_->Enter(reg, live_graph_.interf_graph->NewNode(reg));
  }

  // all registers conflict with each other
  for (auto reg1 : register_list) {
    for (auto reg2 : register_list) {
      if (reg1 != reg2) {
        auto node1 = temp_node_map_->Look(reg1);
        auto node2 = temp_node_map_->Look(reg2);
        live_graph_.interf_graph->AddEdge(node1, node2);
      }
    }
  }

  // set node for all temp registers
  auto node_list = flowgraph_->Nodes()->GetList();
  for (auto node : node_list) {
    auto def_list = node->NodeInfo()->Def()->GetList();
    for (auto tmp : def_list) {
      if (tmp != reg_manager->GetRegister(frame::X64RegManager::Reg::RSP) && 
          !temp_node_map_->Look(tmp)) {
        temp_node_map_->Enter(tmp, live_graph_.interf_graph->NewNode(tmp));
      }
    }

    auto use_list = node->NodeInfo()->Use()->GetList();
    for (auto tmp : use_list) {
      if (tmp != reg_manager->GetRegister(frame::X64RegManager::Reg::RSP) && 
          !temp_node_map_->Look(tmp)) {
        temp_node_map_->Enter(tmp, live_graph_.interf_graph->NewNode(tmp));
      }
    }
  }
  
  // add conflict edge
  for (auto node : node_list) {
    auto instr = node->NodeInfo();
    // for all registers in one IN / OUT, they conflict with each other
    auto in_list = in_->Look(node)->GetList();
    for (auto tmp1 : in_list) {
      for (auto tmp2 : in_list) {
        if (tmp1 != reg_manager->GetRegister(frame::X64RegManager::Reg::RSP) && 
            tmp2 != reg_manager->GetRegister(frame::X64RegManager::Reg::RSP) && 
            tmp1 != tmp2) {
          assert(tmp1 && tmp2);
          auto node1 = temp_node_map_->Look(tmp1);
          auto node2 = temp_node_map_->Look(tmp2);
          assert(node1 && node2);
          live_graph_.interf_graph->AddEdge(node1, node2);
        }
      }
    }

    auto out_list = out_->Look(node)->GetList();
    for (auto tmp1 : out_list) {
      for (auto tmp2 : out_list) {
        if (tmp1 != reg_manager->GetRegister(frame::X64RegManager::Reg::RSP) && 
            tmp2 != reg_manager->GetRegister(frame::X64RegManager::Reg::RSP) && 
            tmp1 != tmp2) {
          assert(tmp1 && tmp2);
          auto node1 = temp_node_map_->Look(tmp1);
          auto node2 = temp_node_map_->Look(tmp2);
          assert(node1 && node2);
          live_graph_.interf_graph->AddEdge(node1, node2);
        }
      }
    }

    if (auto move_instr = dynamic_cast<assem::MoveInstr *>(instr)) {
      // DEF conflict with OUT - USE
      auto def_list = instr->Def()->GetList();
      auto use = instr->Use();
      auto out = out_->Look(node);
      auto out_diff_use = out->Diff(use);
      auto out_diff_use_list = out_diff_use->GetList();
      for (auto tmp1 : def_list) {
        if (tmp1 == reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)) {
          continue;
        }
        for (auto tmp2: out_diff_use_list) {
          if (tmp2 == reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)) {
            continue;
          }
          assert(tmp1 && tmp2);
          auto node1 = temp_node_map_->Look(tmp1);
          auto node2 = temp_node_map_->Look(tmp2);
          assert(node1 && node2);
          live_graph_.interf_graph->AddEdge(node1, node2);
          live_graph_.interf_graph->AddEdge(node2, node1);
        }
      }
      delete out_diff_use;

      // DEF move conflict with USE
      auto use_list = use->GetList();
      for (auto tmp1 : def_list) {
        if (tmp1 == reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)) {
          continue;
        }
        for (auto tmp2 : use_list) {
          if (tmp2 == reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)) {
            continue;
          }
          assert(tmp1 && tmp2);
          auto node1 = temp_node_map_->Look(tmp1);
          auto node2 = temp_node_map_->Look(tmp2);
          assert(node1 && node2);
          live_graph_.moves->Append(node2, node1);
        }
      }
    } else {
      // DEF conflict with OUT
      auto def_list = instr->Def()->GetList();
      for (auto tmp1 : def_list) {
        if (tmp1 == reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)) {
          continue;
        }
        for (auto tmp2: out_list) {
          if (tmp2 == reg_manager->GetRegister(frame::X64RegManager::Reg::RSP)) {
            continue;
          }
          assert(tmp1 && tmp2);
          auto node1 = temp_node_map_->Look(tmp1);
          auto node2 = temp_node_map_->Look(tmp2);
          assert(node1 && node2);
          live_graph_.interf_graph->AddEdge(node1, node2);
          live_graph_.interf_graph->AddEdge(node2, node1);
        }
      }
    }
  }
}

void LiveGraphFactory::Liveness() {
  LiveMap();
  InterfGraph();
}

} // namespace live