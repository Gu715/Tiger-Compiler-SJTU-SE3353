#include "tiger/regalloc/regalloc.h"

#include "tiger/output/logger.h"
#include <iostream>

extern frame::RegManager *reg_manager;
extern std::map<std::string, std::pair<int, int>> frame_info_map;

namespace ra {
/* TODO: Put your lab6 code here */

RegAllocator::RegAllocator(std::string frame, std::unique_ptr<cg::AssemInstr>assem_instr) 
    : frame_(frame), assem_instr_(std::move(assem_instr)) {
}

void RegAllocator::RegAlloc() {
    LivenessAnalysis();
    Build();
    MakeWorklist();

    bool all_empty = false;
    do {
        if (!simplifyWorklist->GetList().empty()) {
            Simplify();
        } else if (!worklistMoves->GetList().empty()) {
            Coalesce();
        } else if (!freezeWorklist->GetList().empty()) {
            Freeze();
        } else if (!spillWorklist->GetList().empty()) {
            SelectSpill();
        } else {
            all_empty = true;
        }
    } while (!all_empty);

    AssignColors();

    if (!spilledNodes->GetList().empty()) {
        RewriteProgram();
        Reset();
        RegAlloc();
    }
}

void RegAllocator::LivenessAnalysis() {
    fg::FlowGraphFactory flow_graph_factory(assem_instr_->GetInstrList());
    flow_graph_factory.AssemFlowGraph();
    auto flow_graph = flow_graph_factory.GetFlowGraph();
    live_graph_factory_ = new live::LiveGraphFactory(flow_graph);
    live_graph_factory_->Liveness();
    auto live_graph_ = live_graph_factory_->GetLiveGraph();
    interf_graph_ = live_graph_.interf_graph;
    worklistMoves = live_graph_.moves;
}


void RegAllocator::Build() {
    precolored = new live::INodeList();
    initial = new live::INodeList();
    simplifyWorklist = new live::INodeList();
    freezeWorklist = new live::INodeList();
    spillWorklist = new live::INodeList();
    spilledNodes = new live::INodeList();
    coalescedNodes = new live::INodeList();
    coloredNodes = new live::INodeList();
    selectStack = new live::INodeList();
    
    coalescedMoves = new live::MoveList();
    constrainedMoves = new live::MoveList();
    frozenMoves = new live::MoveList();
    activeMoves = new live::MoveList();

    auto temp_map = reg_manager->temp_map_;
    auto node_list = interf_graph_->Nodes()->GetList();
    // 遍历所有节点，由于在活性分析中已经构建了冲突图，这里只要构建moveList
    for (auto n : node_list) {
        auto move_list_n = new live::MoveList();
        for (auto m : worklistMoves->GetList()) {
            if (m.first == n || m.second == n) {
                move_list_n->Append(m.first, m.second);
            }
        }
        moveList[n] = move_list_n;
        degree[n] = n->Degree();
        alias[n] = n;
        if (!temp_map->Look(n->NodeInfo())) {
            initial->Append(n);
        } else {
            precolored->Append(n);
            color[n] = n->NodeInfo();
        }
    }
}

void RegAllocator::AddEdge(live::INodePtr u, live::INodePtr v) {
    if (!u->Adj(v) && u != v) {
        if (!precolored->Contain(u)) {
            interf_graph_->AddEdge(u, v);
            degree[u]++;
        }
        if (!precolored->Contain(v)) {
            interf_graph_->AddEdge(v, u);
            degree[v]++;
        }
    }
}

void RegAllocator::MakeWorklist() {
    auto initial_list = initial->GetList();
    for (auto n : initial_list) {
        initial->DeleteNode(n);
        if (degree[n] >= K) {
            spillWorklist->Append(n);
        } else if (MoveRelated(n)) {
            freezeWorklist->Append(n);
        } else {
            simplifyWorklist->Append(n);
        }
    }
}

live::INodeListPtr RegAllocator::Adjacent(live::INodePtr n) {
    // adjList[n] \ (selectStack ∪ coalescedNodes)
    return n->Succ()->Diff(selectStack->Union(coalescedNodes));
}

live::MoveList *RegAllocator::NodeMoves(live::INodePtr n) {
    // moveList[n] ∩ (activeMoves ∪ worklistMoves)
    return moveList[n]->Intersect(activeMoves->Union(worklistMoves));
}

bool RegAllocator::MoveRelated(live::INodePtr n) {
    // NodeMoves(n) ≠ {}
    return !NodeMoves(n)->GetList().empty();
}

void RegAllocator::Simplify() {
    auto n = simplifyWorklist->GetList().front();
    simplifyWorklist->DeleteNode(n);
    selectStack->Prepend(n);
    auto adjacent_list = Adjacent(n)->GetList();
    for (auto m : adjacent_list) {
        DecrementDegree(m);
    }
}

void RegAllocator::DecrementDegree(live::INodePtr m) {
    int d = degree[m];
    degree[m] = d - 1;
    if (d == K) {
        auto adjacent = Adjacent(m);
        adjacent->Append(m);
        EnableMoves(adjacent);
        spillWorklist->DeleteNode(m);

        if (MoveRelated(m)) {
            freezeWorklist->Append(m);
        } else {
            simplifyWorklist->Append(m);
        }
    }
}

void RegAllocator::EnableMoves(live::INodeListPtr nodes) {
    auto node_list = nodes->GetList();
    for (auto n : node_list) {
        auto node_moves_list = NodeMoves(n)->GetList();
        for (auto m : node_moves_list) {
            if (activeMoves->Contain(m.first, m.second)) {
                activeMoves->Delete(m.first, m.second);
                worklistMoves->Append(m.first, m.second);
            }
        }
    }
}

void RegAllocator::Coalesce() {
    auto m = worklistMoves->GetList().front();
    auto x = m.first, y = m.second;
    x = GetAlias(x), y = GetAlias(y);
    auto u = x, v = y;
    if (precolored->Contain(y)) {
        u = y, v = x;
    }
    worklistMoves->Delete(m.first, m.second);
    if (u == v) {
        coalescedMoves->Append(m.first, m.second);
        AddWorkList(u);
    } else if (precolored->Contain(v) || u->Adj(v)) {
        constrainedMoves->Append(m.first, m.second);
        AddWorkList(u);
        AddWorkList(v);
    } else {
        bool cond = true;
        if (precolored->Contain(u)) {
            // u ∈ precolored and (forall t ∈ Adjacent(v), OK(t,u))
            auto adjacent_v_list = Adjacent(v)->GetList();
            for (auto t : adjacent_v_list) {
                cond = cond && OK(t, u);
            }
        } else {
            // u ∉ precolored and Conservative(Adjacent(u) ∪ Adjacent(v))
            cond = Conservative(Adjacent(u)->Union(Adjacent(v)));
        }

        if (cond) {
            coalescedMoves->Append(m.first, m.second);
            Combine(u, v);
            AddWorkList(u);
        } else {
            activeMoves->Append(m.first, m.second);
        }
    }
}

void RegAllocator::AddWorkList(live::INodePtr u) {
    // u ∉ precolored and not(MoveRelated(u)) and degree[u] < K
    if (!precolored->Contain(u) && !MoveRelated(u) && degree[u] < K) {
        freezeWorklist->DeleteNode(u);
        simplifyWorklist->Append(u);
    }
}

bool RegAllocator::OK(live::INodePtr t, live::INodePtr r) {
    // degree[t] < K or t ∈ precolored or (t,r) ∈ adjSet
    return degree[t] < K || precolored->Contain(t) || t->Adj(r);
}

bool RegAllocator::Conservative(live::INodeListPtr nodes) {
    int k = 0;
    auto node_list = nodes->GetList();
    for (auto n : node_list) {
        if (degree[n] >= K) {
            k++;
        }
    }
    return k < K;
}

live::INodePtr RegAllocator::GetAlias(live::INodePtr n) {
    if (coalescedNodes->Contain(n)) {
        return GetAlias(alias[n]);
    } else {
        return n;
    }
}

void RegAllocator::Combine(live::INodePtr u, live::INodePtr v) {
    if (freezeWorklist->Contain(v)) {
        freezeWorklist->DeleteNode(v);
    } else {
        spillWorklist->DeleteNode(v);
    }
    coalescedNodes->Append(v);
    alias[v] = u;
    moveList[u] = moveList[u]->Union(moveList[v]);
    auto nodes = new live::INodeList();
    nodes->Append(v);
    EnableMoves(nodes);
    auto adjacent_v_list = Adjacent(v)->GetList();
    for (auto t : adjacent_v_list) {
        AddEdge(t, u);
        DecrementDegree(t);
    }
    if (degree[u] >= K && freezeWorklist->Contain(u)) {
        freezeWorklist->DeleteNode(u);
        spillWorklist->Append(u);
    }
}

void RegAllocator::Freeze() {
    auto u = freezeWorklist->GetList().front();
    freezeWorklist->DeleteNode(u);
    simplifyWorklist->Append(u);
    FreezeMoves(u);
}

void RegAllocator::FreezeMoves(live::INodePtr u) {
    auto node_moves_u_list = NodeMoves(u)->GetList();
    for (auto m : node_moves_u_list) {
        auto x = m.first, y = m.second;
        auto v = GetAlias(y);
        if (GetAlias(y) == GetAlias(u)) {
            v = GetAlias(x);
        }
        activeMoves->Delete(m.first, m.second);
        frozenMoves->Append(m.first, m.second);
        if (node_moves_u_list.empty() && degree[v] < K) {
            freezeWorklist->DeleteNode(v);
            simplifyWorklist->Append(v);
        }
    }
}

void RegAllocator::SelectSpill() {
    auto m = spillWorklist->GetList().front();
    for (auto t : spillWorklist->GetList()) {
        if (t->Degree() > m->Degree()) {
            m = t;
        }
    }
    spillWorklist->DeleteNode(m);
    simplifyWorklist->Append(m);
    FreezeMoves(m);
}

void RegAllocator::AssignColors() {
    while (!selectStack->GetList().empty()) {
        auto n = selectStack->GetList().front();
        selectStack->DeleteNode(n);
        auto okColors = reg_manager->Registers();
        auto adj_list_n = n->Succ()->GetList();
        for (auto w : adj_list_n) {
            if (coloredNodes->Union(precolored)->Contain(GetAlias(w))) {
                okColors->Delete(color[GetAlias(w)]);
            }
        }
        if (okColors->GetList().empty()) {
            spilledNodes->Append(n);
        } else {
            coloredNodes->Append(n);
            auto c = okColors->GetList().front();
            color[n] = c;
        }
    }
    for (auto n : coalescedNodes->GetList()) {
        color[n] = color[GetAlias(n)];
    }
}

void RegAllocator::RewriteProgram() {
    auto frame_info = frame_info_map[frame_];
    auto offset = frame_info.first, frame_size = frame_info.second;  // 只能拿到offset和frame_size，无法访问frame
    std::map<temp::Temp *, int> spillMap;  // 保存每个溢出节点的栈上offset信息

    // 先为每一个溢出节点分配栈上空间
    for (auto spill_node : spilledNodes->GetList()) {
        if (spillMap.find(spill_node->NodeInfo()) == spillMap.end()) {
            offset -= reg_manager->WordSize();
            frame_size += reg_manager->WordSize();
            spillMap[spill_node->NodeInfo()] = offset;
        }
    }

    // 然后重写程序，对于每一个对溢出节点的读，先从栈上取出；对溢出节点的写，最后写到栈上
    auto instr_list = assem_instr_->GetInstrList()->GetList();
    auto new_instr_li = new assem::InstrList();
    for (auto instr_it = instr_list.begin(); instr_it != instr_list.end(); instr_it++) {
        auto instr = *instr_it;
        temp::TempList *src = nullptr, *dst = nullptr;
        if (auto move_instr = dynamic_cast<assem::MoveInstr *>(instr)) {
            src = move_instr->src_;
            dst = move_instr->dst_;
        } else if (auto oper_instr = dynamic_cast<assem::OperInstr *>(instr)) {
            src = oper_instr->src_;
            dst = oper_instr->dst_;
        }
        auto last_src_it = std::prev(new_instr_li->GetList().end());

        if (src) {
            temp::TempList *new_src = new temp::TempList;
            for (auto tmp : src->GetList()) {
                if (spillMap.find(tmp) != spillMap.end()) {  // 说明这个tmp是一个被溢出的节点
                    int tmp_offset = spillMap[tmp];
                    temp::Temp *new_tmp = temp::TempFactory::NewTemp();
                    new_instr_li->Append(
                        new assem::OperInstr(
                            "movq " + frame_ + "_framesize_local" + std::to_string(tmp_offset) + "(`s0),`d0",
                            new temp::TempList(new_tmp), 
                            new temp::TempList(reg_manager->GetRegister(frame::X64RegManager::RSP)),
                            nullptr
                        ));
                    last_src_it = std::prev(new_instr_li->GetList().end());
                    new_src->Append(new_tmp);
                } else {
                    new_src->Append(tmp);
                }
            }
            src = new_src;
        }

        if (dst) {
            temp::TempList *new_dst = new temp::TempList;
            for (auto tmp : dst->GetList()) {
                if (spillMap.find(tmp) != spillMap.end()) {  // 说明这个tmp是一个被溢出的节点
                    int tmp_offset = spillMap[tmp];
                    temp::Temp *new_tmp = temp::TempFactory::NewTemp();
                    new_instr_li->Append(
                        new assem::OperInstr(
                            "movq `s0," + frame_ + "_framesize_local" + std::to_string(tmp_offset) + "(`s1)",
                            new temp::TempList(),
                            new temp::TempList({new_tmp, reg_manager->GetRegister(frame::X64RegManager::RSP)}), 
                            nullptr
                        ));
                    new_dst->Append(new_tmp);
                } else {
                    new_dst->Append(tmp);
                }
            }
            dst = new_dst;
        }

        if (auto move_instr = dynamic_cast<assem::MoveInstr *>(instr)) {
            new_instr_li->Insert(std::next(last_src_it), new assem::MoveInstr(move_instr->assem_, dst, src));
        } else if (auto oper_instr = dynamic_cast<assem::OperInstr *>(instr)) {
            new_instr_li->Insert(std::next(last_src_it), new assem::OperInstr(oper_instr->assem_, dst, src, oper_instr->jumps_));
        } else if (auto label_instr = dynamic_cast<assem::LabelInstr *>(instr)) {
            new_instr_li->Insert(std::next(last_src_it), new assem::LabelInstr(label_instr->assem_));
        }
    }

    // 记得更新栈信息
    assem_instr_.reset(new cg::AssemInstr(new_instr_li));
    frame_info_map[frame_] = {offset, frame_size};
}

void RegAllocator::Reset() {
    delete live_graph_factory_;

    delete precolored;
    delete initial;
    delete simplifyWorklist;
    delete freezeWorklist;
    delete spillWorklist;
    delete spilledNodes;
    delete coalescedNodes;
    delete coloredNodes;
    delete selectStack;
    
    delete coalescedMoves;
    delete constrainedMoves;
    delete frozenMoves;
    delete activeMoves;

    degree.clear();
    moveList.clear();
    alias.clear();
    color.clear();
}



std::unique_ptr<ra::Result> RegAllocator::TransferResult() {
    auto reg_map = temp::Map::Empty();
    for (auto node : interf_graph_->Nodes()->GetList()) {
        reg_map->Enter(node->NodeInfo(), reg_manager->temp_map_->Look(color[node]));
    }

    // delete move instruction whose src and dst are the same
    auto new_instr_list = new assem::InstrList();
    for (auto instr : assem_instr_->GetInstrList()->GetList()){
        if (auto move_instr = dynamic_cast<assem::MoveInstr *>(instr)) {
            if (move_instr->src_ && move_instr->dst_) {
                if (move_instr->src_->GetList().size() == 1 && move_instr->dst_->GetList().size() == 1) {
                    if (reg_map->Look(move_instr->src_->GetList().front()) ==
                        reg_map->Look(move_instr->dst_->GetList().front())) {
                        continue;
                    }
                }
            }
        }
        new_instr_list->Append(instr);
    }

    return std::make_unique<ra::Result>(reg_map, new_instr_list);
}

} // namespace ra