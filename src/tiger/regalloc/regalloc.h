#ifndef TIGER_REGALLOC_REGALLOC_H_
#define TIGER_REGALLOC_REGALLOC_H_

#include "tiger/codegen/assem.h"
#include "tiger/codegen/codegen.h"
#include "tiger/frame/frame.h"
#include "tiger/frame/temp.h"
#include "tiger/liveness/liveness.h"
#include "tiger/regalloc/color.h"
#include "tiger/util/graph.h"

namespace ra {

class Result {
public:
  temp::Map *coloring_;
  assem::InstrList *il_;

  Result() : coloring_(nullptr), il_(nullptr) {}
  Result(temp::Map *coloring, assem::InstrList *il)
      : coloring_(coloring), il_(il) {}
  Result(const Result &result) = delete;
  Result(Result &&result) = delete;
  Result &operator=(const Result &result) = delete;
  Result &operator=(Result &&result) = delete;
  ~Result() {
    delete coloring_;
    delete il_;
  }
};

class RegAllocator {
  /* TODO: Put your lab6 code here */
public:
  RegAllocator(std::string, std::unique_ptr<cg::AssemInstr>);
  void RegAlloc();
  void LivenessAnalysis();
  void Build();
  void AddEdge(live::INodePtr u, live::INodePtr v);
  void MakeWorklist();
  live::INodeListPtr Adjacent(live::INodePtr n);
  live::MoveList *NodeMoves(live::INodePtr n);
  bool MoveRelated(live::INodePtr n);
  void Simplify();
  void DecrementDegree(live::INodePtr m);
  void EnableMoves(live::INodeListPtr nodes);
  void Coalesce();
  void AddWorkList(live::INodePtr u);
  bool OK(live::INodePtr t, live::INodePtr r);
  bool Conservative(live::INodeListPtr nodes);
  live::INodePtr GetAlias(live::INodePtr n);
  void Combine(live::INodePtr u, live::INodePtr v);
  void Freeze();
  void FreezeMoves(live::INodePtr u);
  void SelectSpill();
  void AssignColors();
  void RewriteProgram();
  void Reset();
  std::unique_ptr<ra::Result> TransferResult();

private:
  std::string frame_;
  std::unique_ptr<cg::AssemInstr> assem_instr_;
  live::LiveGraphFactory *live_graph_factory_;
  live::IGraphPtr interf_graph_;

  const int K = 15;

  live::INodeListPtr precolored;        // 机器寄存器集合
  live::INodeListPtr initial;           // 临时寄存器集合
  live::INodeListPtr simplifyWorklist;  // 低度数的传送无关的节点表
  live::INodeListPtr freezeWorklist;    // 低度数的传送有关的节点表
  live::INodeListPtr spillWorklist;     // 高度数的节点表
  live::INodeListPtr spilledNodes;      // 在本轮中要被溢出的节点集合，初始为空
  live::INodeListPtr coalescedNodes;    // 已合并的寄存器集合
  live::INodeListPtr coloredNodes;      // 已成功着色的节点集合
  live::INodeListPtr selectStack;       // 一个包含从图中删除的临时变量的栈

  live::MoveList *coalescedMoves;       // 已经合并的传送指令集合
  live::MoveList *constrainedMoves;     // 源操作数和目标操作数冲突的传送指令集合
  live::MoveList *frozenMoves;          // 不再考虑合并的传送指令集合
  live::MoveList *worklistMoves;        // 有可能合并的传送指令集合
  live::MoveList *activeMoves;          // 还未做好合并准备的传送指令集合

  std::map<live::INodePtr, int> degree;
  std::map<live::INodePtr, live::MoveList *> moveList;
  std::map<live::INodePtr, live::INodePtr> alias;
  std::map<live::INodePtr, temp::Temp *> color;
};

} // namespace ra

#endif