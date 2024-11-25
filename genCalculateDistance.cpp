#include <iostream>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <map>

std::map<std::string, llvm::StructType *> struct_types;
std::map<std::string, llvm::GlobalVariable *> global_values;
std::map<std::string, llvm::Function *> functions;

llvm::StructType *addStructType(std::shared_ptr<llvm::Module> ir_module, std::string name, std::vector<llvm::Type *> fields)
{
    llvm::StructType *struct_type = llvm::StructType::create(ir_module->getContext(), name);
    struct_type->setBody(fields);
    struct_types.insert(std::make_pair(name, struct_type));
    return struct_type;
}

llvm::GlobalVariable *addGlobalValue(std::shared_ptr<llvm::Module> ir_module, std::string name, llvm::Type *type, llvm::Constant *initializer, int align)
{
    llvm::GlobalVariable *global = (llvm::GlobalVariable *)ir_module->getOrInsertGlobal(name, type);
    global->setInitializer(initializer);
    global->setDSOLocal(true);
    global->setAlignment(llvm::MaybeAlign(align));
    global_values.insert(std::make_pair(name, global));
    return global;
}

llvm::GlobalVariable *addGlobalString(std::shared_ptr<llvm::Module> ir_module, std::string name, std::string value)
{
    llvm::GlobalVariable *global = (llvm::GlobalVariable *)ir_module->getOrInsertGlobal(name, llvm::ArrayType::get(llvm::Type::getInt8Ty(ir_module->getContext()), value.size() + 1));
    global->setInitializer(llvm::ConstantDataArray::getString(ir_module->getContext(), value, true));
    global->setDSOLocal(true);
    global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    global->setLinkage(llvm::GlobalValue::LinkageTypes::PrivateLinkage);
    global->setConstant(true);
    global->setAlignment(llvm::MaybeAlign(1));
    global_values.insert(std::make_pair(name, global));
    return global;
}

llvm::Function *addFunction(std::shared_ptr<llvm::Module> ir_module, std::string name, llvm::FunctionType *type)
{
    llvm::Function *function = llvm::Function::Create(type, llvm::Function::ExternalLinkage, name, ir_module.get());
    for (auto &arg : function->args())
        arg.addAttr(llvm::Attribute::NoUndef);
    functions.insert(std::make_pair(name, function));
    return function;
}

void buildGlobal(std::shared_ptr<llvm::Module> ir_module)
{
    llvm::IRBuilder<> ir_builder(ir_module->getContext());

    // add struct type
    llvm::Type *Edge_s_fields[] = {llvm::Type::getInt32Ty(ir_module->getContext()), llvm::Type::getInt32Ty(ir_module->getContext()), llvm::Type::getInt64Ty(ir_module->getContext())};
    llvm::StructType *Edge_s = addStructType(ir_module, "struct.Edge_s", std::vector<llvm::Type *>(Edge_s_fields, Edge_s_fields + 3));

    // add global value
    llvm::PointerType::get(Edge_s, 0);
    llvm::GlobalVariable *edge1 = addGlobalValue(ir_module, "edge1", Edge_s, llvm::ConstantStruct::get(Edge_s, llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0), llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0), llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 5)), 8);
    llvm::GlobalVariable *edge2 = addGlobalValue(ir_module, "edge2", Edge_s, llvm::ConstantStruct::get(Edge_s, llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0), llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0), llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 10)), 8);
    addGlobalValue(ir_module, "allDist", llvm::ArrayType::get(llvm::ArrayType::get(llvm::Type::getInt32Ty(ir_module->getContext()), 3), 3), llvm::ConstantAggregateZero::get(llvm::ArrayType::get(llvm::ArrayType::get(llvm::Type::getInt32Ty(ir_module->getContext()), 3), 3)), 16);
    addGlobalValue(ir_module, "dist", llvm::ArrayType::get(llvm::PointerType::get(Edge_s, 0), 3), llvm::ConstantArray::get(llvm::ArrayType::get(llvm::PointerType::get(Edge_s, 0), 3), {llvm::ConstantExpr::getBitCast(edge1, llvm::PointerType::get(Edge_s, 0)), llvm::ConstantExpr::getBitCast(edge2, llvm::PointerType::get(Edge_s, 0)), llvm::ConstantPointerNull::get(llvm::PointerType::get(Edge_s, 0))}), 16);
    addGlobalValue(ir_module, "minDistance", llvm::Type::getInt64Ty(ir_module->getContext()), llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 5), 8);

    // add global string
    llvm::GlobalVariable *str = addGlobalString(ir_module, ".str", "%lld\00");
    llvm::GlobalVariable *str1 = addGlobalString(ir_module, ".str1", "%lld %lld %d\n\00");

    // add external function
    addFunction(ir_module, "__isoc99_scanf", llvm::FunctionType::get(llvm::Type::getInt32Ty(ir_module->getContext()), llvm::PointerType::get(llvm::Type::getInt8Ty(ir_module->getContext()), 0), true));
    addFunction(ir_module, "printf", llvm::FunctionType::get(llvm::Type::getInt32Ty(ir_module->getContext()), llvm::PointerType::get(llvm::Type::getInt8Ty(ir_module->getContext()), 0), true));
}

void buildCaculateDistance(std::shared_ptr<llvm::Module> ir_module)
{
    llvm::IRBuilder<> builder(ir_module->getContext());

    llvm::Function *caculateDistance = addFunction(ir_module, "caculateDistance", llvm::FunctionType::get(llvm::Type::getVoidTy(ir_module->getContext()), false));
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(ir_module->getContext(), "", caculateDistance);
    builder.SetInsertPoint(entry);

    // TODO

    // basic block for loop and conditional branch
    llvm::BasicBlock *test_block = llvm::BasicBlock::Create(ir_module->getContext(), "", caculateDistance);
    llvm::BasicBlock *loop_block = llvm::BasicBlock::Create(ir_module->getContext(), "", caculateDistance);
    llvm::BasicBlock *then_block = llvm::BasicBlock::Create(ir_module->getContext(), "", caculateDistance);
    llvm::BasicBlock *else_block = llvm::BasicBlock::Create(ir_module->getContext(), "", caculateDistance);
    llvm::BasicBlock *merge_block = llvm::BasicBlock::Create(ir_module->getContext(), "", caculateDistance);
    llvm::BasicBlock *inc_block = llvm::BasicBlock::Create(ir_module->getContext(), "", caculateDistance);
    llvm::BasicBlock *done_block = llvm::BasicBlock::Create(ir_module->getContext(), "", caculateDistance);

    /**
     * Init part:
     * int k;
     * long long kDist;
     * k = 0;
     * goto test_block;
     */
    llvm::AllocaInst *k = builder.CreateAlloca(llvm::Type::getInt32Ty(ir_module->getContext()), nullptr);
    llvm::AllocaInst *kDist = builder.CreateAlloca(llvm::Type::getInt64Ty(ir_module->getContext()), nullptr);
    builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0), k);
    builder.CreateBr(test_block);

    /**
     * Test part:
     * int k_val_test = k;
     * if (k_val_test < 3) goto loop_block;
     * else goto done_block;
     */
    builder.SetInsertPoint(test_block);
    llvm::LoadInst *k_val_test = builder.CreateLoad(llvm::Type::getInt32Ty(ir_module->getContext()), k);
    llvm::Value *cond = builder.CreateICmpSLT(k_val_test, llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 3));
    builder.CreateCondBr(cond, loop_block, done_block);

    // get global variables and types
    llvm::GlobalVariable *dist_array = global_values["dist"];
    llvm::GlobalVariable *minDistance = global_values["minDistance"];
    llvm::StructType *edge_s = struct_types["struct.Edge_s"];

    /**
     * Loop part:
     * int k32_val = k;
     * long long k64_val = (long long)k32_val;
     * Edge **edge_ptr = &dist[k64_val];
     * Edege *edge = *edge_ptr;
     * long long *w_ptr = &edge->w;
     * long long w_val = *w_ptr;
     * kDist = w_val;
     * long long kDist_val = kDist;
     * long long minDistance_val = minDistance;
     * if (kDist_val < minDistance_val) goto then_block;
     * else goto else_block;
     */
    builder.SetInsertPoint(loop_block);
    llvm::LoadInst *k32_val = builder.CreateLoad(llvm::Type::getInt32Ty(ir_module->getContext()), k);
    llvm::Value *k64_val = builder.CreateSExt(k32_val, llvm::Type::getInt64Ty(ir_module->getContext()));
    llvm::Value *edge_ptr = builder.CreateGEP(
        dist_array->getType()->getPointerElementType(), 
        dist_array, 
        {llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 0), k64_val}
    );
    llvm::LoadInst *edge = builder.CreateLoad(llvm::PointerType::get(edge_s, 0), edge_ptr);
    llvm::Value *w_ptr = builder.CreateGEP(
        edge->getType()->getPointerElementType(),
        edge,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0), llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 2)}
    );
    llvm::LoadInst *w_val = builder.CreateLoad(llvm::Type::getInt64Ty(ir_module->getContext()), w_ptr);
    builder.CreateStore(w_val, kDist);
    llvm::LoadInst *kDist_val = builder.CreateLoad(llvm::Type::getInt64Ty(ir_module->getContext()), kDist);
    llvm::LoadInst *minDistance_val = builder.CreateLoad(llvm::Type::getInt64Ty(ir_module->getContext()), minDistance);
    llvm::Value *cond2 = builder.CreateICmpSLT(kDist_val, minDistance_val);
    builder.CreateCondBr(cond2, then_block, else_block);

    /**
     * Then part:
     * long long t1 = kDist;
     * goto merge_block;
     */
    builder.SetInsertPoint(then_block);
    llvm::LoadInst *t1 = builder.CreateLoad(llvm::Type::getInt64Ty(ir_module->getContext()), kDist);
    builder.CreateBr(merge_block);

    /**
     * Else part:
     * long long t2 = minDistance;
     * goto merge_block;
     */
    builder.SetInsertPoint(else_block);
    llvm::LoadInst *t2 = builder.CreateLoad(llvm::Type::getInt64Ty(ir_module->getContext()), minDistance);
    builder.CreateBr(merge_block);

    /**
     * Merge part:
     * (t1, t2) -> t
     * minDistance = t;
     * goto inc_block;
     */
    builder.SetInsertPoint(merge_block);
    llvm::PHINode *t = builder.CreatePHI(llvm::Type::getInt64Ty(ir_module->getContext()), 2);
    t->addIncoming(t1, then_block);
    t->addIncoming(t2, else_block);
    builder.CreateStore(t, minDistance);
    builder.CreateBr(inc_block);

    /**
     * Increment part:
     * int k_old_val = k;
     * int k_new_val = k_old_val + 1;
     * k = k_new_val;
     * goto test_block;
     */
    builder.SetInsertPoint(inc_block);
    llvm::LoadInst *k_old_val = builder.CreateLoad(llvm::Type::getInt32Ty(ir_module->getContext()), k);
    llvm::Value *k_new_val = builder.CreateNSWAdd(k_old_val, llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 1));
    builder.CreateStore(k_new_val, k);
    builder.CreateBr(test_block);

    /**
     * Done part:
     * return;
     */
    builder.SetInsertPoint(done_block);
    builder.CreateRetVoid();
}

void buildMain(std::shared_ptr<llvm::Module> ir_module)
{
    llvm::IRBuilder<> builder(ir_module->getContext());

    llvm::Function *main = addFunction(ir_module, "main", llvm::FunctionType::get(llvm::Type::getInt32Ty(ir_module->getContext()), false));
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(ir_module->getContext(), "", main);
    builder.SetInsertPoint(entry);
    
    // TODO

    // Edge edge;
    llvm::StructType *edge_s = struct_types["struct.Edge_s"];
    llvm::AllocaInst *edge = builder.CreateAlloca(edge_s, nullptr);
    llvm::Value *w_ptr = builder.CreateGEP(
        edge->getType()->getPointerElementType(),
        edge,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0), llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 2)}
    );

    // scanf("%lld", &edge.w);
    llvm::Function *_scanf = functions["__isoc99_scanf"];
    llvm::GlobalVariable *_scanf_str = global_values[".str"];
    llvm::Value *_scanf_str_ptr = builder.CreateGEP(
        _scanf_str->getType()->getPointerElementType(),
        _scanf_str,
        {llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 0),
         llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 0)}
    );
    builder.CreateCall(_scanf, {_scanf_str_ptr, w_ptr});

 
    // dist[NUM - 1] = &edge;
    llvm::GlobalVariable *dist_array = global_values["dist"];
    llvm::Value *edge_ptr = builder.CreateGEP(
        dist_array->getType()->getPointerElementType(), 
        dist_array, 
        {llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 0),
         llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 2)}
    );
    builder.CreateStore(edge, edge_ptr);

    // allDist[0][0] = edge.w;
    llvm::Value *w_ptr_2 = builder.CreateGEP(
        edge->getType()->getPointerElementType(),
        edge,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 2)}
    );
    llvm::LoadInst *w64_val = builder.CreateLoad(llvm::Type::getInt64Ty(ir_module->getContext()), w_ptr);
    llvm::Value *w32_val = builder.CreateTrunc(w64_val, llvm::Type::getInt32Ty(ir_module->getContext()));
    llvm::GlobalVariable *allDist = global_values["allDist"];
    llvm::Value *allDist_ptr = builder.CreateGEP(
        allDist->getType()->getPointerElementType(),
        allDist,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0),}
    );
    builder.CreateStore(w32_val, allDist_ptr);

    // caculateDistance();
    llvm::Function *caculateDistance = functions["caculateDistance"];
    builder.CreateCall(caculateDistance);

    // printf("%lld %lld %d\n", minDistance, edge.w + 5 + 10, allDist[0][0]);
    llvm::GlobalVariable *minDistance = global_values["minDistance"];
    llvm::LoadInst *minDistance_val = builder.CreateLoad(llvm::Type::getInt64Ty(ir_module->getContext()), minDistance);
    llvm::Value *w_ptr_3 = builder.CreateGEP(
        edge->getType()->getPointerElementType(),
        edge,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 2)}
    );
    llvm::Value *w_val = builder.CreateLoad(llvm::Type::getInt64Ty(ir_module->getContext()), w_ptr_3);
    llvm::Value *w_add_5 = builder.CreateNSWAdd(w_val, llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 5));
    llvm::Value *w_add_15 = builder.CreateNSWAdd(w_add_5, llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 10));
    llvm::Value *allDist_ptr_2 = builder.CreateGEP(
        allDist->getType()->getPointerElementType(),
        allDist,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0),}
    );
    llvm::LoadInst *allDist_val = builder.CreateLoad(llvm::Type::getInt32Ty(ir_module->getContext()), allDist_ptr_2);
    llvm::Function *_printf = functions["printf"];
    llvm::GlobalVariable *_printf_str = global_values[".str1"];
    llvm::Value *_printf_str_ptr = builder.CreateGEP(
        _printf_str->getType()->getPointerElementType(),
        _printf_str,
        {llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 0),
         llvm::ConstantInt::get(llvm::Type::getInt64Ty(ir_module->getContext()), 0)}
    );
    builder.CreateCall(_printf, {_printf_str_ptr, minDistance_val, w_add_15, allDist_val});

    // return 0;
    builder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 0));
}

void buildFunction(std::shared_ptr<llvm::Module> ir_module)
{
    buildCaculateDistance(ir_module);
    buildMain(ir_module);
}

int main(int, char **)
{
    llvm::LLVMContext context;
    std::shared_ptr<llvm::Module> ir_module = std::make_shared<llvm::Module>("calculateDistance", context);
    ir_module->setTargetTriple("x86_64-pc-linux-gnu");

    buildGlobal(ir_module);
    buildFunction(ir_module);

    ir_module->print(llvm::outs(), nullptr);

    return 0;
}
