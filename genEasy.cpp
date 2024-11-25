#include <iostream>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <map>

std::map<std::string, llvm::Function *> functions;

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
}

void buildMain(std::shared_ptr<llvm::Module> ir_module)
{
    llvm::IRBuilder<> builder(ir_module->getContext());

    llvm::Function *main = addFunction(ir_module, "main", llvm::FunctionType::get(llvm::Type::getInt32Ty(ir_module->getContext()), false));
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(ir_module->getContext(), "", main);
    builder.SetInsertPoint(entry);

    // TODO
    llvm::AllocaInst *a = builder.CreateAlloca(llvm::Type::getInt32Ty(ir_module->getContext()), nullptr);
    llvm::AllocaInst *b = builder.CreateAlloca(llvm::Type::getInt32Ty(ir_module->getContext()), nullptr);

    builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 1), a);
    builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 2), b);

    llvm::LoadInst *a_val = builder.CreateLoad(llvm::Type::getInt32Ty(ir_module->getContext()), a);
    llvm::LoadInst *b_val = builder.CreateLoad(llvm::Type::getInt32Ty(ir_module->getContext()), b);

    llvm::Value *cond = builder.CreateICmpSLT(a_val, b_val);
    
    llvm::BasicBlock *then_block = llvm::BasicBlock::Create(ir_module->getContext(), "", main);
    llvm::BasicBlock *merge_block = llvm::BasicBlock::Create(ir_module->getContext(), "", main);

    builder.CreateCondBr(cond, then_block, merge_block);

    builder.SetInsertPoint(then_block);
    builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ir_module->getContext()), 3), b);
    builder.CreateBr(merge_block);

    builder.SetInsertPoint(merge_block);
    llvm::LoadInst *a_new_val = builder.CreateLoad(llvm::Type::getInt32Ty(ir_module->getContext()), a);
    llvm::LoadInst *b_new_val = builder.CreateLoad(llvm::Type::getInt32Ty(ir_module->getContext()), b);
    llvm::Value *result = builder.CreateNSWAdd(a_new_val, b_new_val);

    builder.CreateRet(result);
}

void buildFunction(std::shared_ptr<llvm::Module> ir_module)
{
    buildMain(ir_module);
}

int main()
{
    llvm::LLVMContext context;
    std::shared_ptr<llvm::Module> ir_module = std::make_shared<llvm::Module>("easy", context);
    ir_module->setTargetTriple("x86_64-pc-linux-gnu");

    buildGlobal(ir_module);
    buildFunction(ir_module);

    ir_module->print(llvm::outs(), nullptr);

    return 0;
}