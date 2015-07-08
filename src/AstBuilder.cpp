#include "Packer.h"

namespace asmjs
{

void
AstBuilder::appendArgumentToFunction(AstNode* fun, IString name)
{
    fun->as<FuncNode>().List<ArgNode>::append(*new ArgNode(name));
}

AstNode *
AstBuilder::stmtify(AstNode* expr)
{
    if (!expr)
        return nullptr;
    switch (expr->which) {
    case AstNode::New:
    case AstNode::Object:
    case AstNode::Array:
        unreachable<void>();
    case AstNode::TopLevel:
    case AstNode::Function:
    case AstNode::String:
    case AstNode::Var:
    case AstNode::Block:
    case AstNode::Return:
    case AstNode::If:
    case AstNode::While:
    case AstNode::Do:
    case AstNode::Label:
    case AstNode::Break:
    case AstNode::Continue:
    case AstNode::Switch:
    case AstNode::Call:
        return expr;
    case AstNode::Double:
    case AstNode::Int:
    case AstNode::Name:
        return nullptr;
    case AstNode::Dot:
        return stmtify(&expr->as<DotNode>().base);
    case AstNode::Index:
        return stmtify(expr->as<IndexNode>().index);
    case AstNode::Prefix:
        if (!is_double_coerced_call(expr->as<PrefixNode>()))
            return stmtify(&expr->as<PrefixNode>().kid);
        return expr;
    case AstNode::Binary: {
        BinaryNode& binary = expr->as<BinaryNode>();
        if (!binary.op.equals("=") && !is_int_coerced_call(binary)) {
            AstNode* lhs = stmtify(&binary.lhs);
            AstNode* rhs = stmtify(&binary.rhs);
            return lhs && rhs ? new BlockNode(*lhs, *rhs) : lhs ? lhs : rhs;
        }
        return expr;
    }
    case AstNode::Ternary: {
        TernaryNode& ternary = expr->as<TernaryNode>();
        return makeIf(&ternary.cond, &ternary.lhs, &ternary.rhs);
    }
    }
    return nullptr;
}

AstNodePtr
AstBuilder::makeIf(AstNode* cond, AstNode* if_true, AstNode* if_false)
{
    assert(cond);
    if_true = stmtify(if_true);
    if_false = stmtify(if_false);
    if (if_true)
        return new IfNode(*cond, *if_true, if_false);
    if (if_false)
        return new IfNode(*new PrefixNode(cashew::L_NOT, *cond), *if_false, nullptr);
    return nullptr;
}

} //namespace asmjs
