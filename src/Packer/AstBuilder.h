#pragma once

// =================================================================================================
// AST building
namespace asmjs
{
    struct AstBuilder
    {
        static AstNodePtr makeToplevel() { return new TopLevelNode(); }
        static AstNodePtr makeFunction(IString name) { return new FuncNode(name); }

        static void appendArgumentToFunction(AstNode* fun, IString name);

        static AstNode* stmtify(AstNode* expr);

        static AstNodePtr makeIf(AstNode* cond, AstNode* if_true, AstNode* if_false);

        static AstNodePtr makeStatement(AstNode* expr) { return stmtify(expr); }
        static AstNodePtr makeBlock() { return new BlockNode(); }
        static AstNodePtr makeReturn(AstNode* expr) { return new ReturnNode(expr); }
        static AstNodePtr makeWhile(AstNode* cond, AstNode* body) { assert(cond); return new WhileNode(*cond, *stmtify(body)); }
        static AstNodePtr makeDo(AstNode* body, AstNode* cond) { assert(cond); return new DoNode(*stmtify(body), *cond); }
        static AstNodePtr makeLabel(IString str, AstNode* stmt) { return new LabelNode(str, *stmtify(stmt)); }
        static AstNodePtr makeBreak(IString str) { return new BreakNode(str); }
        static AstNodePtr makeContinue(IString str) { return new ContinueNode(str); }
        static AstNodePtr makeSwitch(AstNode* expr) { assert(expr); return new SwitchNode(*expr); }

        static void appendToBlock(AstNode* block, AstNode* stmt)
        {
            if (!stmt)
                return;
            if (block->is<TopLevelNode>())
                block->as<TopLevelNode>().append(*stmt);
            else if (block->is<FuncNode>())
                block->as<FuncNode>().List<AstNode>::append(*stmt);
            else if (stmt->is<BlockNode>())
                block->as<BlockNode>().append_list(stmt->as<BlockNode>());
            else
                block->as<BlockNode>().append(*stmtify(stmt));
        }

        static void appendCaseToSwitch(AstNode* sw, AstNode* label)
        {
            assert(label);
            sw->as<SwitchNode>().append(*new CaseNode(*label));
        }

        static void appendDefaultToSwitch(AstNode* sw)
        {
            sw->as<SwitchNode>().append(*new CaseNode());
        }

        static void appendCodeToSwitch(AstNode* sw, AstNode* stmt, bool)
        {
            if (!stmt)
                return;
            if (stmt->is<BlockNode>())
                for (AstNode* p = stmt->as<BlockNode>().first; p; p = p->next)
                    sw->as<SwitchNode>().last->append(*new CaseStmtNode(*p));
            else
                sw->as<SwitchNode>().last->append(*new CaseStmtNode(*stmtify(stmt)));
        }

        static AstNodePtr makeString(IString str) { return new StringNode(str); }
        static AstNodePtr makeDouble(double f64) { return new DoubleNode(f64); }
        static AstNodePtr makeInt(uint32_t u32) { return new IntNode(u32); }
        static AstNodePtr makeName(IString str) { return new NameNode(str); }
        static AstNodePtr makePrefix(IString op, AstNode* expr) { assert(expr); return new PrefixNode(op, *expr); }
        static AstNodePtr makeBinary(AstNode* lhs, IString op, AstNode* rhs) { assert(lhs && rhs); return new BinaryNode(op, *lhs, *rhs); }
        static AstNodePtr makeConditional(AstNode* cond, AstNode* lhs, AstNode* rhs) { assert(cond && lhs && rhs); return new TernaryNode(*cond, *lhs, *rhs); }
        static AstNodePtr makeNew(AstNode* call) { return new NewNode(call->as<CallNode>()); }
        static AstNodePtr makeDot(AstNode* base, IString name) { assert(base); return new DotNode(*base, name); }
        static AstNodePtr makeDot(AstNode* base, AstNode* name) { assert(base); return new DotNode(*base, name->as<NameNode>().str); }
        static AstNodePtr makeIndexing(AstNode* array, AstNode* index) { assert(array && index); return new IndexNode(*array, *index); }
        static AstNodePtr makeCall(AstNode* callee) { return new CallNode(*callee); }
        static AstNodePtr makeObject() { return new ObjectNode(); }
        static AstNodePtr makeVar(bool is_const) { return new VarNode(); }
        static AstNodePtr makeArray() { return new ArrayNode(); }

        static void appendToCall(AstNode* call, AstNode* arg)
        {
            call->as<CallNode>().append(*arg);
        }

        static void appendToObject(AstNode* object, IString key, AstNode* value)
        {
            assert(value);
            object->as<ObjectNode>().append(*new FieldNode(key, *value));
        }

        static void appendToVar(AstNode* var, IString name, AstNode* init)
        {
            assert(init);
            var->as<VarNode>().append(*new VarNameNode(name, *init));
        }

        static void appendToArray(AstNode* array, AstNode* elem)
        {
            assert(elem);
            array->as<ArrayNode>().append(*elem);
        }
    };
}
