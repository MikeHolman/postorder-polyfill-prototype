// vim: set ts=2 sw=2 tw=99 et:

#include "Packer.h"

using namespace std;
using cashew::IString;

namespace asmjs
{

const FuncNode&
parse(char* src)
{
    const AstNode* ast = cashew::Parser<AstNodePtr, AstBuilder>().parseToplevel(src);
    const TopLevelNode& top = ast->as<TopLevelNode>();
    if (!top.first || top.first != top.last || !top.first->is<FuncNode>())
    {
        throw runtime_error("there should be exactly one top-level asm.js statement in the asm.js file");
    }

    FuncNode& module_func = top.first->as<FuncNode>();
    List<AstNode>& body = module_func;
    if (!body.first || !body.first->is<StringNode>() || !body.first->as<StringNode>().str.equals("use asm"))
    {
        throw runtime_error("the argument to the top-level call should be an asm.js module");
    }

    return module_func;
}

void
patch_unpacked_size(std::ostream& os, uint32_t unpacked_size)
{
    os.seekp(sizeof(uint32_t));
    Out out(os);
    out.fixed_width<uint32_t>(unpacked_size);
}

void
pack(ostream& os, const FuncNode& module)
{
    Module m(os);
    analyze_module(m, module);
    write_module(m);
}

}  // namespace asmjs

int
main(int argc, char** argv)
try
{
    if (argc != 3 || !argv[1] || !argv[2])
    {
        cerr << "Usage: pack-asmjs in.js out.wasm" << endl;
        return -1;
    }

    // Parse the asm.js file.
    ifstream in_stream(argv[1], ios::binary | ios::ate);
    in_stream.exceptions(ios::failbit | ios::badbit);
    vector<char> in_bytes(in_stream.tellg());
    in_stream.seekg(0);
    in_stream.read(in_bytes.data(), in_bytes.size());
    in_bytes.push_back('\0');
    in_stream.close();
    const asmjs::FuncNode& module = asmjs::parse(in_bytes.data());

    // Write out the .asm file (with bogus unpacked-size).
    fstream out_stream(argv[2], ios::in | ios::out | ios::binary | ios::trunc);
    out_stream.exceptions(ios::failbit | ios::badbit);
    asmjs::pack(out_stream, module);

    // Compute unpacked-size (using unpack()) and patch the file.
    vector<uint8_t> out_bytes(out_stream.tellp());
    out_stream.seekg(0);
    out_stream.read((char*)out_bytes.data(), out_bytes.size());
#ifdef CHECKED_OUTPUT_SIZE
    uint32_t unpacked_size = asmjs::calculate_unpacked_size(out_bytes.data());
    asmjs::patch_unpacked_size(out_stream, unpacked_size);
#endif
    return 0;
}

catch (const ios::failure& err)
{
    cerr << "Failed with runtime error: " << err.what() << endl;
    return -1;
}

catch (const runtime_error& err)
{
    cerr << "Failed with runtime error: " << err.what() << endl;
    return -1;
}
