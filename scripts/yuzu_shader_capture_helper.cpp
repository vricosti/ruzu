// LLDB helper loaded into yuzu to serialize the upstream Shader::IR::Program.

#include <fstream>

#include "shader_recompiler/frontend/ir/program.h"
#include "video_core/memory_manager.h"

extern "C" void RuzuDumpYuzuShaderIr(const Shader::IR::Program* program, const char* path) {
    std::ofstream output{path, std::ios::binary | std::ios::trunc};
    output << Shader::IR::DumpProgram(*program);
}

extern "C" void RuzuDumpYuzuGpuMemory(const Tegra::MemoryManager* memory_manager,
                                       const u64 gpu_address, const std::size_t size,
                                       const char* path) {
    std::string bytes(size, '\0');
    memory_manager->ReadBlockUnsafe(gpu_address, bytes.data(), bytes.size());
    std::ofstream output{path, std::ios::binary | std::ios::trunc};
    output.write(bytes.data(), static_cast<std::streamsize>(bytes.size()));
}
