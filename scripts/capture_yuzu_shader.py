#!/usr/bin/env python3
"""Capture one yuzu graphics pipeline's upstream IR and SPIR-V with LLDB.

Load this script from LLDB before launching yuzu. The command filters pipeline
creation by VertexB and fragment unique hashes, so unrelated asynchronous
pipeline compilations do not contaminate the capture.
"""

import pathlib
import shlex
import struct

import lldb


_state = {}

_FIXED_STATE_OFFSET = 6 * 8
_DYNAMIC_STATE_OFFSET = 64
_ATTRIBUTES_OFFSET = 104
_VERTEX_STRIDES_OFFSET = 360
_XFB_STATE_OFFSET = 424


def _fixed_state_size(raw1):
    xfb_enabled = bool(raw1 & (1 << 6))
    dynamic_vertex_input = bool(raw1 & (1 << 5))
    extended_dynamic_state = bool(raw1 & (1 << 0))
    extended_dynamic_state_3_blend = bool(raw1 & (1 << 3))
    if xfb_enabled:
        raise RuntimeError(
            "transform-feedback state size requires the upstream sizeof; "
            "this capture currently targets non-XFB transition pipelines"
        )
    if dynamic_vertex_input and extended_dynamic_state_3_blend:
        return _DYNAMIC_STATE_OFFSET
    if dynamic_vertex_input:
        return _ATTRIBUTES_OFFSET
    if extended_dynamic_state:
        return _VERTEX_STRIDES_OFFSET
    return _XFB_STATE_OFFSET


def _dump_pipeline_key(process, key_address, hashes):
    state_prefix = _read(process, key_address + _FIXED_STATE_OFFSET, 64)
    raw1, raw2 = struct.unpack_from("<II", state_prefix)
    state_size = _fixed_state_size(raw1)
    state = _read(process, key_address + _FIXED_STATE_OFFSET, state_size)
    key = _read(process, key_address, _FIXED_STATE_OFFSET + state_size)
    (_state["output"] / "fixed_state.bin").write_bytes(state)
    (_state["output"] / "key.bin").write_bytes(key)

    color_formats = tuple(state[8:16])
    alpha_test_ref, point_size = struct.unpack_from("<II", state, 16)
    viewport_swizzles = struct.unpack_from("<16H", state, 24)
    attribute_types = struct.unpack_from("<Q", state, 56)[0]
    lines = [
        "unique_hashes=" + " ".join(f"0x{value:016X}" for value in hashes),
        f"state_size={state_size}",
        f"raw1=0x{raw1:08X}",
        f"raw2=0x{raw2:08X}",
        "color_formats=" + " ".join(f"0x{value:02X}" for value in color_formats),
        f"alpha_test_ref=0x{alpha_test_ref:08X}",
        f"point_size=0x{point_size:08X}",
        "viewport_swizzles="
        + " ".join(f"0x{value:04X}" for value in viewport_swizzles),
        f"attribute_types_or_enabled_divisors=0x{attribute_types:016X}",
    ]
    if state_size >= _DYNAMIC_STATE_OFFSET + 8:
        dynamic_raw1, dynamic_raw2 = struct.unpack_from("<II", state, 64)
        lines.extend(
            (
                f"dynamic_raw1=0x{dynamic_raw1:08X}",
                f"dynamic_raw2=0x{dynamic_raw2:08X}",
            )
        )
    if state_size >= _ATTRIBUTES_OFFSET:
        attachments = struct.unpack_from("<8I", state, 72)
        lines.append(
            "attachments=" + " ".join(f"0x{value:08X}" for value in attachments)
        )
    (_state["output"] / "fixed_state.txt").write_text("\n".join(lines) + "\n")
    print(f"[yuzu-capture] wrote pipeline key: {len(key)} bytes")


def _register_u64(frame, name):
    return frame.FindRegister(name).GetValueAsUnsigned()


def _read(process, address, size):
    error = lldb.SBError()
    data = process.ReadMemory(address, size, error)
    if not error.Success():
        raise RuntimeError(f"read 0x{address:x}+0x{size:x}: {error}")
    return data


def _function_address(target, name):
    functions = target.FindFunctions(name, lldb.eFunctionNameTypeAuto)
    for index in range(functions.GetSize()):
        context = functions.GetContextAtIndex(index)
        function = context.GetFunction()
        if function.IsValid():
            address = function.GetStartAddress().GetLoadAddress(target)
            if address != lldb.LLDB_INVALID_ADDRESS:
                return address
        symbol = context.GetSymbol()
        if symbol.IsValid():
            address = symbol.GetStartAddress().GetLoadAddress(target)
            if address != lldb.LLDB_INVALID_ADDRESS:
                return address
    raise RuntimeError(f"symbol not found: {name}")


def _dump_ir(frame, program_address, stage):
    target = frame.GetThread().GetProcess().GetTarget()
    dump_address = _function_address(target, "RuzuDumpYuzuShaderIr")
    output_path = str(_state["output"] / f"{stage}.ir.txt")
    escaped_path = output_path.replace("\\", "\\\\").replace('"', '\\"')
    options = lldb.SBExpressionOptions()
    options.SetLanguage(lldb.eLanguageTypeC_plus_plus)
    options.SetTimeoutInMicroSeconds(30_000_000)
    expression = (
        f"((void(*)(const void*, const char*))0x{dump_address:x})"
        f"((const void*)0x{program_address:x}, \"{escaped_path}\")"
    )
    value = frame.EvaluateExpression(expression, options)
    if value.GetError().Fail() and not pathlib.Path(output_path).is_file():
        raise RuntimeError(f"DumpProgram expression: {value.GetError()}")


def _on_pipeline(frame, _location, _dict):
    process = frame.GetThread().GetProcess()
    key_address = _register_u64(frame, "x2")
    try:
        hashes = struct.unpack("<6Q", _read(process, key_address, 48))
    except RuntimeError as error:
        print(f"[yuzu-capture] {error}")
        return False
    if hashes[1] != _state["vertex_hash"] or hashes[5] != _state["fragment_hash"]:
        return False

    print(
        "[yuzu-capture] matched pipeline "
        f"VB=0x{hashes[1]:016X} FS=0x{hashes[5]:016X}"
    )
    try:
        _dump_pipeline_key(process, key_address, hashes)
    except RuntimeError as error:
        print(f"[yuzu-capture] pipeline key capture failed: {error}")
    _state["pipeline_breakpoint"].SetEnabled(False)
    target = process.GetTarget()
    breakpoint = target.BreakpointCreateByName("Shader::Backend::SPIRV::EmitSPIRV")
    breakpoint.SetThreadID(frame.GetThread().GetThreadID())
    breakpoint.SetScriptCallbackFunction("capture_yuzu_shader._on_emit_spirv")
    _state["emit_breakpoint"] = breakpoint
    return False


def _on_emit_spirv(frame, _location, _dict):
    stage_index = _state["stage_index"]
    if stage_index >= 2:
        return False
    stage = ("vertex", "fragment")[stage_index]
    program_address = _register_u64(frame, "x2")
    vector_address = _register_u64(frame, "x8")
    return_address = _register_u64(frame, "x30")
    try:
        _dump_ir(frame, program_address, stage)
    except RuntimeError as error:
        print(f"[yuzu-capture] IR capture failed for {stage}: {error}")

    target = frame.GetThread().GetProcess().GetTarget()
    breakpoint = target.BreakpointCreateByAddress(return_address)
    breakpoint.SetThreadID(frame.GetThread().GetThreadID())
    breakpoint.SetOneShot(True)
    breakpoint.SetScriptCallbackFunction("capture_yuzu_shader._on_emit_return")
    _state["returns"][breakpoint.GetID()] = (stage, vector_address)
    _state["emit_breakpoint"].SetEnabled(False)
    return False


def _on_emit_return(frame, location, _dict):
    process = frame.GetThread().GetProcess()
    breakpoint_id = location.GetBreakpoint().GetID()
    pending = _state["returns"].pop(breakpoint_id, None)
    if pending is None:
        return False
    stage, vector_address = pending
    try:
        begin, end = struct.unpack("<QQ", _read(process, vector_address, 16))
        if end < begin or (end - begin) % 4 != 0:
            raise RuntimeError(
                f"invalid SPIR-V vector [0x{begin:x}, 0x{end:x})"
            )
        spirv = _read(process, begin, end - begin)
        (_state["output"] / f"{stage}.spv").write_bytes(spirv)
        print(f"[yuzu-capture] wrote {stage}: {len(spirv) // 4} SPIR-V words")
    except RuntimeError as error:
        print(f"[yuzu-capture] SPIR-V capture failed for {stage}: {error}")

    _state["stage_index"] += 1
    if _state["stage_index"] == 2:
        print(f"[yuzu-capture] complete: {_state['output']}")
        return True
    _state["emit_breakpoint"].SetEnabled(True)
    return False


def capture_yuzu_shader(debugger, command, result, _dict):
    """capture-yuzu-shader OUTPUT_DIR VERTEX_HASH FRAGMENT_HASH"""
    arguments = shlex.split(command)
    if len(arguments) != 3:
        result.SetError(
            "usage: capture-yuzu-shader OUTPUT_DIR VERTEX_HASH FRAGMENT_HASH"
        )
        return
    output = pathlib.Path(arguments[0]).expanduser().resolve()
    output.mkdir(parents=True, exist_ok=True)
    target = debugger.GetSelectedTarget()
    breakpoint = target.BreakpointCreateByName(
        "Vulkan::PipelineCache::CreateGraphicsPipeline"
    )
    breakpoint.SetScriptCallbackFunction("capture_yuzu_shader._on_pipeline")
    _state.clear()
    _state.update(
        output=output,
        vertex_hash=int(arguments[1], 0),
        fragment_hash=int(arguments[2], 0),
        pipeline_breakpoint=breakpoint,
        emit_breakpoint=None,
        stage_index=0,
        returns={},
    )
    debugger.HandleCommand("settings set target.max-string-summary-length 16777216")
    result.AppendMessage(f"pipeline capture armed; output={output}")


def __lldb_init_module(debugger, _dict):
    debugger.HandleCommand(
        "command script add -f capture_yuzu_shader.capture_yuzu_shader "
        "capture-yuzu-shader"
    )
