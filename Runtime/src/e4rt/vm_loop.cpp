#include "e4platf/byte_stream_reader.h"
#include "e4rt/vm.h"

namespace e4 {

// Switch-based VM loop (slower but compact code and compact bytecode)
//
// To go faster: see threaded goto(void*) VM loop and convert bytecode to
// label addresses during the load-time.
void VM::run() {
  // schedule:
  auto proc = sched_.next();
  if (not proc) {
    E4LOG0("idle");
    return;  // nothing to do
    // TODO: perform maintenance tasks, enter energy saving idle
  }

  auto& context_ = proc->context_;

  auto pc0 = context_.pc_.ptr();
  auto pc_last = pc0 + 100;

  uint8_t instruction_raw;
  Instruction instruction;
fetch:
  instruction_raw = platf::unaligned_read<uint8_t>(pc0);
  pc0++;

  // Those 3 masked bits may contain extra flags for the command
  instruction = (Instruction)(instruction_raw & 0b0001'1111);

  switch (instruction) {
    case instr::FuncInfo: {
      Word fn, ar;
      pc0 = e4::tool::read_varint(pc0, MUTABLE fn);
      pc0 = e4::tool::read_varint(pc0, MUTABLE ar);
      E4LOG3("[%p] func_info fun=%zu arity=%zu\n", pc0, fn, ar);
    } break;

    case instr::CallLocal: {
      Word lbl;
      pc0 = e4::tool::read_varint(pc0, MUTABLE lbl);
      E4LOG2("[%p] call_local label=%zu\n", pc0, lbl);
    } break;

    case instr::CallExt: {
      Word import_i;
      pc0 = e4::tool::read_varint(pc0, MUTABLE import_i);
      E4LOG2("[%p] call_ext import=%zu\n", pc0, import_i);
    } break;

    case instr::Bif: {
      Word name;
      pc0 = e4::tool::read_varint(pc0, MUTABLE name);
      E4LOG2("[%p] bif name=%zu\n", pc0, name);
    } break;

    case instr::AllocStack: {
      Word stack_need, live;
      pc0 = e4::tool::read_varint(pc0, MUTABLE stack_need);
      pc0 = e4::tool::read_varint(pc0, MUTABLE live);
      E4LOG3("[%p] alloc_stack need=%zu live=%zu\n", pc0, stack_need, live);
    } break;

    case instr::AllocStackHeap: {} break;

    case instr::GetElement: {} break;

    case instr::Move: {} break;

    case instr::CallFun: {} break;

    case instr::SetNil: {} break;

    case instr::TestHeap: {} break;

    case instr::PutTuple: {} break;

    case instr::Put: {} break;

    case instr::Ret0: {} break;

    case instr::RetN: {} break;

    case instr::SelectVal: {} break;

    case instr::Cons: {} break;

    case instr::Jump: {
//      proc->jump_rel(offs);
    } break;

    case instr::Trim: {} break;

    case instr::MakeFun: {} break;

    case instr::SetElement: {} break;

    case instr::ClearStack: {} break;
  }

  if (pc0 < pc_last) goto fetch;
}

}  // ns e4
