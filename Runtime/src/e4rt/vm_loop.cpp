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
  instruction = static_cast<Instruction>(instruction_raw & 0b0001'1111);

  switch (instruction) {
    case instr::FuncInfo: {
      Word fn = platf::unaligned_read_big<Word>(pc0);
      Word ar = platf::unaligned_read_big<Word>(pc0 + BYTES_PER_WORD);
      pc0 += 2 * BYTES_PER_WORD;
      E4LOG3("[%p] func_info fun=%zu arity=%zu\n", pc0, fn, ar);
    } break;

    case instr::CallLocal: {
      Word lbl = platf::unaligned_read_big<Word>(pc0);
      pc0 += BYTES_PER_WORD;
      E4LOG2("[%p] call_local label=%zu\n", pc0, lbl);
    } break;

    case instr::CallExt: {
      Word import_i = platf::unaligned_read_big<Word>(pc0);
      pc0 += BYTES_PER_WORD;
      E4LOG2("[%p] call_ext import=%zu\n", pc0, import_i);
    } break;

    case instr::CallBif: {
      Word name = platf::unaligned_read_big<Word>(pc0);
      pc0 += BYTES_PER_WORD;
      E4LOG2("[%p] bif name=%zu\n", pc0, name);
    } break;

    case instr::Alloc: {
      Word arg0 = platf::unaligned_read_big<Word>(pc0);
      Word stack_need = arg0 & 0b1111111111;
      Word heap_need = (arg0 >> 10) & 0b1111111111;
      Word live = (arg0 >> 20) & 0b1111111111;
      pc0 += BYTES_PER_WORD;
      E4LOG4("[%p] alloc need=%zu heap=%zu live=%zu\n",
             pc0, stack_need, heap_need, live);
    } break;

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
