#pragma once

#include "e4platf/types.h"

#include "e4rt/term.h"

namespace e4 {

class Process;
constexpr Word SCHED_HIGH_ADVANTAGE = 3;

class Scheduler {
 private:
  // Queue pointers point to the last process scheduled (mod queue size)
  Word q_ptr_normal_ = 0;
  Word q_ptr_high_ = 0;
  PODVector<Process*> runq_normal_;
  PODVector<Process*> runq_high_;
  // One normal prio process is scheduled for every SCHED_HIGH_ADVANTAGE
  // cycles of high prio processes
  Word high_advantage_ = 0;

  Word pid_counter_ = 0;
  Map<Term, Process*> processes_;

 public:
  Scheduler() {}

  Term make_pid();
  void register_proc(Process* p);
  void schedule(Process* p);

  // Take next process from the runqueue with respect to priorities
  Process* next();

 private:
  Process* next_normal();
  Process* next_high();
};

}  // ns e4
