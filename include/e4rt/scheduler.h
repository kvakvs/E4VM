#pragma once

#include "e4platf/types.h"

#include "e4rt/term.h"

namespace e4 {

class Process;

class Scheduler {
private:
    Vector<Process*> runq_;
    Word pid_counter_ = 0;
    Map<Term, Process*> processes_;

public:
    Term make_pid();
    void register_proc(Process* p);

    // Take next process from the runqueue with respect to priorities
    Process* next();
};

} // ns e4
