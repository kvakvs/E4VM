#include "e4rt/scheduler.h"
#include "e4rt/process.h"

namespace e4 {

Term Scheduler::make_pid() {
    auto t = Term::make_short_pid(pid_counter_++);
    // TODO: implement wrap when word counter overflows
    E4ASSERT(processes_.find(t) == nullptr);
    return t;
}

void Scheduler::register_proc(Process* p) {
    processes_.insert(p->self(), p);
}

Process* Scheduler::next() {
    return nullptr;
}

} // ns e4
