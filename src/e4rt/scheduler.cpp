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
    schedule(p);
}

Process* Scheduler::next() {
    return nullptr;
}

//static int compare_ptr_voidp(const void* pp1, const void* pp2) {
//    const void* p1 = *static_cast<const void**>(pp1);
//    const void* p2 = *static_cast<const void**>(pp2);
//    return p1 == p2 ? 0 : (p1 < p2 ? -1 : 1);
//}
void Scheduler::schedule(Process* p) {
    E4ASSERT(not runq_normal_.contains_val(p));
    E4ASSERT(not runq_high_.contains_val(p));
}

} // ns e4
