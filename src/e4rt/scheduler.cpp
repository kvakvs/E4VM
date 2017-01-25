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

void Scheduler::schedule(Process* p) {
    E4ASSERT(not runq_normal_.contains_val(p));
    E4ASSERT(not runq_high_.contains_val(p));
    switch (p->priority()) {
        case ProcessPriority::Normal:
            runq_normal_.push_back(p);
            break;
        case ProcessPriority::High:
            runq_high_.push_back(p);
            break;
    }
}

Process* Scheduler::next() {
    Process* n = next_high();
    if (not n) {
        n = next_normal();
    }

    return n;
}

Process* Scheduler::next_normal() {
    auto qsize = runq_normal_.size();
    if (not qsize) {
        return nullptr;
    }
    q_ptr_normal_ = (q_ptr_normal_ + 1) % qsize;
    return runq_normal_[q_ptr_normal_];
}

Process* Scheduler::next_high() {
    high_advantage_ = (high_advantage_ + 1) % SCHED_HIGH_ADVANTAGE;
    if (high_advantage_ == 0) {
        return nullptr; // force null return so that normal processes can run
    }

    auto qsize = runq_high_.size();
    if (not qsize) {
        return nullptr;
    }

    q_ptr_high_ = (q_ptr_high_ + 1) % qsize;
    return runq_normal_[q_ptr_high_];
}

} // ns e4
