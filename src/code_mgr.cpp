#include "gluon/debug.h"
#include "gluon/code_mgr.h"
#include "gluon/fs.h"
#include "gluon/vm.h"

namespace gluon {

    void CodeManager::from_file(VM &vm, const char *fn) {
#if GLUON_FEATURE_FS
        File f(fn, "rb");
        Gluon_assert(f.is_open());

        UniqueBox<Uint8> data = f.read_everything();
        auto m = std::make_unique<Module>(vm, data.view());
#else
        assert(false);
        return;
#endif
    }

} // ns gluon
