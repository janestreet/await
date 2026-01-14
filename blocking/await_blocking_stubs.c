#include <limits.h>
#include <stdatomic.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <linux/futex.h>

#include "caml/version.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/signals.h"

#if !(defined CAML_RUNTIME_5) || (OCAML_VERSION_MAJOR <= 4)

// Inlined from their definitions in runtime5 for runtime4 compatibility, as these aren't
// defined in runtime4

Caml_inline value Val_ptr(void *p) {
  CAMLassert(((value)p & 1) == 0);
  return (value)p + 1;
}
Caml_inline void *Ptr_val(value val) {
  CAMLassert(val & 1);
  return (void *)(val - 1);
}

#endif

// We use a futex per thread to avoid contention / spurious wakeups.  However, a single
// futex would be sufficient for correctness.  In other words, you can remove the
// [__thread] specifier and everything will still work, but will likely perform worse.
static __thread _Atomic uint32_t futex_for_thread = 0;

#define Futex_val(v_futex) ((_Atomic uint32_t *)(Ptr_val(v_futex)))

// See [await_blocking.ml] for documentation for these functions.

CAMLprim value await_blocking_futex_get(value v_unit /* immediate */) {
  CAMLparam0();
  (void)v_unit;
  _Atomic uint32_t *futex = &futex_for_thread;
  CAMLreturn(Val_ptr(futex));
}

CAMLprim value await_blocking_futex_count(value v_futex /* immediate */) {
  CAMLparam0();

  _Atomic uint32_t *futex = Futex_val(v_futex);

  CAMLreturn(Val_int(atomic_load_explicit(futex, memory_order_acquire)));
}

CAMLprim value await_blocking_futex_wait(value v_futex /* immediate */,
                                         value v_count /* immediate */) {
  CAMLparam0();

  _Atomic uint32_t *futex = Futex_val(v_futex);

  caml_enter_blocking_section();
  syscall(SYS_futex, futex, FUTEX_WAIT_PRIVATE, Int_val(v_count), NULL);
  caml_leave_blocking_section();

  CAMLreturn(Val_int(atomic_load_explicit(futex, memory_order_acquire)));
}

CAMLprim value await_blocking_futex_signal(value v_futex /* immediate */) {
  CAMLparam0();

  _Atomic uint32_t *futex = Futex_val(v_futex);

  // The futex counter could theoretically wrap around, but that should be practically
  // impossible, because an increment is done at most once per allocated trigger.
  atomic_fetch_add_explicit(futex, 1, memory_order_relaxed);

  // When using a futex per thread there should be at most one thread blocked on a futex,
  // but we broadcast anyway, because it should not slow things down and allows the
  // approach to work even with a shared futex.
  syscall(SYS_futex, futex, FUTEX_WAKE_PRIVATE, INT_MAX /* broadcast */);

  CAMLreturn(Val_unit);
}
