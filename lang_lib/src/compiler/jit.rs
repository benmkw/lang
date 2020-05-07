// this compiler backend emits code into memory

use crate::{Compiler, Expr};
use cranelift::prelude::*;
use cranelift_module::{DataContext, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
use std::mem;

#[must_use]
pub fn interpret(expr: &Expr) -> i64 {
    // TODO instead of default libcall names a function could be passed here which resolves local function names
    // to function pointers and maybe loads dynamic libraries
    let builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
    let module = Module::new(builder);

    let mut jit: Compiler<SimpleJITBackend> = Compiler {
        builder_context: FunctionBuilderContext::new(),
        ctx: module.make_context(),
        data_ctx: DataContext::new(),
        module,
    };

    let id = jit.translate(&[expr.clone()]);

    // a raw pointer to machine code
    let function = jit.module.get_finalized_function(id);

    // Cast the raw pointer to a typed function pointer.
    // have to trust that the generated code is safe to be called
    let function = unsafe { mem::transmute::<_, fn() -> isize>(function) };
    let res = function() as i64;
    dbg!(res)
}
