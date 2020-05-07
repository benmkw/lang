// this compiler backend creates object files

use crate::{Compiler, Expr};
use cranelift::prelude::{settings, FunctionBuilderContext};
use cranelift_module::{DataContext, Module};
use cranelift_object::{ObjectBackend, ObjectBuilder};
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;

#[must_use]
pub fn interpret(expr: &Expr) -> i64 {
    let flag_builder = settings::builder();
    let isa_builder = cranelift_native::builder().unwrap();
    let isa = isa_builder.finish(settings::Flags::new(flag_builder));

    let name = vec![];
    // TODO instead of default libcall names a function could be passed here which resolves local function names
    // to function pointers and maybe loads dynamic libraries
    let builder = ObjectBuilder::new(isa, name, cranelift_module::default_libcall_names());
    let module = Module::new(builder);

    let mut obj: Compiler<ObjectBackend> = Compiler {
        builder_context: FunctionBuilderContext::new(),
        ctx: module.make_context(),
        data_ctx: DataContext::new(),
        module,
    };

    let _id = obj.translate(&[expr.clone()]);

    let code = obj.module.finish();
    let data = code.emit().unwrap();

    let name = "out.obj";
    let mut buffer = File::create(name).unwrap();
    buffer.write_all(&data).unwrap();

    Command::new("sh")
        .arg("-c")
        .arg(format!("ld64.lld {} -lsystem", name))
        .output()
        .unwrap();

    Command::new("sh")
        .arg("-c")
        .arg("./a.out")
        .status()
        .unwrap()
        .code()
        .unwrap() as i64
}
