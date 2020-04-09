// adapted from https://github.com/bytecodealliance/simplejit-demo/blob/master/src/jit.rs

use cranelift::prelude::*;
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
use std::collections::HashMap;
use std::mem;

use crate::Expr;

#[must_use]
pub fn interpret_ast_jit(expr: &Expr) -> i64 {
    let mut jit = JIT::new();

    // a raw pointer to machine code
    let foo = jit.compile(&expr).unwrap();

    // Cast the raw pointer to a typed function pointer.
    // have to trust that the generated code is safe to be called
    let foo = unsafe { mem::transmute::<_, fn() -> isize>(foo) };
    let res = foo() as i64;
    dbg!(res);
    res
}

pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data context, which is to data objects what `ctx` is to functions.
    data_ctx: DataContext,

    /// The module, with the simplejit backend, which manages the JIT'd
    /// functions.
    module: Module<SimpleJITBackend>,
}

impl JIT {
    pub fn new() -> Self {
        let builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        let module = Module::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }

    /// Compile a string in the toy language into machine code.
    pub fn compile(&mut self, stmts: &Expr) -> Result<*const u8, String> {
        let name = "FunctioNameFoo".to_string();
        let params: Vec<String> = vec![];
        let the_return = "returnName".to_string();

        // Then, translate the AST nodes into Cranelift IR.
        self.translate(params, the_return, vec![stmts.clone()])
            .unwrap();

        // Next, declare the function to simplejit. Functions must be declared
        // before they can be called, or defined.
        let id = self
            .module
            .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
            .unwrap();

        // Define the function to simplejit. This finishes compilation, although
        // there may be outstanding relocations to perform. Currently, simplejit
        // cannot finish relocations until all functions to be called are
        // defined. For this toy demo for now, we'll just finalize the function
        // below.
        self.module.define_function(id, &mut self.ctx).unwrap();

        // Now that compilation is finished, we can clear out the context state.
        self.module.clear_context(&mut self.ctx);

        // Finalize the functions which we just defined, which resolves any
        // outstanding relocations (patching in addresses, now that they're
        // available).
        self.module.finalize_definitions();

        // We can now retrieve a pointer to the machine code.
        let code = self.module.get_finalized_function(id);

        Ok(code)
    }

    // Translate from toy-language AST nodes into Cranelift IR.
    fn translate(
        &mut self,
        params: Vec<String>,
        the_return: String,
        stmts: Vec<Expr>,
    ) -> Result<(), String> {
        // Our toy language currently only supports I64 values, though Cranelift
        // supports other types.
        let int = self.module.target_config().pointer_type();

        debug_assert_eq!(&params.len(), &0_usize);
        for _p in &params {
            self.ctx.func.signature.params.push(AbiParam::new(int));
        }

        // Our toy language currently only supports one return value, though
        // Cranelift is designed to support more.
        self.ctx.func.signature.returns.push(AbiParam::new(int));

        // Create the builder to builder a function.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();

        // Since this is the entry block, add block parameters corresponding to
        // the function's parameters.
        builder.append_block_params_for_function_params(entry_block);

        // Tell the builder to emit code in this block.
        builder.switch_to_block(entry_block);

        // And, tell the builder that this block will have no further
        // predecessors. Since it's the entry block, it won't have any
        // predecessors.
        builder.seal_block(entry_block);

        // The toy language allows variables to be declared implicitly.
        // Walk the AST and declare all implicitly-declared variables.
        let variables =
            declare_variables(int, &mut builder, &params, &the_return, &stmts, entry_block);

        // Now translate the statements of the function body.
        let mut trans = FunctionTranslator {
            int,
            builder,
            variables,
            module: &mut self.module,
        };
        // for expr in stmts {
        //     trans.translate_expr(expr);
        // }

        debug_assert_eq!(&stmts.len(), &1_usize);
        let ret_value = trans.translate_expr(stmts[0].clone());

        let mut builder = trans.builder;
        let variables = trans.variables;

        // `def_var` is used to write the value of a variable. Note that
        // variables can have multiple definitions. Cranelift will
        // convert them into SSA form for itself automatically.
        let return_var = variables.get(&the_return).unwrap();

        builder.def_var(*return_var, ret_value);

        // Set up the return variable of the function. Above, we declared a
        // variable to hold the return value. Here, we just do a use of that
        // variable.

        let return_variable = variables.get(&the_return).unwrap();
        let return_value = builder.use_var(*return_variable);

        // Emit the return instruction.
        builder.ins().return_(&[return_value]);

        // Tell the builder we're done with this function.
        builder.finalize();
        Ok(())
    }
}

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
struct FunctionTranslator<'a> {
    int: types::Type,
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut Module<SimpleJITBackend>,
}

impl<'a> FunctionTranslator<'a> {
    /// When you write out instructions in Cranelift, you get back `Value`s. You
    /// can then use these references in other instructions.
    fn translate_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Number(literal) => self.builder.ins().iconst(self.int, literal),

            Expr::Add(box (lhs, rhs)) => {
                let lhs = self.translate_expr(lhs);
                let rhs = self.translate_expr(rhs);
                self.builder.ins().iadd(lhs, rhs)
            }
            Expr::Sub(box (lhs, rhs)) => {
                let lhs = self.translate_expr(lhs);
                let rhs = self.translate_expr(rhs);
                self.builder.ins().isub(lhs, rhs)
            }
            Expr::Mult(box (lhs, rhs)) => {
                let lhs = self.translate_expr(lhs);
                let rhs = self.translate_expr(rhs);
                self.builder.ins().imul(lhs, rhs)
            }
            Expr::Div(box (lhs, rhs)) => {
                let lhs = self.translate_expr(lhs);
                let rhs = self.translate_expr(rhs);
                self.builder.ins().udiv(lhs, rhs)
            }
            Expr::Identifier(name) => {
                panic!();
                // let name = name.get_string();
                // // `use_var` is used to read the value of a variable.
                // let variable = self.variables.get(&name).expect("variable not defined");
                // self.builder.use_var(*variable)
            }
            _ => panic!(),
        }
    }
}

fn declare_variables(
    int: types::Type,
    builder: &mut FunctionBuilder,
    params: &[String],
    the_return: &str,
    stmts: &[Expr],
    entry_block: Block,
) -> HashMap<String, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;

    for (i, name) in params.iter().enumerate() {
        let val = builder.block_params(entry_block)[i];
        let var = declare_variable(int, builder, &mut variables, &mut index, name);
        builder.def_var(var, val);
    }
    let zero = builder.ins().iconst(int, 0);
    let return_variable = declare_variable(int, builder, &mut variables, &mut index, the_return);
    builder.def_var(return_variable, zero);
    // for expr in stmts {
    //     declare_variables_in_stmt(int, builder, &mut variables, &mut index, expr);
    // }

    variables
}

// /// Recursively descend through the AST, translating all implicit
// /// variable declarations.
// fn declare_variables_in_stmt(
//     int: types::Type,
//     builder: &mut FunctionBuilder,
//     variables: &mut HashMap<String, Variable>,
//     index: &mut usize,
//     expr: &Expr,
// ) {
//     // match *expr {
//     // Expr::Assign(ref name, _) => {
//     //     declare_variable(int, builder, variables, index, name);
//     // }
//     // _ => (),
//     // }
// }

/// Declare a single variable declaration.
fn declare_variable(
    int: types::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, Variable>,
    index: &mut usize,
    name: &str,
) -> Variable {
    let var = Variable::new(*index);
    if !variables.contains_key(name) {
        variables.insert(name.into(), var);
        builder.declare_var(var, int);
        *index += 1;
    }
    var
}
