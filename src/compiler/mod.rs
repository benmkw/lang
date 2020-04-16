// adapted from https://github.com/bytecodealliance/simplejit-demo/blob/master/src/jit.rs

// the main stuff of converting the language to IR happens here

use crate::Expr;
use cranelift::prelude::*;
use cranelift_codegen::binemit::NullTrapSink;
use cranelift_module::{DataContext, Linkage, Module};
use std::collections::HashMap;

pub mod jit;
pub mod obj;

pub struct Compiler<Backend>
where
    Backend: cranelift_module::Backend,
{
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    pub builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    pub ctx: codegen::Context,

    /// The data context, which is to data objects what `ctx` is to functions.
    pub data_ctx: DataContext,

    /// The module, with the backend, which manages the functions
    pub module: Module<Backend>,
}

impl<Backend> Compiler<Backend>
where
    Backend: cranelift_module::Backend,
{
    // Translate from toy-language AST nodes into Cranelift IR.
    pub fn translate(&mut self, stmts: &[Expr]) -> cranelift_module::FuncId {
        let name = "main".to_string(); // otherwise linker complains
        let params: Vec<String> = vec![];
        let the_return = "returnName".to_string();

        // Our toy language currently only supports I64 values, though Cranelift
        // supports other types.
        let int = self.module.target_config().pointer_type();

        debug_assert_eq!(&params.len(), &0_usize);
        for _p in &params {
            self.ctx.func.signature.params.push(AbiParam::new(int));
        }

        self.ctx.func.signature.returns.push(AbiParam::new(int));

        // builder to build a function.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // entry block, to start emitting code in.
        let entry_block = builder.create_block();

        // Since this is the entry block, add block parameters corresponding to
        // the function's parameters.
        builder.append_block_params_for_function_params(entry_block);

        // Tell the builder to emit code in this block.
        builder.switch_to_block(entry_block);

        // And, tell the builder that this block will have no further
        // predecessors. Since it's the entry block, it won't have any predecessors.
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
        let return_var = &variables[&the_return];

        builder.def_var(*return_var, ret_value);

        // Set up the return variable of the function. Above, we declared a
        // variable to hold the return value. Here, we just do a use of that
        // variable.

        let return_variable = &variables[&the_return];
        let return_value = builder.use_var(*return_variable);

        // Emit the return instruction.
        builder.ins().return_(&[return_value]);

        // Tell the builder we're done with this function.
        builder.finalize();

        // FROM compile
        // Then, translate the AST nodes into Cranelift IR.
        // self.translate(params, the_return, vec![stmts.clone()])
        //     .unwrap();

        // Functions must be declared before they are defined or called
        let id = self
            .module
            .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
            .unwrap();

        // Define the function to simplejit. This finishes compilation, although
        // there may be outstanding relocations to perform. Currently, simplejit
        // cannot finish relocations until all functions to be called are
        // defined. For this toy demo for now, we'll just finalize the function
        // below.
        self.module
            .define_function(id, &mut self.ctx, &mut NullTrapSink {})
            .unwrap();

        // Now that compilation is finished, we can clear out the context state.
        self.module.clear_context(&mut self.ctx);

        // Finalize the functions which we just defined, which resolves any
        // outstanding relocations (patching in addresses, now that they're
        // available).
        self.module.finalize_definitions();
        id
    }
}

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
pub struct FunctionTranslator<'a, Backend>
where
    Backend: cranelift_module::Backend,
{
    pub int: types::Type,
    pub builder: FunctionBuilder<'a>,
    pub variables: HashMap<String, Variable>,
    pub module: &'a mut Module<Backend>,
}

impl<'a, Backend> FunctionTranslator<'a, Backend>
where
    Backend: cranelift_module::Backend,
{
    /// When you write out instructions in Cranelift, you get back `Value`s. You
    /// can then use these references in other instructions.
    pub fn translate_expr(&mut self, expr: Expr) -> Value {
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
            Expr::Negate(val) => {
                let res = self.translate_expr(*val);
                self.builder.ins().ineg(res)
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
            Expr::Identifier(_name) => {
                panic!();
                // let name = name.get_string();
                // // `use_var` is used to read the value of a variable.
                // let variable = self.variables.get(&name).expect("variable not defined");
                // self.builder.use_var(*variable)
            }
            Expr::Block(..) | Expr::Exp(..) | Expr::Assign(..) => panic!(),
        }
    }
}

pub fn declare_variables(
    int: types::Type,
    builder: &mut FunctionBuilder,
    params: &[String],
    the_return: &str,
    _stmts: &[Expr],
    entry_block: Block,
) -> HashMap<String, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;

    for (i, name) in params.iter().enumerate() {
        let value = builder.block_params(entry_block)[i];
        let variable = declare_variable(int, builder, &mut variables, &mut index, name);
        builder.def_var(variable, value);
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
