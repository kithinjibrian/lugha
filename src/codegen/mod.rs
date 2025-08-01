pub mod jit;
pub mod aot;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{PointerValue},
};

use crate::parser::ast::{BinaryOp, Expr, Stmt};

pub struct Codegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub variables: std::collections::HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            variables: std::collections::HashMap::new(),
        }
    }

    pub fn compile(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.codegen_stmt(stmt);
        }
    }

    fn codegen_stmt(&mut self, stmt: &Stmt) -> Option<inkwell::values::IntValue<'ctx>> {
        match stmt {
            Stmt::Function { name, params, body } => {
                let i64_type = self.context.i64_type();
                let fn_type = i64_type.fn_type(
                    &vec![i64_type.into(); params.len()],
                    false, // change to true if the function is variadic
                );

                let function = self.module.add_function(name, fn_type, None);
                let entry = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(entry);

                let old_variables = std::mem::take(&mut self.variables);

                for (i, param_name) in params.iter().enumerate() {
                    let param = function.get_nth_param(i as u32).unwrap().into_int_value();
                    let ptr = self
                                .builder
                                .build_alloca(i64_type, param_name)
                                .expect("Failed to allocate parameter");
                    self.builder.build_store(ptr, param).expect("Failed to store parameter");
                    self.variables.insert(param_name.clone(), ptr);
                }

                if let Expr::Block(bd) = body {
                    for stmt in bd {
                        self.codegen_stmt(stmt);
                    }
                }
 

                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_return(Some(&i64_type.const_zero()))
                        .expect("Failed to generate implicit return");
                }

                self.variables = old_variables;

                None
            }
            Stmt::Let { name, expr } => {
                let ptr = self
                    .builder
                    .build_alloca(self.context.i64_type(), name)
                    .expect("Failed to allocate local variable");

                if let Some(exp) = expr {
                    let _exp = self.codegen_expr(exp)?;

                    self.builder
                        .build_store(ptr, _exp)
                        .expect("LLVM failed to store value");
                }

                self.variables.insert(name.clone(), ptr);
                None
            }
            Stmt::ExprStmt(expr) => {
                let _ = self.codegen_expr(expr);
                None
            }
            Stmt::Return(expr_opt) => {
                let val = match expr_opt {
                    Some(expr) => self.codegen_expr(expr)?,
                    None => self.context.i64_type().const_zero(),
                };
                self.builder
                    .build_return(Some(&val))
                    .expect("LLVM failed to return");
                None
            },
            _ => {
                eprintln!("Unsupported statement during codegen: {:?}", stmt);
                None
            }
        }
    }

    fn codegen_expr(&mut self, expr: &Expr) -> Option<inkwell::values::IntValue<'ctx>> {
        let i64_type = self.context.i64_type();

        match expr {
            Expr::Integer(n) => Some(i64_type.const_int(*n as u64, true)),
            Expr::Identifier(name) => {
                let ptr = self.variables.get(name)?;
                Some(
                    self.builder
                        .build_load(*ptr, name)
                        .expect("Failed to load variable")
                        .into_int_value(),
                )

            }
            Expr::Binary { left, op, right } => {
                let lhs = self.codegen_expr(left)?;
                let rhs = self.codegen_expr(right)?;
                match op {
                    BinaryOp::Add => Some(
                            self.builder
                                .build_int_add(lhs, rhs, "addtmp")
                                .expect("LLVM failed to build add"),
                            ),
                    BinaryOp::Subtract => Some(
                            self.builder
                                .build_int_sub(lhs, rhs, "subtmp")
                                .expect("LLVM failed to build sub")
                            ),
                    BinaryOp::Multiply => Some(
                            self.builder
                                .build_int_mul(lhs, rhs, "multmp")
                                .expect("LLVM failed to build mult")
                            ),
                    _ => None,
                }
            },
            _ => {
                eprintln!("Unsupported expression during codegen: {:?}", expr);
                None
            }
        }
    }
}
