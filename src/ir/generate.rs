use std::collections::{HashMap, LinkedList};

use crate::{
    error::CompErr,
    grammar::{BinaryOperation, Node, NodeInner},
    types::Type,
    utils::pad_bytes,
};

use super::*;

pub struct IrSnippet {
    pub insts: Vec<Inst>,
    pub assigns: Option<Value>,
}

impl IrSnippet {
    pub fn debug_print(&self) {
        readable::write_readable(std::io::stdout(), self).unwrap();

        // for inst in &self.insts {
        //     println!("{:?}", inst);
        // }
    }
}

pub struct GenContext {
    scopes: Vec<Scope>,
    stack_frames: Vec<StackFrame>,
}

impl GenContext {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
            stack_frames: vec![StackFrame::default()],
        }
    }

    fn read_var(&self, ident: &str) -> Option<StorageItem> {
        for scope in self.scopes.iter().rev() {
            if let Some(vreg) = scope.variables.get(ident) {
                return Some(vreg.clone());
            }
        }

        None
    }

    fn declare(&mut self, ident: &str, type_: Type) -> StorageItem {
        let item = StorageItem {
            type_,
            vreg: new_id(),
        };

        let scope = self.scopes.iter_mut().last().unwrap();
        scope.variables.insert(ident.to_string(), item.clone());

        let frame = self.stack_frames.iter_mut().last().unwrap();
        frame.variables.insert(ident.to_string(), item.clone());

        item
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define_type(&mut self, ident: &str, t: Type) {
        println!("{} := {:?}", ident, t);
        let scope = self.scopes.iter_mut().last().unwrap();
        scope.types.insert(ident.to_string(), t);
    }
}

#[derive(Default)]
struct Scope {
    variables: HashMap<String, StorageItem>,
    types: HashMap<String, Type>,
}

#[derive(Default)]
struct StackFrame {
    variables: HashMap<String, StorageItem>,
}

#[derive(Debug, Clone)]
struct StorageItem {
    vreg: VregId,
    type_: Type,
}

impl StorageItem {
    pub fn as_value(&self) -> Value {
        Value {
            type_: self.type_.clone(),
            inner: ValueInner::Vreg(self.vreg),
        }
    }
}

pub fn generate(ast: Node, ctx: &mut GenContext) -> Result<IrSnippet, CompErr> {
    let mut insts = Vec::new();
    let assigns;

    match ast.inner {
        NodeInner::StatementList(statements) => {
            for s in statements {
                let s = generate(s, ctx)?;
                insts.extend(s.insts);
            }

            // TODO: COPY last and assign
            assigns = None;
        }
        NodeInner::Indentifier(ident) => {
            let Some(var) = ctx.read_var(&ident) else {
                return CompErr::new_general(
                    format!("Undeclared identifier `{}`", ident),
                    ast.range,
                );
            };

            let Some(t) = var.type_.clone().deref() else {
                return CompErr::new_general(
                    format!("Cannot derefernce {:?}", var.type_),
                    ast.range,
                );
            };

            assigns = Some(new_vreg(t));

            insts.push(Inst {
                assigns: assigns.clone(),
                inner: InstInner::Load(var.as_value()),
            });
        }
        NodeInner::Call(fn_name, args) => {
            let mut arg_values = Vec::with_capacity(args.len());

            for arg in args {
                let arg = generate(arg, ctx)?;

                let Some(val) = arg.assigns else {
                    return CompErr::new_general(
                        format!("Argument was not a valid expression",),
                        ast.range,
                    );
                };

                insts.extend(arg.insts);

                arg_values.push(val);
            }

            assigns = Some(new_vreg(Type::Auto));

            insts.push(Inst {
                assigns: assigns.clone(),
                inner: InstInner::Call {
                    fn_name,
                    args: arg_values,
                },
            });
        }
        NodeInner::IfThenElse {
            condition,
            then_branch,
            else_branch,
        } => {
            let cond = generate(*condition, ctx)?;
            let then = generate(*then_branch, ctx)?;

            let Some(cond_val) = cond.assigns else {
                return CompErr::new_general(
                    format!("Condition was not a valid expression",),
                    ast.range,
                );
            };

            if cond_val.type_ != Type::Boolean {
                return CompErr::new_general("Condition must be of type bool", ast.range);
            }

            insts.extend(cond.insts);

            let then_branch_label = new_id();
            let end_label = new_id();

            insts.push(Inst {
                assigns: None,
                inner: InstInner::Jz(then_branch_label, cond_val),
            });

            insts.extend(then.insts);

            if else_branch.is_some() {
                insts.push(Inst {
                    assigns: None,
                    inner: InstInner::Jmp(end_label),
                });
            }

            insts.push(Inst {
                assigns: None,
                inner: InstInner::Label(then_branch_label),
            });

            if let Some(eb) = else_branch {
                let else_ = generate(*eb, ctx)?;

                insts.extend(else_.insts);

                insts.push(Inst {
                    assigns: None,
                    inner: InstInner::Label(end_label),
                });
            };

            assigns = None;
        }
        NodeInner::BinaryOperation(BinaryOperation::Declare, lhs, rhs)
        | NodeInner::BinaryOperation(BinaryOperation::DeclareConst, lhs, rhs) => {
            let NodeInner::Indentifier(ident) = lhs.inner else {
                return CompErr::new_general(
                    format!("Expected identifier; got {:?}", lhs.inner),
                    lhs.range,
                );
            };

            if let NodeInner::Type(t) = rhs.inner {
                ctx.define_type(&ident, t);
            } else {
                let rhs = generate(*rhs, ctx)?;
                let Some(rhs_val) = rhs.assigns else {
                    return CompErr::new_general(
                        format!("Right hand side of assignment was not a valid expression",),
                        ast.range,
                    );
                };
                insts.extend(rhs.insts);

                let vreg = ctx.declare(&ident, Type::Ptr(Box::new(rhs_val.type_.clone())));

                insts.push(Inst {
                    assigns: None,
                    inner: InstInner::Store(vreg.as_value(), rhs_val),
                });
            }

            assigns = None;
            // rhs_val
        }
        NodeInner::BinaryOperation(op, lhs, rhs) => {
            let lhs = generate(*lhs, ctx)?;
            let rhs = generate(*rhs, ctx)?;
            let Some(lhs_val) = lhs.assigns else {
                return CompErr::new_general(
                    format!(
                        "Left hand side of {} was not a valid expression",
                        op.stringify()
                    ),
                    ast.range,
                );
            };
            let Some(rhs_val) = rhs.assigns else {
                return CompErr::new_general(
                    format!(
                        "Right hand side of {} was not a valid expression",
                        op.stringify()
                    ),
                    ast.range,
                );
            };
            insts.extend(lhs.insts);
            insts.extend(rhs.insts);

            if rhs_val.type_ != lhs_val.type_ {
                return CompErr::new_general(
                    format!(
                        "Could not apply operation {} to types TODO and TODO",
                        op.stringify()
                    ),
                    ast.range,
                );
            }

            assigns = Some(new_vreg(rhs_val.type_.clone()));

            insts.push(Inst {
                assigns: assigns.clone(),
                inner: InstInner::BinOp {
                    op,
                    lhs: lhs_val,
                    rhs: rhs_val,
                },
            });
        }
        NodeInner::DoubleLiteral(value) => {
            assigns = Some(Value {
                type_: Type::F64,
                inner: ValueInner::Immediate(pad_bytes(value.to_le_bytes())),
            });
        }
        NodeInner::BoolLiteral(value) => {
            assigns = Some(Value {
                type_: Type::Boolean,
                inner: ValueInner::Immediate(pad_bytes(if value { [1; 1] } else { [0; 1] })),
            });
        }
        NodeInner::Scope(body) => {
            ctx.push_scope();
            let body = generate(*body, ctx)?;
            ctx.pop_scope();

            insts.extend(body.insts);

            assigns = body.assigns;
        }
        NodeInner::While { condition, body } => {
            let start_label = new_id();
            let end_label = new_id();

            insts.push(Inst {
                assigns: None,
                inner: InstInner::Label(start_label),
            });

            ctx.push_scope();

            let cond = generate(*condition, ctx)?;
            insts.extend(cond.insts);

            let Some(cond_val) = cond.assigns else {
                return CompErr::new_general(
                    "Condition of while loop must be of type bool",
                    ast.range,
                );
            };

            insts.push(Inst {
                assigns: None,
                inner: InstInner::Jz(end_label, cond_val),
            });

            let body = generate(*body, ctx)?;
            insts.extend(body.insts);

            ctx.pop_scope();

            insts.push(Inst {
                assigns: None,
                inner: InstInner::Jmp(start_label),
            });

            insts.push(Inst {
                assigns: None,
                inner: InstInner::Label(end_label),
            });

            assigns = None;
        }
        NodeInner::For { var, range, body } => {
            let start_label = new_id();
            let end_label = new_id();

            insts.push(Inst {
                assigns: None,
                inner: InstInner::Label(start_label),
            });

            ctx.push_scope();

            let it = if let Some(v) = var {
                let NodeInner::Indentifier(ident) = v.inner else {
                    return CompErr::new_general(
                        format!("Expected identifier; got {:?}", v.inner),
                        v.range,
                    );
                };
                ident
            } else {
                "it".to_string()
            };

            let it = ctx.declare(&it, Type::Void);

            let range = generate(*range, ctx)?;
            insts.extend(range.insts);

            let Some(cond_val) = range.assigns else {
                return CompErr::new_general("for must contain range", ast.range);
            };

            // TODO

            let body = generate(*body, ctx)?;
            insts.extend(body.insts);

            ctx.pop_scope();

            insts.push(Inst {
                assigns: None,
                inner: InstInner::Jmp(start_label),
            });

            insts.push(Inst {
                assigns: None,
                inner: InstInner::Label(end_label),
            });

            assigns = None;
        }
        _ => panic!("{:?}", ast),
    }

    Ok(IrSnippet { insts, assigns })
}
