use std::collections::HashMap;

use crate::{
    error::{CompErr, Range},
    grammar::{BinaryOperation, Node, NodeInner, UnaryOperation},
    types::{bin_op_coerce, Type, UnknownTypeId},
};

use super::*;

#[derive(Debug, Clone)]
pub struct IrSnippet {
    pub insts: Vec<Inst>,
    pub assigns: Option<Value>,
}

impl IrSnippet {
    pub fn debug_print(&self) {
        readable::write_readable(&mut std::io::stdout(), self).unwrap();

        // for inst in &self.insts {
        //     println!("{:?}", inst);
        // }
    }
}

pub struct GenContext {
    scopes: Vec<Scope>,
    stack_frames: Vec<StackFrame>,
    pub auto_map: HashMap<UnknownTypeId, Type>,
}

impl GenContext {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
            stack_frames: vec![StackFrame::default()],
            auto_map: HashMap::new(),
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

pub fn generate(ast: Node, ctx: &mut GenContext, underef: bool) -> Result<IrSnippet, CompErr> {
    let mut insts = Vec::new();
    let mut assigns = None;

    match ast.inner {
        NodeInner::StatementList(statements) => {
            for s in statements {
                let s = generate(s, ctx, false)?;
                insts.extend(s.insts);
            }

            // TODO: COPY last and assign
        }
        NodeInner::Indentifier(ident) => {
            let Some(var) = ctx.read_var(&ident) else {
                return CompErr::new_general(
                    format!("Undeclared identifier `{}`", ident),
                    ast.range,
                );
            };

            if underef {
                assigns = Some(var.as_value());
            } else {
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
        }
        NodeInner::Function { name, body } => {
            let body = generate(*body, ctx, false)?;

            insts.push(Inst {
                assigns: assigns.clone(),
                inner: InstInner::Function { name, body },
            });
        }
        NodeInner::Call(fn_name, args) => {
            let mut arg_values = Vec::with_capacity(args.len());

            for arg in args {
                let arg = generate(arg, ctx, false)?;

                let Some(val) = arg.assigns else {
                    return CompErr::new_general(
                        format!("Argument was not a valid expression",),
                        ast.range,
                    );
                };

                insts.extend(arg.insts);

                arg_values.push(val);
            }

            assigns = Some(new_vreg(Type::new_auto()));

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
            let cond = generate(*condition, ctx, false)?;
            let then = generate(*then_branch, ctx, false)?;

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
                let else_ = generate(*eb, ctx, false)?;

                insts.extend(else_.insts);

                insts.push(Inst {
                    assigns: None,
                    inner: InstInner::Label(end_label),
                });
            };
        }
        NodeInner::BinaryOperation(BinaryOperation::Declare, lhs, rhs)
        | NodeInner::BinaryOperation(BinaryOperation::DeclareConst, lhs, rhs) => {
            let NodeInner::Indentifier(ident) = lhs.inner else {
                return CompErr::new_general(
                    format!("Expected identifier; got {:?}", lhs.inner),
                    lhs.range,
                );
            };

            decleration(&mut insts, ctx, ast.range, &ident, None, rhs)?;

            // rhs_val
        }
        NodeInner::TypedDeclare { name, type_, value } => {
            decleration(&mut insts, ctx, ast.range, &name, Some(type_), value)?;

            // rhs_val
        }
        NodeInner::BinaryOperation(BinaryOperation::Assign, lhs, rhs) => {
            let rhs = generate(*rhs, ctx, false)?;
            let Some(rhs_val) = rhs.assigns else {
                return CompErr::new_general(
                    "Right hand side of assignment was not a valid expression",
                    ast.range,
                );
            };
            insts.extend(rhs.insts);

            assign(ctx, &mut insts, lhs, rhs_val, ast.range)?;
        }
        NodeInner::BinaryOperation(BinaryOperation::AssignAnd(op), lhs, rhs) => {
            let val = perform_bin_op(ctx, &mut insts, *op, &*lhs, &*rhs, &ast.range)?;
            assign(ctx, &mut insts, lhs, val, ast.range)?;
        }
        NodeInner::BinaryOperation(op, lhs, rhs) => {
            assigns = Some(perform_bin_op(
                ctx, &mut insts, op, &*lhs, &*rhs, &ast.range,
            )?);
        }
        NodeInner::Literal(value) => {
            assigns = Some(value);
        }
        NodeInner::Scope(body) => {
            ctx.push_scope();
            let body = generate(*body, ctx, false)?;
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

            let cond = generate(*condition, ctx, false)?;
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

            let body = generate(*body, ctx, false)?;
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
        }
        NodeInner::UnaryOperation(UnaryOperation::AddressOf, value) => {
            let lhs = generate(*value, ctx, true)?;
            insts.extend(lhs.insts);
            assigns = lhs.assigns;
        }
        NodeInner::UnaryOperation(op, value) => {
            let lhs = generate(*value, ctx, false)?;
            let Some(lhs_val) = lhs.assigns else {
                return CompErr::new_general(
                    format!(
                        "Left hand side of {} was not a valid expression",
                        op.stringify()
                    ),
                    ast.range,
                );
            };
            insts.extend(lhs.insts);

            match op {
                crate::grammar::UnaryOperation::Negate => {
                    let t = lhs_val.type_.clone();

                    assigns = Some(new_vreg(t));

                    insts.push(Inst {
                        assigns: assigns.clone(),
                        inner: InstInner::BinOp {
                            op: BinaryOperation::Mul,
                            lhs: lhs_val,
                            rhs: Value::int_immediate(-1),
                        },
                    });
                }
                crate::grammar::UnaryOperation::Not => {
                    let t = lhs_val.type_.clone();

                    assigns = Some(new_vreg(t));

                    insts.push(Inst {
                        assigns: assigns.clone(),
                        inner: InstInner::BinOp {
                            op: BinaryOperation::Xor,
                            lhs: lhs_val,
                            rhs: Value::int_immediate(1),
                        },
                    });
                }
                crate::grammar::UnaryOperation::Deref => {
                    let Some(t) = lhs_val.type_.clone().deref() else {
                        return CompErr::new_general(
                            format!("Could not dereference {:?}", lhs_val.type_),
                            ast.range,
                        );
                    };

                    assigns = Some(new_vreg(t));

                    insts.push(Inst {
                        assigns: assigns.clone(),
                        inner: InstInner::Load(lhs_val),
                    });
                }
                crate::grammar::UnaryOperation::AddressOf => unreachable!(),
            }
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

            let range = generate(*range, ctx, false)?;
            insts.extend(range.insts);

            let Some(cond_val) = range.assigns else {
                return CompErr::new_general("for must contain range", ast.range);
            };

            // TODO

            let body = generate(*body, ctx, false)?;
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
        }
        NodeInner::Return(value) => {
            let rhs = generate(*value, ctx, false)?;
            let Some(rhs_value) = rhs.assigns else {
                return CompErr::new_general("Return value was not a valid expression", ast.range);
            };
            insts.extend(rhs.insts);

            // // TODO: coerce with return type of function
            // let Some(t) = bin_op_coerce(&lhs_val.type_, &rhs_val.type_) else {
            //     return CompErr::new_general(
            //         format!(
            //             "Could not apply operation {} to types {:?} and {:?}",
            //             op.stringify(),
            //             lhs_val.type_,
            //             rhs_val.type_,
            //         ),
            //         ast.range,
            //     );
            // };

            insts.push(Inst {
                assigns: assigns.clone(),
                inner: InstInner::Ret(rhs_value),
            });
        }
        _ => panic!("{:?}", ast),
    }

    Ok(IrSnippet { insts, assigns })
}

fn assign(
    ctx: &mut GenContext,
    insts: &mut Vec<Inst>,
    lhs: Box<Node>,
    rhs_val: Value,
    range: Range,
) -> Result<(), CompErr> {
    let lhs = generate(*lhs, ctx, true)?;
    let Some(lhs_val) = lhs.assigns else {
        return CompErr::new_general(
            "Left hand side of assignment was not a valid expression",
            range,
        );
    };
    insts.extend(lhs.insts);
    let Type::Ptr(ref lhs_inner) = lhs_val.type_ else {
        return CompErr::new_general("Can not assign to lhs", range);
    };
    let _ = bin_op_coerce(&lhs_inner, &rhs_val.type_, ctx, range)?;
    insts.push(Inst {
        assigns: None,
        inner: InstInner::Store(lhs_val, rhs_val),
    });
    Ok(())
}

fn perform_bin_op(
    ctx: &mut GenContext,
    insts: &mut Vec<Inst>,
    op: BinaryOperation,
    lhs: &Node,
    rhs: &Node,
    range: &Range,
) -> Result<Value, CompErr> {
    let lhs = generate(lhs.clone(), ctx, false)?;
    let rhs = generate(rhs.clone(), ctx, false)?;
    let Some(lhs_val) = lhs.assigns else {
        return CompErr::new_general(
            format!(
                "Left hand side of {} was not a valid expression",
                op.stringify()
            ),
            range.clone(),
        );
    };
    let Some(rhs_val) = rhs.assigns else {
        return CompErr::new_general(
            format!(
                "Right hand side of {} was not a valid expression",
                op.stringify()
            ),
            range.clone(),
        );
    };
    insts.extend(lhs.insts);
    insts.extend(rhs.insts);
    let mut t = bin_op_coerce(&lhs_val.type_, &rhs_val.type_, ctx, range.clone())?;
    if op.logical_output() {
        t = Type::Boolean;
    }
    let assigns = new_vreg(t);
    insts.push(Inst {
        assigns: Some(assigns.clone()),
        inner: InstInner::BinOp {
            op,
            lhs: lhs_val,
            rhs: rhs_val,
        },
    });
    Ok(assigns)
}

fn decleration(
    insts: &mut Vec<Inst>,
    ctx: &mut GenContext,
    range: crate::error::Range,
    ident: &str,
    type_: Option<Type>,
    rhs: Box<Node>,
) -> Result<(), CompErr> {
    if let NodeInner::Type(t) = rhs.inner {
        ctx.define_type(&ident, t);
    } else {
        let rhs = generate(*rhs, ctx, false)?;
        let Some(rhs_val) = rhs.assigns else {
            return CompErr::new_general(
                format!("Right hand side of assignment was not a valid expression",),
                range,
            );
        };
        insts.extend(rhs.insts);

        let type_ = type_.unwrap_or_else(|| rhs_val.type_.clone());

        if !matches!(type_, Type::Auto(_)) {
            if let Type::Auto(ids) = &rhs_val.type_ {
                for id in ids {
                    assert!(!matches!(type_, Type::Auto(_)));
                    ctx.auto_map.insert(*id, type_.clone());
                }
            }
        }

        let vreg = ctx.declare(&ident, Type::Ptr(Box::new(type_)));

        insts.push(Inst {
            assigns: Some(vreg.as_value()),
            inner: InstInner::Alloca,
        });

        insts.push(Inst {
            assigns: None,
            inner: InstInner::Store(vreg.as_value(), rhs_val),
        });
    }

    Ok(())
}
